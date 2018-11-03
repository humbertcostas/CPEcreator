# Unsupervised learning

#' makeTrainingTestSet
#'
#' @return
#' @export
#'
#' @examples
makeTrainingTestSet <- function() {
  cpe <- getSampleSet(sample.size = 100000)

  # Training sets
  cpe.a <- dplyr::filter(cpe, part == "a")[1:1000,]
  cpe.o <- dplyr::filter(cpe, part == "o")[1:1000,]
  cpe.h <- dplyr::filter(cpe, part == "h")[1:1000,]

  for (i in 1:nrow(cpe.a)) {
    fileConn <- file(paste("inst/extdata/training/", "a_1.0_", cpe.a$file[i], sep = ""))
    writeLines(text = cpe.a$title[i], con = fileConn)
    close(fileConn)
  }
  for (i in 1:nrow(cpe.o)) {
    fileConn <- file(paste("inst/extdata/training/", "o_1.0_", cpe.o$file[i], sep = ""))
    writeLines(text = cpe.o$title[i], con = fileConn)
    close(fileConn)
  }
  for (i in 1:nrow(cpe.h)) {
    fileConn <- file(paste("inst/extdata/training/", "h_1.0_", cpe.h$file[i], sep = ""))
    writeLines(text = cpe.h$title[i], con = fileConn)
    close(fileConn)
  }

  # Test sets
  cpe.test <- cpe[99001:99200,]
  for (i in 1:nrow(cpe.test)) {
    fileConn <- file(paste("inst/extdata/test/", cpe.test$file[i], sep = ""))
    writeLines(text = cpe.test$title[i], con = fileConn)
    close(fileConn)
  }

}

#' trainCPEpart
#'
#' @return
#' @export
#'
#' @examples
trainCPEpart <- function() {
  # Step 1. Ingest your training data and clean it.
  train <- tm::VCorpus(tm::DirSource("inst/extdata/training", pattern = "a*", encoding = "UTF-8"))
  train <- tm::tm_map(train, tm::content_transformer(tm::stripWhitespace))
  train <- tm::tm_map(train, tm::content_transformer(tolower))
  train <- tm::tm_map(train, tm::content_transformer(tm::removeNumbers))
  train <- tm::tm_map(train, tm::content_transformer(tm::removePunctuation))

  # Step 2. Create your document term matrices for the training data.
  train.dtm <- as.matrix(tm::DocumentTermMatrix(train,
                                                control = list(wordLengths = c(1,Inf))),
                         stringsAsFactors = FALSE)

  # Step 3. Repeat steps 1 & 2 above for the Test set.
  test <- tm::VCorpus(tm::DirSource("inst/extdata/test", pattern = "*", encoding = "UTF-8"))
  test <- tm::tm_map(test, tm::content_transformer(tm::stripWhitespace))
  test <- tm::tm_map(test, tm::content_transformer(tolower))
  test <- tm::tm_map(test, tm::content_transformer(tm::removeNumbers))
  test <- tm::tm_map(test, tm::content_transformer(tm::removePunctuation))
  test.dtm <- as.matrix(tm::DocumentTermMatrix(test,
                                               control = list(wordLengths = c(1,Inf))),
                        stringsAsFactors = FALSE)

  # Step 4. Make test and train matrices of identical length (find intersection)
  train.df <- data.frame(train.dtm[,intersect(colnames(train.dtm), colnames(test.dtm))])
  test.df <- data.frame(test.dtm[,intersect(colnames(test.dtm), colnames(train.dtm))])

  # Step 5. Retrieve the correct labels for training data and put dummy values for testing data
  label.df <- data.frame(row.names(train.df))
  colnames(label.df) <- c("filenames")
  label.df <- splitstackshape::cSplit(label.df, 'filenames', sep = "_", type.convert = FALSE)
  train.df$corpus <- label.df$filenames_1
  test.df$corpus <- c("o", "o", "o", "h")
  test.df$corpus <- as.factor(test.df$corpus)
  levels(test.df$corpus) <- c("h", "o", "a")

  # Step 6. Create folds of your data, then run the training once to inspect results
  df.train <- train.df
  df.test <- train.df
  df.test$corpus <- as.factor(df.test$corpus)
  df.model <- kernlab::ksvm(corpus~., data = df.train, kernel = "rbfdot")
  df.pred <- kernlab::predict(df.model, df.test)
  con.matrix <- caret::confusionMatrix(df.pred, df.test$corpus)
  print(con.matrix)

  # Step 7. Run the final prediction on the test data and re-attach file names.
  df.test <- test.df
  df.pred <- kernlab::predict(df.model, df.test)
  results <- as.data.frame(df.pred)
  rownames(results) <- rownames(test.df)
  print(results)

  # Step 8. Validate the prediction
  results$file <- rownames(results)
  df.validation <- dplyr::left_join(results, cpe.test[, c("file", "part")], by = "file")
  df.validation$ok <- as.character(df.validation$df.pred) == df.validation$part
  print(sum(df.validation$ok) / 200)

  return(df.model)
}

#' getSampleSet
#'
#' @param seed
#' @param sample.size
#'
#' @return
#' @export
#'
#' @examples
getSampleSet <- function(seed = 666, sample.size = 1000) {
  netsec.file <- tempfile()
  download.file(url = "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda",
                destfile = netsec.file,)
  load(netsec.file)
  cpe <- netsec.data$datasets$cpes
  rm(netsec.data)
  cpe <- dplyr::select(cpe, title, part, vendor, product, version)

  set.seed(seed)
  sample.cpe <- sample(x = 1:nrow(cpe), size = sample.size)
  cpe <- cpe[sample.cpe, ]

  cpe$md5 <- sapply(cpe$title, function(x) digest::digest(object = x , algo = "md5"))
  cpe$file <- sapply(cpe$title,
                     function(x) paste(uuid::UUIDgenerate(),
                                       ".txt", sep = ""))

  return(cpe)
}



