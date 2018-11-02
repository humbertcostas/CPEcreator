# Unsupervised learning

buildTraingTests <- function() {
  netsec.file <- tempfile()
  download.file(url = "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda",
                destfile = netsec.file,)
  load(netsec.file)
  cpe <- netsec.data$datasets$cpes
  rm(netsec.data)
  cpe <- dplyr::select(cpe, title, part, vendor, product, version)
  cpe$md5 <- sapply(cpe$title, function(x) digest::digest(object = x , algo = "md5"))
  cpe$file <- sapply(cpe$title,
                     function(x) paste(uuid::UUIDgenerate(),
                                       ".txt", sep = ""))

  cpe.a <- dplyr::filter(cpe, part == "a")
  cpe.o <- dplyr::filter(cpe, part == "o")
  cpe.h <- dplyr::filter(cpe, part == "h")

  # Write training & tests set for Applications
  sample.size <- 600
  tests.size <- 100
  sample.a <- sample(x = 1:nrow(cpe.a), size = sample.size)
  tr.cpe.a <- cpe.a[sample.a[1:(sample.size - tests.size)], ]
  ts.cpe.a <- cpe.a[sample.a[(sample.size - tests.size + 1):sample.size], ]
  for (i in 1:nrow(tr.cpe.a)) {
    fileConn <- file(paste("inst/extdata/training/", "a_1.0_", tr.cpe.a$file[i], sep = ""))
    writeLines(text = tr.cpe.a$title[i], con = fileConn)
    close(fileConn)
  }
  for (i in 1:nrow(ts.cpe.a)) {
    fileConn <- file(paste("inst/extdata/test/", ts.cpe.a$file[i], sep = ""))
    writeLines(text = ts.cpe.a$title[i], con = fileConn)
    close(fileConn)
  }

  # Write training set for Operating Systems
  sample.o <- sample(x = 1:nrow(cpe.o), size = sample.size)
  tr.cpe.o <- cpe.o[sample.o[1:(sample.size - tests.size)], ]
  ts.cpe.o <- cpe.o[sample.o[(sample.size - tests.size + 1):sample.size], ]
  for (i in 1:nrow(tr.cpe.o)) {
    fileConn <- file(paste("inst/extdata/training/", "o_1.0_", tr.cpe.o$file[i], sep = ""))
    writeLines(text = tr.cpe.o$title[i], con = fileConn)
    close(fileConn)
  }
  for (i in 1:nrow(ts.cpe.o)) {
    fileConn <- file(paste("inst/extdata/test/", ts.cpe.o$file[i], sep = ""))
    writeLines(text = ts.cpe.o$title[i], con = fileConn)
    close(fileConn)
  }

  # Write training set for Hardware
  sample.h <- sample(x = 1:nrow(cpe.h), size = sample.size)
  tr.cpe.h <- cpe.h[sample.h[1:(sample.size - tests.size)], ]
  ts.cpe.h <- cpe.h[sample.h[(sample.size - tests.size + 1):sample.size], ]
  for (i in 1:nrow(tr.cpe.h)) {
    fileConn <- file(paste("inst/extdata/training/", "h_1.0_", tr.cpe.h$file[i], sep = ""))
    writeLines(text = tr.cpe.h$title[i], con = fileConn)
    close(fileConn)
  }
  for (i in 1:nrow(ts.cpe.h)) {
    fileConn <- file(paste("inst/extdata/test/", ts.cpe.h$file[i], sep = ""))
    writeLines(text = ts.cpe.h$title[i], con = fileConn)
    close(fileConn)
  }

  return(levels(as.factor(cpe$part)))
}

trainCPEpart <- function(part.levels = c("a", "h", "o")) {
  # Step 1. Ingest your training data and clean it.
  train <- tm::VCorpus(tm::DirSource("inst/extdata/training/a*", encoding = "UTF-8"))
  train <- tm::tm_map(train, tm::content_transformer(tm::stripWhitespace))
  train <- tm::tm_map(train, tm::content_transformer(tolower))
  train <- tm::tm_map(train, tm::content_transformer(tm::removeNumbers))
  train <- tm::tm_map(train, tm::content_transformer(tm::removePunctuation))

  # Step 2. Create your document term matrices for the training data.
  train.dtm <- as.matrix(tm::DocumentTermMatrix(train,
                                                control = list(wordLengths = c(1,Inf))))
  #train.matrix <- as.matrix(train.dtm, stringsAsFactors=F)

  # Step 3. Repeat steps 1 & 2 above for the Test set.
  test <- tm::VCorpus(tm::DirSource("inst/extdata/test/", encoding = "UTF-8"))
  test <- tm::tm_map(test, tm::content_transformer(tm::stripWhitespace))
  test <- tm::tm_map(test, tm::content_transformer(tolower))
  test <- tm::tm_map(test, tm::content_transformer(tm::removeNumbers))
  test <- tm::tm_map(test, tm::content_transformer(tm::removePunctuation))
  test.dtm <- as.matrix(tm::DocumentTermMatrix(test, control = list(wordLengths = c(1,Inf))))

  # Step 4. Make test and train matrices of identical length (find intersection)
  train.df <- data.frame(train.dtm[,intersect(colnames(train.dtm), colnames(test.dtm))])
  test.df <- data.frame(test.dtm[,intersect(colnames(test.dtm), colnames(train.dtm))])

  # Step 5. Retrieve the correct labels for training data and put dummy values for testing data
  label.df <- data.frame(row.names(train.df))
  colnames(label.df) <- c("filenames")
  label.df <- splitstackshape::cSplit(label.df, 'filenames', sep = "_", type.convert = FALSE)
  train.df$corpus <- label.df$filenames_1
  test.df$corpus <- c("a")

  # Step 6. Create folds of your data, then run the training once to inspect results
  df.train <- train.df
  df.test <- train.df
  df.model <- kernlab::ksvm(corpus~., data = df.train, kernel = "rbfdot")
  df.pred <- kernlab::predict(df.model, df.test)
  con.matrix <- caret::confusionMatrix(df.pred, df.test$corpus)
  print(con.matrix)
}


