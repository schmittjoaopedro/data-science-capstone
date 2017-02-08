library(tm)
library(SnowballC)
library(cldr)

data_clean_dir <- "data/clean"
dir_data_raw_final <- "data/raw/final"

source("5_predicting.R")

locale_file <- "en_US"
test.file <- c()
for(source in c("blogs", "news", "twitter")) {
    
    file <- readLines(sprintf("%s/%s/%s.%s.txt", dir_data_raw_final, locale_file, locale_file, source), warn = F)
    file_lines <- length(file)
    file_sample <- ceiling(file_lines * 0.0001)
    test.file <- file[sample(1:file_lines, file_sample, replace = F)]
    rm(file)
    # Remove phrases that not are in english
    test.file <- test.file[detectLanguage(test.file)$detectedLanguage == "ENGLISH"]
    # Create a corpus
    dataVS <- VectorSource(test.file)
    testCorpus <- VCorpus(dataVS)
    # Transform to lower
    testCorpus <- tm_map(testCorpus, content_transformer(tolower))
    # Remove ponctuation
    testCorpus <- tm_map(testCorpus, removePunctuation)
    # Remove numbers
    testCorpus <- tm_map(testCorpus, removeNumbers)
    # Remove extra spaces
    testCorpus <- tm_map(testCorpus, stripWhitespace)
    
    test.clean <- c()
    for(i in 1:length(test.file)) {
        test.clean <- c(test.clean, testCorpus[[i]]$content)
    }
    
    totalWords <- 0
    rightWords <- 0
    for(i in 1:length(test.clean)) {
        sentence <- unlist(strsplit(test.clean[i]," "))
        n <- length(sentence)
        if(n > 3) {
            for(i in 1:(n - 3)) {
                wordsPredicted <- predictor.predictWord(sprintf("%s %s %s", sentence[i], sentence[i + 1], sentence[i + 2]))
                totalWords <- totalWords + 1
                if(sentence[i + 3] %in% head(wordsPredicted$word4)) {
                    rightWords <- rightWords + 1
                }
            }
        }
    }
    
    print(sprintf("Predicted for %s in %s documents with %s of accuracy.", 
                  source, 
                  file_sample,
                  round((rightWords / totalWords) * 100, 2)))
}

#[1] Predicted for blogs in 90 documents with 21.68 of accuracy.
#[1] Predicted for news in 102 documents with 21.63 of accuracy.
#[1] Predicted for twitter in 237 documents with 21.47 of accuracy.