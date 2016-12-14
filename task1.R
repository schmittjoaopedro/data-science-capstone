library(tm)
library(SnowballC)
library(cldr)

# ============================================
# Merge, sampling and save english documents
# ============================================
docs <- c("final/en_US/en_US.twitter.txt", 
          "final/en_US/en_US.blogs.txt",
          "final/en_US/en_US.news.txt")
dataDocuments <- character()
# Sampling the data
for(doc in docs) {
    data <- readLines(doc)
    sampleSize <- length(data) * .01
    data <- data[detectLanguage(data)$detectedLanguage == "ENGLISH"]
    data <- sample(data, sampleSize)
    dataDocuments <- c(dataDocuments, data)
}
# Saving the data
write.table(x = dataDocuments, 
          file = "tidy/sample.txt", 
          quote = FALSE,
          row.names = FALSE,
          col.names = FALSE)

# ============================================
# Remove pontuaction, numbers and transform to lower
# ============================================
# Read the sample data
dataDocuments <- readLines("tidy/sample.txt")
# Cleaning the data
dataVS <- VectorSource(dataDocuments)
dataCorpus <- VCorpus(dataVS)
dataCorpus <- tm_map(dataCorpus, removePunctuation)
dataCorpus <- tm_map(dataCorpus, removeNumbers)
dataCorpus <- tm_map(dataCorpus, content_transformer(tolower))
dataCorpus <- tm_map(dataCorpus, stripWhitespace)
# Saving the data
conn <- file("tidy/tidy.txt", "w")
for(i in 1:length(dataDocuments)) {
    writeLines(text = dataCorpus[[i]]$content, con = conn)
}
close(conn)

# ============================================
# Remove stop words
# ============================================
dataCorpus <- tm_map(dataCorpus, removeWords, stopwords("english"))
dataCorpus <- tm_map(dataCorpus, stripWhitespace)
# Saving the data
conn <- file("tidy/tidy_reduced.txt", "w")
for(i in 1:length(dataDocuments)) {
    writeLines(text = dataCorpus[[i]]$content, con = conn)
}
close(conn)

# ============================================
# Stem the words
# ============================================
dataCorpus <- tm_map(dataCorpus, stemDocument)
dataCorpus <- tm_map(dataCorpus, stripWhitespace)
# Saving the data
conn <- file("tidy/tidy_stemmed.txt", "w")
for(i in 1:length(dataDocuments)) {
    writeLines(text = dataCorpus[[i]]$content, con = conn)
}
close(conn)
