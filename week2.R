#Week2
library(tm)
library(SnowballC)
library(RWeka)
library(ggplot2)

docs <- c("final/en_US/en_US.twitter.txt", 
          "final/en_US/en_US.blogs.txt",
          "final/en_US/en_US.news.txt")

dataDocuments <- character()

for(doc in docs) {
    data <- readLines(doc)
    sampleSize <- length(data) * .01
    data <- sample(data, sampleSize)
    dataDocuments <- c(dataDocuments, data)
}

dataVS <- VectorSource(dataDocuments)
dataCorpus <- VCorpus(dataVS)
dataCorpus <- tm_map(dataCorpus, removePunctuation)
dataCorpus <- tm_map(dataCorpus, removeNumbers)
dataCorpus <- tm_map(dataCorpus, stripWhitespace)
dataCorpus <- tm_map(dataCorpus, content_transformer(tolower))
dataCorpus <- tm_map(dataCorpus, stemDocument)

dataFreqTerm <- data.frame(Term = character(), Freq = numeric())
for(i in 1:length(dataDocuments)) {
    doc <- dataCorpus[[i]]
    freqTerms <- termFreq(doc)
    for(term in rownames(freqTerms)) {
        idx <- dataFreqTerm$Term == term
        if(sum(idx) == 0) {
            dataFreqTerm <- rbind(dataFreqTerm, data.frame(Term = term, Freq = freqTerms[[term]]))
        } else {
            dataFreqTerm[idx,]$Freq <- dataFreqTerm[idx,]$Freq + freqTerms[term]   
        }
    }
}
dataFreqTerm <- dataFreqTerm[order(-dataFreqTerm$Freq),]
dataFreqTermSorted <- dataFreqTerm[1:50,]

# 1. Some words are more frequent than others - what are the distributions of word frequencies?
ggplot(data = dataFreqTermSorted, aes(x = reorder(Term, Freq), y = Freq)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    ylab("Frequency") + 
    xlab("Words")
ggsave("word-distribution.png", height = 10, width = 10)


findIdxByPercentageFreq <- function(df, perc) {
    totalFreq <- sum(df) * perc
    lastIdx <- 0
    totalSum <- 0
    for(i in 1:length(df)) {
        if(totalFreq >= totalSum) {
            totalSum <- totalSum + df[i]
        } else {
            lastIdx <- i
            break;
        }
    }
    lastIdx
}

findIdxByPercentageFreq(dataFreqTerm$Freq, 0.9)
findIdxByPercentageFreq(dataFreqTerm$Freq, 0.5)