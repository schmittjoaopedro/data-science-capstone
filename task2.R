library(tm)
library(SnowballC)
library(RWeka)
library(wordcloud)
library(RColorBrewer)
library(cldr)

# ============================================
# Generate plot for the frequencies of words
# ============================================
# Loaded the tidy data reduced (not stemmed)
dataDocuments <- readLines("tidy/tidy_reduced.txt")
# Prepare the corpus
dataVS <- VectorSource(dataDocuments)
dataCorpus <- VCorpus(dataVS)
# Configure the cores
options(mc.cores=1)
# Generate a mono-gram of frequencies
monoGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdmMono <- TermDocumentMatrix(dataCorpus, control = list(tokenize = monoGramTokenizer))
freqMono <- slam::row_sums(tdmMono)
freqMono <- freqMono[order(-freqMono)]
freqMono <- data.frame(word = names(freqMono), freq = freqMono)
write.csv(x = freqMono, file = "tidy/freqMono.csv", row.names = FALSE)

# Generate a bi-gram of frequencies
biGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdmBi <- TermDocumentMatrix(dataCorpus, control = list(tokenize = biGramTokenizer))
freqBi <- slam::row_sums(tdmBi)
freqBi <- freqBi[order(-freqBi)]
freqBi <- data.frame(word = names(freqBi), freq = freqBi)
write.csv(x = freqBi, file = "tidy/freqBi.csv", row.names = FALSE)

# Generate a three-gram of frequencies
threeGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdmThree <- TermDocumentMatrix(dataCorpus, control = list(tokenize = threeGramTokenizer))
freqThree <- slam::row_sums(tdmThree)
freqThree <- freqThree[order(-freqThree)]
freqThree <- data.frame(word = names(freqThree), freq = freqThree)
write.csv(x = freqThree, file = "tidy/freqThree.csv", row.names = FALSE)

# Plot a wordcloud of frequencies of monograms
png("wordcloud.png", width=640,height=480)
wordcloud(words = freqMono$word, freq = freqMono$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()
# Bar plot mono gram
png("mono-gram.png", width=1024,height=768)
par(mar=c(4,4,4,4))
barplot(freqMono[1:50,]$freq, las = 2, names.arg = freqMono[1:50,]$word,
        col ="lightblue", main ="Most frequent mono-gram words",
        ylab = "Word frequencies")
dev.off()
# Bar plot bi gram
png("bi-gram.png", width=1024,height=768)
par(mar=c(8,4,4,4))
barplot(freqBi[1:50,]$freq, las = 2, names.arg = freqBi[1:50,]$word,
        col ="lightblue", main ="Most frequent bi-gram words",
        ylab = "Word frequencies")
dev.off()
# Bar plot three gram
png("three-gram.png", width=1024,height=768)
par(mar=c(14,4,4,4))
barplot(freqThree[1:50,]$freq, las = 2, names.arg = freqThree[1:50,]$word,
        col ="lightblue", main ="Most frequent three-gram words",
        ylab = "Word frequencies")
dev.off()



# ============================================
# Calculate frequencies for tidy
# ============================================
# Read the tidy data
dataDocuments <- readLines("tidy/tidy.txt")
# Prepare the corpus
dataVS <- VectorSource(dataDocuments)
dataCorpus <- VCorpus(dataVS)
# Configure the cores
options(mc.cores=1)
# Generate a monogram
monoGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdmMono <- TermDocumentMatrix(dataCorpus, control = list(tokenize = monoGramTokenizer))
freqMono <- slam::row_sums(tdmMono)
freqMono <- freqMono[order(-freqMono)]
freqMono <- data.frame(word = names(freqMono), freq = freqMono)
# Calculate the words quantity for 50%
totalFreq <- sum(freqMono$freq) * .5
currentFreq <- 0
selectedIdx <- 0
for(i in 1:length(freqMono$freq)) {
    currentFreq <- currentFreq + freqMono$freq[i]
    if(currentFreq >= totalFreq) {
        selectedIdx <- i - 1
        break
    }
}
(selectedIdx/length(freqMono$freq)) * 100
# Calculate the words quantity for 90%
totalFreq <- sum(freqMono$freq) * .9
currentFreq <- 0
selectedIdx <- 0
for(i in 1:length(freqMono$freq)) {
    currentFreq <- currentFreq + freqMono$freq[i]
    if(currentFreq >= totalFreq) {
        selectedIdx <- i - 1
        break
    }
}
(selectedIdx/length(freqMono$freq)) * 100


# ============================================
# Evaluating foreign words count
# ============================================
dataDocuments <- readLines("tidy/tidy.txt")
foreignWords <- 0
for(i in 1:length(dataDocuments)) {
    for(j in strsplit(dataDocuments[i], " ")) {
        if(detectLanguage(j)$detectedLanguage[1] != "ENGLISH") {
            foreignWords <- foreignWords + 1    
        }
    }
}
foreignWords