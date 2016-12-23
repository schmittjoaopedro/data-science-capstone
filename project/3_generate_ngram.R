library(tm)
library(SnowballC)
library(RWeka)
library(dplyr)

dir_data_clean <- "data/clean"
data_clean_file <- "data_clean"
data_ngram_file <- "data_ngram"

# Load clean data
data.clean <- readRDS(sprintf("%s/%s.rds", dir_data_clean, data_clean_file))
dataVS <- VectorSource(data.clean)
dataCorpus <- VCorpus(dataVS)

generateNGram <- function(corpus, level = 1) {
    options(mc.cores=1)
    tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = level, max = level))
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizer))
    freq <- slam::row_sums(tdm)
    freq <- freq[order(-freq)]
    freq <- data.frame(word = names(freq), freq = freq)
}

tetraGram <- generateNGram(dataCorpus, 4)
# Split NGram in frequencies table
tetraGramSplit <- within(tetraGram, word <- data.frame(do.call('rbind', strsplit(as.character(word), " ", fixed = T))))
rownames(tetraGramSplit) <- 1:nrow(tetraGramSplit)
tetraGramSplit$word1 <- tetraGramSplit$word$X1
tetraGramSplit$word2 <- tetraGramSplit$word$X2
tetraGramSplit$word3 <- tetraGramSplit$word$X3
tetraGramSplit$word4 <- tetraGramSplit$word$X4
tetraGramSplit <- tetraGramSplit %>% select(word1, word2, word3, word4, freq)

saveRDS(object = tetraGramSplit, file = sprintf("%s/%s.rds", dir_data_clean, data_ngram_file))
