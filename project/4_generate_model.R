library(dplyr)

dir_data_clean <- "data/clean"
data_ngram_file <- "data_ngram"

data.ngram <- readRDS(sprintf("%s/%s.rds", dir_data_clean, data_ngram_file))

model <- list()

model$w1w2w3 <- data.ngram %>% 
    group_by(word1, word2, word3) %>%
    mutate(freqTotal = sum(freq)) %>%
    group_by(word4, add = TRUE) %>%
    mutate(prob = freq / freqTotal) %>%
    arrange(word1, word2, word3, word4, desc(prob)) %>%
    as.data.frame()

model$w2w3 <- data.ngram %>% 
    select(word2, word3, word4, freq) %>%
    group_by(word2, word3, word4) %>%
    summarise_each(funs(sum(freq))) %>%
    group_by(word2, word3) %>%
    mutate(freqTotal = sum(freq)) %>%
    group_by(word4, add = TRUE) %>%
    mutate(prob = freq / freqTotal) %>%
    arrange(word2, word3, word4, desc(prob)) %>%
    as.data.frame()

model$w3 <- data.ngram %>% 
    select(word3, word4, freq) %>%
    group_by(word3, word4) %>%
    summarise_each(funs(sum(freq))) %>%
    group_by(word3) %>%
    mutate(freqTotal = sum(freq)) %>%
    group_by(word4, add = TRUE) %>%
    mutate(prob = freq / freqTotal) %>%
    arrange(word3, word4, desc(prob)) %>%
    as.data.frame()

model$w1w3 <- data.ngram %>% 
    select(word1, word3, word4, freq) %>%
    group_by(word1, word3, word4) %>%
    summarise_each(funs(sum(freq))) %>%
    group_by(word1, word3) %>%
    mutate(freqTotal = sum(freq)) %>%
    group_by(word4, add = TRUE) %>%
    mutate(prob = freq / freqTotal) %>%
    arrange(word1, word3, word4, desc(prob)) %>%
    as.data.frame()

model$w1w2 <- data.ngram %>% 
    select(word1, word2, word4, freq) %>%
    group_by(word1, word2, word4) %>%
    summarise_each(funs(sum(freq))) %>%
    group_by(word1, word2) %>%
    mutate(freqTotal = sum(freq)) %>%
    group_by(word4, add = TRUE) %>%
    mutate(prob = freq / freqTotal) %>%
    arrange(word1, word2, word4, desc(prob)) %>%
    as.data.frame()

model$w1 <- data.ngram %>% 
    select(word1, word4, freq) %>%
    group_by(word1, word4) %>%
    summarise_each(funs(sum(freq))) %>%
    group_by(word1) %>%
    mutate(freqTotal = sum(freq)) %>%
    group_by(word4, add = TRUE) %>%
    mutate(prob = freq / freqTotal) %>%
    arrange(word1, word4, desc(prob)) %>%
    as.data.frame()

model$w2 <- data.ngram %>% 
    select(word2, word4, freq) %>%
    group_by(word2, word4) %>%
    summarise_each(funs(sum(freq))) %>%
    group_by(word2) %>%
    mutate(freqTotal = sum(freq)) %>%
    group_by(word4, add = TRUE) %>%
    mutate(prob = freq / freqTotal) %>%
    arrange(word2, word4, desc(prob)) %>%
    as.data.frame()

model$w4 <- data.ngram %>% 
    select(word4, freq) %>%
    group_by(word4) %>%
    summarise(freq = n()) %>%
    mutate(prob = freq / sum(freq)) %>%
    arrange(word4, desc(prob)) %>%
    as.data.frame()