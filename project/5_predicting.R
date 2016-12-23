library(stringi)
library(R.utils)
library(dplyr)

#prediction using interpolation, model extended with partial matches for 4-grams
predict.interp <- function(model, t1, t2, t3) {
  
  if(is.null(model$coef)) {
    #coefficients for interpolation, default values, optimized for max likelihood
    a123 = 0.33 * 0.35; a12 = 0.33 * 0.22;
    a13 = 0.33 * 0.33; a1 = 0.33 * 0.1;
    a23 = 0.33 * 1; a2 = 0.33 * 0; a3 = 0.32; a4 = 0.02
  } else {
    a123 <- model$coef$a123
    a12 <- model$coef$a12
    a13 <- model$coef$a13
    a1 <- model$coef$a1
    a2 <- model$coef$a2
    a23 <- model$coef$a23
    a3 <- model$coef$a3
  }
  
  #search 4gram, full match
  q123 <- model$w1w2w3 %>%
          filter(word1 == t1, word2 == t2, word3 == t3) %>%
          group_by(word4) %>%
          mutate(probAdjusted = a123 * prob) %>%
          as.data.frame()
  
  q12 <- model$w1w2 %>%
    filter(word1 == t1, word2 == t2) %>%
    group_by(word4) %>%
    mutate(probAdjusted = a12 * prob) %>%
    as.data.frame()
  
  q13 <- model$w1w3 %>%
    filter(word1 == t1, word3 == t3) %>%
    group_by(word4) %>%
    mutate(probAdjusted = a13 * prob) %>%
    as.data.frame()
  
  q1 <- model$w1 %>%
    filter(word1 == t1) %>%
    group_by(word4) %>%
    mutate(probAdjusted = a1 * prob) %>%
    as.data.frame()
  
  q23 <- model$w2w3 %>%
    filter(word2 == t2, word3 == t3) %>%
    group_by(word4) %>%
    mutate(probAdjusted = a23 * prob) %>%
    as.data.frame()
  
  q3 <- model$w3 %>%
    filter(word3 == t3) %>%
    group_by(word4) %>%
    mutate(probAdjusted = a3 * prob) %>%
    as.data.frame()
  
  q <- rbind(
          q123[, c("word4","probAdjusted")], 
          q12[, c("word4","probAdjusted")], 
          q13[, c("word4","probAdjusted")], 
          q1[, c("word4","probAdjusted")], 
          q23[, c("word4","probAdjusted")], 
          q3[, c("word4","probAdjusted")]) %>%
        select(word4, probAdjusted) %>%
        group_by(word4) %>%
        summarise_each(funs(sum)) %>%
        arrange(desc(probAdjusted)) %>%
        as.data.frame()
  
  print(sum(is.na(q$word4)))
  
  if(sum(is.na(q$word4)) == 0) {
    return (q[complete.cases(q),])
  }
  #last option, return the most frequent unigrams
  if (t3 == "<S>") {
    return (data.frame(word4 = "the", probAdjusted = 0))
  }
  return (data.frame(word4 = "and", probAdjusted = 0))
}


nextWord <- function(model, phrase, specificWordsToFind = c()) {
  words <- strsplit(phrase, " ")[[1]]
  t1 <- ""
  t2 <- ""
  t3 <- ""
  if(length(words) > 0) t1 <- words[1]
  if(length(words) > 1) t2 <- words[2]
  if(length(words) > 2) t3 <- words[3]
  predicted <- predict.interp(model, t1, t2, t3)
  if(length(specificWordsToFind) == 0) {
    return(predicted)
  } else {
    predicted %>% filter(word4 %in% specificWordsToFind) %>% arrange(desc(probAdjusted)) %>% as.data.frame()
  }
}
