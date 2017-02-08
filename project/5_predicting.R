library(dplyr)
library(tm)
library(SnowballC)
library(cldr)

data_clean_dir <- "data/clean"
data_sgt_file <- "sgt_model"
data_model_file <- "data_model"
data_predictor <- "predictor_api"

# Thanks!
#
# http://www.grsampson.net/RGoodTur.html
# http://www.grsampson.net/AGtf1.html
# http://www.grsampson.net/D_SGT.c 
# https://github.com/dxrodri/datasciencecoursera/blob/master/SwiftKeyCapstone/predictFunctions.r
calculateSimpleGoodTuring <- function(model){
    
    freqTable <- table(model$freq)
    
    SGT_DT <- data.frame(
        r=as.numeric(names(freqTable)),
        n=as.vector(freqTable),
        Z=vector("numeric",length(freqTable)), 
        logr=vector("numeric",length(freqTable)),
        logZ=vector("numeric",length(freqTable)),
        r_star=vector("numeric",length(freqTable)),
        p=vector("numeric",length(freqTable)))
    
    num_r <- nrow(SGT_DT)

    for (j in 1:num_r) {
        if(j == 1) {
            r_i <- 0
        } else {
            r_i <- SGT_DT$r[j-1]
        }
        if(j == num_r) {
            r_k <- SGT_DT$r[j]
        } else {
            r_k <- SGT_DT$r[j+1]
        }
        SGT_DT$Z[j] <- 2 * SGT_DT$n[j] / (r_k - r_i)
    }
    
    SGT_DT$logr <- log(SGT_DT$r)
    SGT_DT$logZ <- log(SGT_DT$Z)
    linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
    c0 <- linearFit$coefficients[1]
    c1 <- linearFit$coefficients[2]
    
    use_y = FALSE
    for (j in 1:(num_r-1)) {
        r_plus_1 <- SGT_DT$r[j] + 1
        
        s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
        s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
        y <- r_plus_1 * s_r_plus_1/s_r
        
        if(use_y) {
            SGT_DT$r_star[j] <- y
        } else { 
            n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
            if(length(n_r_plus_1) == 0 ) {
                SGT_DT$r_star[j] <- y
                use_y = TRUE
            } else {
                n_r <- SGT_DT$n[j]
                x<-(r_plus_1) * n_r_plus_1/n_r
                if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
                    SGT_DT$r_star[j] <- x
                } else {
                    SGT_DT$r_star[j] <- y
                    use_y = TRUE
                }
            }
        }
        if(j==(num_r-1)) {
            SGT_DT$r_star[j+1] <- y
        }
    }
    N <- sum(SGT_DT$n * SGT_DT$r)
    Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
    Po <- SGT_DT$n[1] / N
    SGT_DT$p <- (1-Po) * SGT_DT$r_star/Nhat
    
    return(SGT_DT)
}

predictNextWord <- function(testSentence, model, sgt, validResultsList=NULL) {
    
    options("scipen"=100, "digits"=8)
    
    testSentenceList <- unlist(strsplit(testSentence," "))
    noOfWords <- length(testSentenceList)
    
    resultDF <- data.frame(word4 = factor(), probAdj = numeric())
    
    predictNGram(resultDF, "w1w2w3", sgt$w1w2w3, validResultsList,
                 model$w1w2w3 %>% filter(word1 == testSentenceList[noOfWords-2], 
                                         word2 == testSentenceList[noOfWords-1], 
                                         word3 == testSentenceList[noOfWords]))
    
    predictNGram(resultDF, "w2w3", sgt$w2w3, validResultsList, 
                 model$w2w3 %>% filter(word2 == testSentenceList[noOfWords-1], 
                                       word3 == testSentenceList[noOfWords]))
    
    predictNGram(resultDF, "w3", sgt$w3, validResultsList, 
                 model$w3 %>% filter(word3 == testSentenceList[noOfWords]))
    
    
    predictNGram(resultDF, "w1w2", sgt$w1w2, validResultsList, 
                 model$w1w2 %>% filter(word1 == testSentenceList[noOfWords-2], 
                                       word2 == testSentenceList[noOfWords-1]))
    
    predictNGram(resultDF, "w1w3", sgt$w1w3, validResultsList, 
                 model$w1w3 %>% filter(word1 == testSentenceList[noOfWords-2], 
                                       word3 == testSentenceList[noOfWords]))
    
    predictNGram(resultDF, "w1", sgt$w1, validResultsList, 
                 model$w1 %>% filter(word1 == testSentenceList[noOfWords-2]))
    
    return(resultDF %>% arrange(desc(probAdj)))
  
}

predictNGram <- function(resultDF, labelName, sgt, validResultsList, subGram) {
    if(nrow(subGram) > 0 & !(nrow(resultDF) > 0)) {
        #print(labelName)
        subGram$probAdj <- sapply(subGram$freq, FUN = function(x) sgt$p[sgt$r == x])
        subGram <- subGram %>% select(word4, probAdj)
        if(!is.null(validResultsList) & nrow(subGram) > 0) {
            subGram <- subGram %>% filter(word4 %in% validResultsList)
        }
        eval.parent(substitute(resultDF <- subGram))
    }
}


cleanSentence <- function(testSentence) {
    testSentence <- stripWhitespace(testSentence)
    testSentence <- tolower(testSentence)
    testSentence <- removeNumbers(testSentence)
    testSentence <- removePunctuation(testSentence, preserve_intra_word_dashes = TRUE)
  return(testSentence)
}

predictWord <- function(sentence) {
    sentence <- cleanSentence(sentence)
    sentenceList <- unlist(strsplit(sentence," "))
    noOfWords <- length(sentenceList)
    if(noOfWords >= 3) {
        return(predictNextWord(paste(
            sentenceList[noOfWords-2], 
            sentenceList[noOfWords-1], 
            sentenceList[noOfWords]), predictor.model, predictor.sgt))
    } else if(noOfWords == 2) {
        return(predictNextWord(paste(
            "-", 
            sentenceList[noOfWords-1], 
            sentenceList[noOfWords]), predictor.model, predictor.sgt))
    } else if(noOfWords == 1) {
        return(predictNextWord(paste(
            "-", 
            "-", 
            sentenceList[noOfWords]), predictor.model, predictor.sgt))
    }
}

variables <- ls()
if(sum(variables == "model") == 0) {
    model <- readRDS(sprintf("%s/%s.rds", data_clean_dir, data_model_file))
    variables <- ls()
}

sgt <- list()
sgt$w1w2w3 <- calculateSimpleGoodTuring(model$w1w2w3)
sgt$w2w3 <- calculateSimpleGoodTuring(model$w2w3)
sgt$w3 <- calculateSimpleGoodTuring(model$w3)
sgt$w1w3 <- calculateSimpleGoodTuring(model$w1w3)
sgt$w1w2 <- calculateSimpleGoodTuring(model$w1w2)
sgt$w1 <- calculateSimpleGoodTuring(model$w1)
sgt$w2 <- calculateSimpleGoodTuring(model$w2)
sgt$w4 <- calculateSimpleGoodTuring(model$w4)

saveRDS(object = sgt, file = sprintf("%s/%s.rds", data_clean_dir, data_sgt_file))

predictor <- list()
predictor.model <- model
predictor.sgt <- sgt
predictor.predictWord <- predictWord