library(shiny)
library(dplyr)
library(plotly)

source("predictor.R")

shinyServer(function(input, output) {
    
    output$sentenceOutput <- renderPlotly({
        words <- predictor.predictWord(input$sentenceInput)
        if(!is.null(words) && nrow(words) > 0) {
            words <- head(words, 15)
            words <- arrange(words, probAdj)
            words$word4 <- factor(words$word4, levels = as.character(words$word4))
            words$probAdj <- ((words$probAdj / sum(words$probAdj)) * 100)
            return(
                plot_ly(words, 
                        y = ~word4, 
                        x = ~probAdj, 
                        type = "scatter", 
                        mode = "markers",
                        name = "Next word probability") %>%
                    layout(yaxis = list(title = ""), 
                           xaxis = list(title = "Probability (%)", range = c(0, 110)), 
                           margin = list(l = max(sapply(as.character(words$word4), nchar)) * 10))   
            )
        } else {
            return(NULL)
        }
    })
  
})
