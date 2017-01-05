library(shiny)
library(plotly)

shinyUI(fluidPage(
    titlePanel("Next word prediction."),
    sidebarLayout(
        sidebarPanel(
            textInput("sentenceInput", "Enter with a sentence: ")
        ),
        mainPanel(
            plotlyOutput("sentenceOutput")
        )
    )
))
