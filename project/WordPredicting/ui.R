library(shiny)
library(plotly)

shinyUI(fluidPage(
    tabsetPanel(
        tabPanel("Prediction", 
            h3("Next word prediction"), 
            sidebarLayout(
                sidebarPanel(
                    textInput("sentenceInput", "Enter with a sentence: ")
                ),
                mainPanel(
                    h3("Next word probabilities:"),
                    plotlyOutput("sentenceOutput")
                )
            )
        ),
        tabPanel("About", 
            h3("Use mode"),
            p("Enter with a sentence in the text area, and see the plot showing the words with the highest probability to be selected."),
            h3("Author"),
            p("João Pedro Schmitt"),
            h3("Project"),
            p("A better explanation can be found at this link: ", a("RPubs", href = "http://rpubs.com/schmittjoaopedro/247738", target="_blank")),
            p("A brief presentation can be found at: ", a("Slide deck", href = "http://rpubs.com/schmittjoaopedro/248102", target="_blank")),
            p("The full source code can be found at the following link: ", a("Github repository", href = "https://github.com/schmittjoaopedro/data-science-capstone/tree/master/project", target="_blank"))
        )
    ),
    tags$script('
        $(document).on("ready", function (e) {
            $("#sentenceInput").focus()
        });
    ') 
))
