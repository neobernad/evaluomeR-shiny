library(shiny)
library(evaluomeR)

source("modules/stability.R")

server <- function(input, output, session) {
    rv <- reactiveValues(
        inputData = NULL
    )
    
    observeEvent(input$inputSample, {
        rv$inputData=input$inputSample
    })
    
    observeEvent(input$inputCSV, {
        rv$inputData=input$inputCSV$datapath
    })
    
    output$tableContent <- renderTable({
        tryCatch(
            {
                df <- read.csv(rv$inputData,
                               header = TRUE,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        return(head(df))
    })
    
    # Logic for tab uiOutput("tabTable") ----
    output$tabTable <- renderUI({
        
        if (is.null(rv$inputData) || rv$inputData == "") {
            return(tags$h3("No input data selected"))
        }
        
        tagList(
            tags$h3("Input data table head"),
            tags$hr(),
            div(style = 'overflow-x: auto', tableOutput("tableContent"))
        )
    })
    
    # Logic for tab uiOutput("tabTable") ----
    output$tabStability <- callModule(stability, "tabStability", reactive(rv))
    
}