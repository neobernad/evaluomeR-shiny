library(shiny)
library(shinyalert)
library(evaluomeR)
library(plotly)

source("modules/stability.R")
source("modules/quality.R")
source("modules/correlations.R")
source("modules/optimal_k.R")

loadDataset <- function(input, rv) {
    if (!is.null(rv$inputData) && rv$inputData != "") {
        cat(file=stderr(), "Loading input:", rv$inputData, "\n")
        tryCatch({
            rv$inputDf <- read.csv(rv$inputData,
                                   header = TRUE,
                                   sep = input$sep,
                                   quote = input$quote)
        }, error = function(e) {
            errorMsg = paste("Could not load CSV:",e)
            cat(file=stderr(), errorMsg, "\n")
            shinyalert("Oops!", errorMsg, type = "error")
            safeError(e)
        })
    }
}

server <- function(input, output, session) {
    rv <- reactiveValues(
        inputData = NULL,
        inputDf = NULL
    )
    
    observeEvent(input$inputSample, {
        rv$inputData=input$inputSample
    })
    
    observeEvent(input$inputCSV, {
        rv$inputData=input$inputCSV$datapath
    })
    
    # Update dataset if:
    #   New data file
    #   New separator
    #   New quote
    observeEvent({
        input$sep
        input$quote
        rv$inputData
    }, {
        loadDataset(input, rv)
    })
    
    output$tableContent <- renderTable({
        return(head(rv$inputDf))
    })
    
    # Logic for tab uiOutput("tabTable") ----
    output$tabTable <- renderUI({
        
        if (is.null(rv$inputData) || rv$inputData == "") {
            return(tags$h3(MSG_NO_INPUT_DATA))
        }
        
        tagList(
            tags$h3("Input data table head"),
            tags$hr(),
            div(style = 'overflow-x: auto', tableOutput("tableContent"))
        )
    })
    # Logic for tab stabilityUI("tabStability") ----
    # 'tabStability' UI is managed by 'stabilityUI'
    callModule(stability, "tabStability", rv)
    # Logic for tab qualityUi("tabQuality") ----
    # 'tabQuality' UI is managed by 'qualityUi'
    callModule(quality, "tabQuality", rv)
    # Logic for tab correlationsUI("tabCorrelations") ----
    # 'tabCorrelations' UI is managed by 'correlationsUI'
    callModule(correlations, "tabCorrelations", rv)
    # Logic for tab optimalkUI("tabOptimalk") ----
    # 'tabOptimalk' UI is managed by 'optimalkUI'
    callModule(optimalk, "tabOptimalk", rv)
    
    
}