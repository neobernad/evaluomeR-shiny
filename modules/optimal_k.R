library(shiny)
library(shinyjs)
library(plotly)
library(stringr)
library(viridis)
library(reshape2)

# Optimal k UI ----
optimalkUI <- function(id) {
  ns <- NS(id)
  # Optimal k configuration parameters ----
  tagList(
    tags$h3("Optimal K value"),
    fluidRow(
      column(12, wellPanel(id = ns("qualityConf"), 
                           style = "overflow: hidden;", 
                           fluidRow( # Row of configs
                             column(12, tags$h5("Quality configuration parameters"))
                           ),
                           fluidRow(
                             column(2, selectInput(ns("plotType"), "Plot type:", 
                                                   choices=c("full", "triangular"), multiple = FALSE))
                           ),
                           tags$hr(),
                           fluidRow( # Row of buttons
                             column(3, actionButton(ns("btnExecute"), "Execute", icon("terminal"), 
                                                    style="color: #fff; background-color: #337ab7; ")),
                             column(3, hidden(downloadButton(ns("btnDownloadCSV"), "CSV", 
                                                             style="color: #fff; background-color: #0c9463; ")))
                           )
      ) # End well panel
      )
      #, column(12, tags$hr())
    ),
    tags$hr(),
    fluidRow(
      column(12, 
             div(style="max-height: 200px; width: 100%; border-left: 5px solid #2196F3; background-color: #ddffff; padding-left: 10px; overflow-y: auto;",
                 textOutput(ns("evaluomeROutput")))
      )
    ),
    # Optimal k output div handled by 'optimalk' function ----
    uiOutput(ns("optimalkResult"))
  )
}

# Optimal k Logic ----
optimalk <- function(input, output, session, data) {
  
  results <- reactiveValues(
    optimalkData=NULL,
    visibleDownloadButtons=FALSE
  )
  
  output$optimalkResult <- renderUI({
    if (is.null(data$inputData) || data$inputData == "" || is.null(data$inputDf)) {
      return(fluidRow(column(12, tags$h3(MSG_NO_INPUT_DATA))))
    }
  })
  
  # If input data changes, reset and disable buttons
  observeEvent(data$inputData, {
    results$optimalkData=NULL
    results$visibleDownloadButtons = FALSE
    output$correlationsResult = NULL
    shinyjs::html("evaluomeROutput", "")
  })
  
  output$btnDownloadCSV <- downloadHandler(
    filename = function(){
      paste0("optimalk",".zip")
      
    },
    content = function(file){
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      fileName <- paste("correlations.csv",sep = "")
      write.table(assay(results$optimalkData),
                               fileName,
                               sep = ',',
                               row.names = F,
                               col.names = T)
      files <- c(fileName,files)
      #create the zip file
      zip(file,files)
    }
  )
  
  observeEvent(results$visibleDownloadButtons, {
    if (results$visibleDownloadButtons) {
      shinyjs::show("btnDownloadCSV", anim = FALSE)
    } else {
      shinyjs::hide("btnDownloadCSV", anim = FALSE)
    }
  })
  
  observeEvent(input$btnExecute, {
    if (is.null(data$inputDf)) {
      return(NULL)
    }
    results$optimalkData =  runOptimalk(data$inputDf)
    if (is.null(results$optimalkData)) {
      shinyalert("Oops!", MSG_CORRELATIONS_WENT_WRONG, type = "error")
      return(NULL)
    }
    
    # Render tables
    renderOptimalkTablesTabs(output, results, plotType = input$plotType)
    
    results$visibleDownloadButtons = TRUE
  })
  
  return(NULL)
  
}

# Correlations private functions ----
# Runs an evaluomeR correlations execution
runOptimalk <- function(df) {
  cat(file=stderr(), 
      "Running correlations\n")
  
  result <- NULL
  withCallingHandlers({
    shinyjs::html("evaluomeROutput", "")
    shinyjs::html(id = "evaluomeROutput", html = paste0("Calculating correlations...", "<br>"), add = TRUE)
    result <- metricsCorrelations(data=df, getImages = FALSE)
    shinyjs::html(id = "evaluomeROutput", html = paste0("Done", "<br>"), add = TRUE)
  },
  message = function(m) {
    shinyjs::html(id = "evaluomeROutput", html = paste0(m$message, "<br>"), add = TRUE)
  })
  #shinyjs::disable("btnExecute")
  
  return(result)
}

renderOptimalkTablesTabs <- function(output, results, plotType) {
  output$correlationsResult <- renderUI({
    tabNum <- 1
    tabNames <- c("Correlations")
    
    tagList(
      tags$h4("Result tables"),
      fluidRow(
        column(12,
               do.call(tabsetPanel,
                       c(id='correlationTable',
                         lapply(1:tabNum, function(i) {
                           tabName <- tabNames[i]
                           tabPanel(
                             title=paste0(tabName), 
                             renderTable({assay(results$optimalkData)})
                           )
                         }
                         )
                       )
               )
        )
      ),
      tags$hr()
    )
    
  })
}



