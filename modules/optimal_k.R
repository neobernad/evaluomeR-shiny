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
      column(12, wellPanel(id = ns("optimalkConf"), 
                           style = "overflow: hidden;", 
                           fluidRow( # Row of configs
                             column(12, tags$h5("Configuration parameters"))
                           ),
                           fluidRow(
                             column(3, selectInput(ns("cbiStability"), "Select a classification algorithm for stability:", 
                                                   choices=evaluomeRSupportedCBI(), multiple = FALSE)),
                             column(3, selectInput(ns("cbiQuality"), "Select a classification algorithm for quality:", 
                                                   choices=evaluomeRSupportedCBI(), multiple = FALSE)),
                             column(3, numericInput(ns("kmin"), "Min. num. of clusters:", 2, min = 2, max = 15)),
                             column(3, numericInput(ns("kmax"), "Max. num. of clusters:", 3, min = 2, max = 15))
                           ),
                           fluidRow(
                             column(3, numericInput(ns("bs"), "Bootstrap:", 20, min = 20, max = 500, step=10)),
                             column(3, numericInput(ns("seedStability"), "Seed:", 20, min = 1, step=10))
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
    stabilityData=NULL,
    qualityData=NULL,
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
    results$stabilityData=NULL
    results$qualityData=NULL
    results$visibleDownloadButtons = FALSE
    output$optimalkResult = NULL
    shinyjs::html("evaluomeROutput", "")
  })
  
  output$btnDownloadCSV <- downloadHandler(
    filename = function(){
      paste0("optimal_k",".zip")
      
    },
    content = function(file){
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      fileName <- paste("optimal_k.csv",sep = "")
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
    
    results$stabilityData = runStability(data$inputDf, input$kmin, input$kmax,
                                       input$cbiStability, input$bs, input$seed)
    results$qualityData = runQuality(data$inputDf, input$kmin, input$kmax, 
                                     input$cbiQuality, input$seed)
    results$optimalkData = runOptimalk(data$inputDf, 
                                       input$kmin, input$kmax,
                                       results$stabilityData, results$qualityData)
    if (is.null(results$optimalkData)) {
      shinyalert("Oops!", MSG_CORRELATIONS_WENT_WRONG, type = "error")
      return(NULL)
    }
    
    # Render tables
    renderOptimalkTablesTabs(output, results)
    
    results$visibleDownloadButtons = TRUE
  })
  
  return(NULL)
  
}

# Correlations private functions ----
# Runs an evaluomeR correlations execution
runOptimalk <- function(df, kmin, kmax, stabilityData, qualityData) {
  cat(file=stderr(), 
      "Running optimal K algorithm for [,", kmin, ", ", kmax, "]\n")
  
  result <- NULL
  withCallingHandlers({
    shinyjs::html("evaluomeROutput", "")
    shinyjs::html(id = "evaluomeROutput", html = paste0("Calculating optimal K values...", "<br>"), add = TRUE)
    result <- getOptimalKValue(stabilityData, qualityData, k.range=c(kmin,kmax))
    shinyjs::html(id = "evaluomeROutput", html = paste0("Done", "<br>"), add = TRUE)
  },
  message = function(m) {
    shinyjs::html(id = "evaluomeROutput", html = paste0(m$message, "<br>"), add = TRUE)
  })
  #shinyjs::disable("btnExecute")
  
  return(result)
}

renderOptimalkTablesTabs <- function(output, results) {
  
  #optimalKTable = NULL
  #optimalKTable$Metric = results$optimalkData$Metric
  #optimalKTable$Optimal_k = results$optimalkData$Global_optimal_k
  #optimalKTable = as.data.frame(optimalKTable)
  
  output$optimalkResult <- renderUI({
    tabNum <- 1
    tabNames <- c("Optimal K values")
    
    tagList(
      tags$h4("Result tables"),
      fluidRow(
        column(12,
               do.call(tabsetPanel,
                       c(id='optimalKTable',
                         lapply(1:tabNum, function(i) {
                           tabName <- tabNames[i]
                           tabPanel(
                             title=paste0(tabName), 
                             renderTable({results$optimalkData})
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



