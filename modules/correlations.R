library(shiny)
library(shinyjs)
library(plotly)
library(stringr)
library(viridis)
library(reshape2)

# Correlations UI ----
correlationsUI <- function(id) {
  ns <- NS(id)
  # Correlations configuration parameters ----
  tagList(
    tags$h3("Metrics correlations"),
    fluidRow(
      column(12, wellPanel(id = ns("qualityConf"), 
                           style = "overflow: hidden;", 
                           fluidRow( # Row of configs
                             column(12, tags$h5("Configuration parameters"))
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
    # Correlations output div handled by 'correlations' function ----
    uiOutput(ns("correlationsResult"))
  )
}

# Correlations Logic ----
correlations <- function(input, output, session, data) {
  
  results <- reactiveValues(
    correlationsData=NULL,
    visibleDownloadButtons=FALSE
  )
  
  output$correlationsResult <- renderUI({
    if (is.null(data$inputData) || data$inputData == "" || is.null(data$inputDf)) {
      return(fluidRow(column(12, tags$h3(MSG_NO_INPUT_DATA))))
    }
  })
  
  # If input data changes, reset and disable buttons
  observeEvent(data$inputData, {
    results$correlationsData=NULL
    results$visibleDownloadButtons = FALSE
    output$correlationsResult = NULL
    shinyjs::html("evaluomeROutput", "")
  })
  
  output$btnDownloadCSV <- downloadHandler(
    filename = function(){
      paste0("correlations",".zip")
      
    },
    content = function(file){
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      fileName <- paste("correlations.csv",sep = "")
      write.table(assay(results$correlationsData),
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
    results$correlationsData =  runCorrelations(data$inputDf)
    if (is.null(results$correlationsData)) {
      shinyalert("Oops!", MSG_CORRELATIONS_WENT_WRONG, type = "error")
      return(NULL)
    }
    
    # Render tables
    renderCorrelationsTablesTabs(output, results, plotType = input$plotType)
    
    results$visibleDownloadButtons = TRUE
  })
  
  return(NULL)
  
}

# Correlations private functions ----
# Runs an evaluomeR correlations execution
runCorrelations <- function(df) {
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

renderCorrelationsTablesTabs <- function(output, results, plotType) {
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
                             renderTable({assay(results$correlationsData)})
                           )
                         }
                         )
                       )
               )
        )
      ),
      tags$hr(),
      tags$h4("Result figures"),
      renderCorrelationsFigures(results, plotType = plotType),
      tags$hr()
    )
    
  })
}

renderCorrelationsFigures <- function(results, plotType) {
  
  # Correlations
  if (is.null(results$correlationsData)) {
    return (NULL)  
  }
  
  correlationsDf = as.data.frame(assay(results$correlationsData))
  dataMatrix = as.matrix(correlationsDf)
  
  switch(plotType, 
         triangular={
           dataMatrix[upper.tri(dataMatrix)] <- NA  
         }
  )
  
  p_correlations <- plot_ly(correlationsDf,
                            x=rownames(correlationsDf),
                            y=colnames(correlationsDf),
                            z=dataMatrix,
                            reversescale = TRUE,
                            type="heatmap") %>% layout (
                              title = "Correlations heatmap",
                              xaxis = list(ticks = ""),
                              yaxis = list(ticks = "")
                            )
  
  return(renderUI({
    
    tagList(
      tags$h5("Correlations"),
      fluidRow(column(6, renderPlotly({
        p_correlations
      })))
    )
  }))
} 


