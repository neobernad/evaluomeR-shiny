library(shiny)
library(shinyjs)
library(plotly)
library(gridExtra)
library(ggpubr)

# Stability UI ----
stabilityUI <- function(id) {
  ns <- NS(id)
  # Stability configuration parameters ----
  tagList(
    tags$h3("Stability analysis"),
    #tags$hr(),
    fluidRow(
      column(12, wellPanel(id = "stabilityConf", 
                           style = "overflow: hidden;", 
                           fluidRow( # Row of configs
                             column(12, tags$h5("Configuration parameters")),
                             column(3, numericInput(ns("kmin"), "Min. num. of clusters:", 2, min = 2, max = 15)),
                             column(3, numericInput(ns("kmax"), "Max. num. of clusters:", 3, min = 2, max = 15)),
                             column(3, numericInput(ns("bs"), "Bootstrap:", 20, min = 20, max = 500, step=10)),
                             column(3, numericInput(ns("seed"), "Seed:", 20, min = 1, step=10))
                           ),
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
    fluidRow(
      column(12, 
             div(style="max-height: 200px; width: 100%; border-left: 5px solid #2196F3; background-color: #ddffff; padding-left: 10px; overflow-y: auto;",
                 textOutput(ns("evaluomeROutput")))
      )
    ),
    tags$hr(),
    # Stability output div handled by 'stability' function ----
    uiOutput(ns("stabilityResult"))
    #fluidRow(
    #  tags$div(id='stabilityResult')
    #)
  )
}

# Stability Logic ----
stability <- function(input, output, session, data) {
  
  results <- reactiveValues(
    stabilityData=NULL,
    visibleDownloadButtons=FALSE
  )
  
  output$stabilityResult <- renderUI({
    if (is.null(data$inputData) || data$inputData == "" || is.null(data$inputDf)) {
      return(fluidRow(column(12, tags$h3(MSG_NO_INPUT_DATA))))
    }
  })
  
  # If input data changes, reset and disable buttons
  observeEvent(data$inputData, {
    results$stabilityData=NULL
    results$visibleDownloadButtons = FALSE
    output$stabilityResult = NULL
    shinyjs::html("evaluomeROutput", "")
  })
  
  output$btnDownloadCSV <- downloadHandler(
    filename = function(){
      paste0("stabilityData",".zip")
      
    },
    content = function(file){
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      for (table_name in names(results$stabilityData)) {
        fileName <- paste(table_name,".csv",sep = "")
        write.table(assay(results$stabilityData[[table_name]]),
                    fileName,
                    sep = ',',
                    row.names = F,
                    col.names = T)
        files <- c(fileName,files)
      }
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
    if (input$kmin > 15 || input$kmin < 2) {
      shinyalert("Oops!", MSG_K_MIN_NOT_IN_RANGE, type = "error")
      return(NULL)
    }
    if (input$kmax > 15 || input$kmax < 2) {
      shinyalert("Oops!", MSG_K_MAX_NOT_IN_RANGE, type = "error")
      return(NULL)
    }
    if (input$kmin > input$kmax) {
      shinyalert("Oops!", MSG_K_MIN_GREATER_THAN_K_MAX, type = "error")
      return(NULL)
    }
    results$stabilityData =  runStability(data$inputDf, input$kmin, input$kmax, input$bs, input$seed)
    if (is.null(results$stabilityData)) {
      shinyalert("Oops!", MSG_STABILITY_WENT_WRONG, type = "error")
      return(NULL)
    }
    
    # Render tables
    renderTablesTabs(output, results)
    
    results$visibleDownloadButtons = TRUE
  })
  return(NULL)
  
  #return (reactive({cat("--> ", data$inputData, "\n")}))
}

# Stability private functions ----
# Runs an evaluomeR stability execution
runStability <- function(df, kmin, kmax, bs, seed) {
  cat(file=stderr(), 
      "Running stability index with kmin=",kmin,", kmax=",kmax,
      ", bs=", bs, ", seed=", seed,"\n")
  
  result <- NULL
  withCallingHandlers({
    shinyjs::html("evaluomeROutput", "")
    result <- stabilityRange(data=df, k.range=c(kmin, kmax), 
                             bs=bs, seed=seed, getImages = FALSE)
  },
  message = function(m) {
    shinyjs::html(id = "evaluomeROutput", html = paste0(m$message, "<br>"), add = TRUE)
  })
  #shinyjs::disable("btnExecute")
  
  return(result)
}

# Renders a table given a stability output 'stabilityData'
renderTablesTabs <- function(output, results) {
  output$stabilityResult <- renderUI({
    tabNum <- length(names(results$stabilityData))
    tabNames <- names(results$stabilityData)
    
    tagList(
      tags$h5("Result tables"),
      fluidRow(
        column(12,
               do.call(tabsetPanel,
                       c(id='stabilityTables',
                         lapply(1:tabNum, function(i) {
                           tabName <- tabNames[i]
                           tabPanel(
                             title=paste0(tabName), 
                             renderTable({assay(results$stabilityData[tabName])})
                           )
                         }
                         )
                       )
               )
        )
      ),
      tags$hr(),
      tags$h5("Result figures"),
      renderFigures(results),
      tags$hr()
    )
    
  })
}

renderFigures <- function(results) {
  stabilityMeanDf = as.data.frame(assay(results$stabilityData[["stability_mean"]]))
  names(stabilityMeanDf) = gsub(x = names(stabilityMeanDf), pattern="^.*_.*_.*_", replacement = "k_")
  stabilityMeanDfMelted = melt(stabilityMeanDf, id="Metric")
  stabilityMeanDfMelted$value = as.numeric(stabilityMeanDfMelted$value)
  
  
  p_kAcrossMetrics <- plot_ly(stabilityMeanDfMelted, x = ~Metric,
          y = ~value, color = ~variable, colors = "Set1",
          type = 'scatter', mode = 'lines+markers') %>%
    layout(
      title = "K values across metrics",
      showlegend = TRUE,
      xaxis = list(
        title = 'Metrics'
      ),
      yaxis = list(
        title = 'Stability index',
        range = c(0,1)
      )
    )
  
  p_MetricsAcrossK <- plot_ly(stabilityMeanDfMelted, x = ~variable,
                              y = ~value, color = ~Metric, colors = "Set1",
                              type = 'scatter', mode = 'lines+markers') %>%
    layout(
      title = "Metrics across K values",
      showlegend = TRUE,
      xaxis = list(
        title = 'K'
      ),
      yaxis = list(
        title = 'Stability index',
        range = c(0,1)
      )
    )
  
  
  
  return(renderUI({
    
    tagList(
    #tags$h6("ASD"),
    fluidRow(column(6, renderPlotly({
      p_kAcrossMetrics
    }))),
    tags$hr(),
    fluidRow(column(6, renderPlotly({
      p_MetricsAcrossK
    })))
    )
  }))
} 

