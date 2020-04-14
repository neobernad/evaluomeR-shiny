library(shiny)
library(shinyjs)
library(plotly)
library(stringr)
library(viridis)
library(reshape2)

# Quality UI ----
qualityUI <- function(id) {
  ns <- NS(id)
  # Quality configuration parameters ----
  tagList(
    tags$h3("Quality analysis"),
    #tags$hr(),
    fluidRow(
      column(12, wellPanel(id = ns("qualityConf"), 
                           style = "overflow: hidden;", 
                           fluidRow( # Row of configs
                             column(12, tags$h5("Configuration parameters"))
                           ),
                           fluidRow(
                             column(3, numericInput(ns("kmin"), "Min. num. of clusters:", 2, min = 2, max = 15)),
                             column(3, numericInput(ns("kmax"), "Max. num. of clusters:", 3, min = 2, max = 15)),
                             column(3, numericInput(ns("seed"), "Seed:", 20, min = 1, step=10))
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
    fluidRow(
      column(12, 
             div(style="max-height: 200px; width: 100%; border-left: 5px solid #2196F3; background-color: #ddffff; padding-left: 10px; overflow-y: auto;",
                 textOutput(ns("evaluomeROutput")))
      )
    ),
    tags$hr(),
    # Stability output div handled by 'stability' function ----
    uiOutput(ns("qualityResult"))
  )
}

# Quality Logic ----
quality <- function(input, output, session, data) {
  
  results <- reactiveValues(
    qualityData=NULL,
    visibleDownloadButtons=FALSE
  )
  
  output$qualityResult <- renderUI({
    if (is.null(data$inputData) || data$inputData == "" || is.null(data$inputDf)) {
      return(fluidRow(column(12, tags$h3(MSG_NO_INPUT_DATA))))
    }
  })
  
  # If input data changes, reset and disable buttons
  observeEvent(data$inputData, {
    results$qualityData=NULL
    results$visibleDownloadButtons = FALSE
    output$qualityResult = NULL
    shinyjs::html("evaluomeROutput", "")
  })
  
  output$btnDownloadCSV <- downloadHandler(
    filename = function(){
      paste0("qualityData",".zip")
      
    },
    content = function(file){
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      for (table_name in names(results$qualityData)) {
        fileName <- paste("quality_", table_name,".csv",sep = "")
        write.table(assay(results$qualityData[[table_name]]),
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
    results$qualityData =  runQuality(data$inputDf, input$kmin, input$kmax, input$seed)
    if (is.null(results$qualityData)) {
      shinyalert("Oops!", MSG_QUALITY_WENT_WRONG, type = "error")
      return(NULL)
    }
    
    # Render tables
    renderQualityTablesTabs(output, results)
    
    results$visibleDownloadButtons = TRUE
  })
  
  return(NULL)
  
}

# Quality private functions ----
# Runs an evaluomeR quality execution
runQuality <- function(df, kmin, kmax, seed) {
  cat(file=stderr(), 
      "Running quality index with kmin=",kmin,", kmax=",kmax,
      ", seed=", seed,"\n")
  
  result <- NULL
  withCallingHandlers({
    shinyjs::html("evaluomeROutput", "")
    result <- qualityRange(data=df, k.range=c(kmin, kmax), 
                             seed=seed, getImages = FALSE)
  },
  message = function(m) {
    shinyjs::html(id = "evaluomeROutput", html = paste0(m$message, "<br>"), add = TRUE)
  })
  #shinyjs::disable("btnExecute")
  
  return(result)
}

renderQualityTablesTabs <- function(output, results) {
  output$qualityResult <- renderUI({
    tabNum <- length(names(results$qualityData))
    tabNames <- names(results$qualityData)
    
    tagList(
      tags$h4("Result tables"),
      fluidRow(
        column(12,
               do.call(tabsetPanel,
                       c(id='qualityTables',
                         lapply(1:tabNum, function(i) {
                           tabName <- tabNames[i]
                           tabPanel(
                             title=paste0(tabName), 
                             renderTable({assay(results$qualityData[tabName])})
                           )
                         }
                         )
                       )
               )
        )
      ),
      tags$hr(),
      tags$h4("Result figures"),
      renderQualityFigures(results),
      tags$hr()
    )
    
  })
}

renderQualityFigures <- function(results) {
  
  qualityDfStand = standardizeQualityData(results$qualityData)
  qualityDfStand$Metric = rownames(qualityDfStand)
  qualityDfStandMelt = melt(qualityDfStand, id="Metric")
  qualityDfStandMelt$value = as.numeric(qualityDfStandMelt$value)
  
  # K behaviour
  
  p_kAcrossMetrics <- plot_ly(qualityDfStandMelt, x = ~Metric,
                              y = ~value, color = ~variable,
                              colors = viridis_pal(option = "D")(length(unique(qualityDfStandMelt$variable))),
                              type = 'scatter', mode = 'lines+markers') %>%
    layout(
      title = "K values across metrics", showlegend = TRUE,
      xaxis = list(title = 'Metrics'), yaxis = list(title = 'Avg. silhouette width', range = c(0,1))
    )
  
  p_MetricsAcrossK <- plot_ly(qualityDfStandMelt, x = ~variable,
                              y = ~value, color = ~Metric, 
                              colors = viridis_pal(option = "D")(length(unique(qualityDfStandMelt$Metric))),
                              type = 'scatter', mode = 'lines+markers') %>%
    layout(
      title = "Metrics across K values", showlegend = TRUE,
      xaxis = list(title = 'K'), yaxis = list(title = 'Avg. silhouette width',range = c(0,1))
    )
  
  # Cluster behaviour
  
  maxNumClusters = length(names(results$qualityData))+1
  color_palette = viridis_pal(option = "D")(maxNumClusters)
  
  ptlist = list()
  i = 1
  for (k_value in names(results$qualityData)) {
    qualityDf = as.data.frame(assay(results$qualityData[[k_value]]))
    qualityDfMelt = melt(qualityDf, id="Metric")
    
    qualityDfMeltClusterSize = qualityDfMelt[str_detect(qualityDfMelt$variable, "Cluster_._Size"), ]
    qualityDfMeltClusterSize$value = as.numeric(qualityDfMeltClusterSize$value)
    qualityDfMeltClusterSize$variable = gsub("_Size","", qualityDfMeltClusterSize$variable)
    
    
    p_clusterSizes <- plot_ly(qualityDfMeltClusterSize, x = ~value,
                  y = ~Metric, color = ~variable, 
                  legendgroup = ~variable,
                  showlegend = if (i == length(names(results$qualityData))) TRUE else FALSE,
                  colors = color_palette[1:length(unique(qualityDfMeltClusterSize$variable))],
                  type = 'bar', orientation='h') %>%
      layout(
        xaxis = list(title = 'Cluster size'), yaxis = list(title = 'Metrics'),
        annotations = list(text = paste0("Cluster of ", k_value),
          xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center",
          align = "center", x = 0.5, y = 1, showarrow = FALSE)
      )
    ptlist[[i]] = p_clusterSizes
    i = i +1
    
  }
  
  nrows = as.integer((length(ptlist) / 2)) + 1
  
  plotGrid = subplot(ptlist, shareY = TRUE, shareX = TRUE, titleX = TRUE, margin = 0.05, nrows = nrows)
  
  
  return(renderUI({
    
    tagList(
      tags$h5("K behaviour"),
      fluidRow(column(6, renderPlotly({
        p_kAcrossMetrics
      }))),
      fluidRow(column(6, renderPlotly({
        p_MetricsAcrossK
      }))),
      tags$hr(),
      tags$h5("Cluster size behaviour"),
      fluidRow(column(12, renderPlotly({
        plotGrid
      })))
    )
  }))
} 
