library(shiny)
library(shinyjs)
library(R.utils)

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
                               style="color: #fff; background-color: #0c9463; "))),
                        column(3, hidden(downloadButton(ns("btnDownloadImages"), "Images", 
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
  
  output$btnDownloadCSV <- downloadHandler(
    filename = function() {
      paste("stabilityData", ".tar.gz", sep = "")
    },
    content = function(file) {
      #write.csv("data", file)
      tmpdir <- tempdir()
      setwd(tempdir())
      csvNameList = unlist(lapply(names(results$stabilityData), 
                           function(csvName) {paste0(csvName, ".csv")}), use.names=FALSE)
      print(list.files(path = "."))
      dfName = names(results$stabilityData)
      for (i in 1:length(csvNameList)) {
        #print(assay(results$stabilityData[dfName[i]]))
        write.csv(assay(results$stabilityData[dfName[i]]), file=csvNameList[i], row.names = FALSE)
      }
      # TODO: El tar está vacío
      print(csvNameList)
      tar(tarfile = file, files="stability_mean.csv")# compression="gzip")
    }
  )
  
  observeEvent(results$visibleDownloadButtons, {
    if (results$visibleDownloadButtons) {
      shinyjs::show("btnDownloadCSV", anim = FALSE)
      shinyjs::show("btnDownloadImages", anim = FALSE)
    } else {
      shinyjs::hide("btnDownloadCSV", anim = FALSE)
      shinyjs::hide("btnDownloadImages", anim = FALSE)
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

    renderTablesTabs(output, results)
    results$visibleDownloadButtons = TRUE
  })
  #return(NULL)
  
  #return (reactive({cat("--> ", data$inputData, "\n")}))
}

# Stability private functions ----
# Runs an evaliomeR stability execution
runStability <- function(df, kmin, kmax, bs, seed) {
  cat(file=stderr(), 
      "Running stability index with kmin=",kmin,", kmax=",kmax,
      ", bs=", bs, ", seed=", seed,"\n")
  shinyjs::disable("btnExecute")
  result <- NULL
  withCallingHandlers({
    shinyjs::html("evaluomeROutput", "")
    result <- stabilityRange(data=df, k.range=c(kmin, kmax), 
                   bs=bs, seed=seed, getImages = FALSE)
  },
  message = function(m) {
     shinyjs::html(id = "evaluomeROutput", html = paste0(m$message, "<br>"), add = TRUE)
    })
  shinyjs::enable("btnExecute")

  return(result)
}

# Renders a table given a stability output 'stabilityData'
renderTablesTabs <- function(output, results) {
  output$stabilityResult <- renderUI({
    tabNum <- length(names(results$stabilityData))
    tabNames <- names(results$stabilityData)
    csvNameList = lapply(names(results$stabilityData), 
                         function(csvName) {paste0(csvName, ".csv")})
    
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
      tags$hr()
    )
    
  })
}

