library(shiny)
library(shinyjs)

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
                        column(3, actionButton(ns("btnDownloadCSV"), "CSV", icon("fas fa-download"), 
                               style="color: #fff; background-color: #0c9463; ")),
                        column(3, actionButton(ns("btnDownloadImages"), "Images", icon("fas fa-download"), 
                               style="color: #fff; background-color: #0c9463; "))
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
  
  output$stabilityResult <- renderUI({
    if (is.null(data$inputData) || data$inputData == "" || is.null(data$inputDf)) {
      return(fluidRow(column(12, tags$h3(MSG_NO_INPUT_DATA))))
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
    stabilityData <- runStability(data$inputDf, input$kmin, input$kmax, input$bs, input$seed)
    if (is.null(stabilityData)) {
      shinyalert("Oops!", MSG_STABILITY_WENT_WRONG, type = "error")
      return(NULL)
    }
    
    # TODO: HIDE BUTTONS!
    renderTables(output, stabilityData)
  })
  return(NULL)
  
  #return (reactive({cat("--> ", data$inputData, "\n")}))
}

# Stability private functions ----
# Runs an evaliomeR stability execution
runStability <- function(df, kmin, kmax, bs, seed) {
  cat(file=stderr(), 
      "Running stability index with kmin=",kmin,", kmax=",kmax,
      ", bs=", bs, ", seed=", seed,"\n")
  shinyjs::disable("btnExecute")
  stabilityData <- NULL
  withCallingHandlers({
    shinyjs::html("evaluomeROutput", "")
    stabilityData <- stabilityRange(data=df, k.range=c(kmin, kmax), 
                   bs=bs, seed=seed, getImages = FALSE)
  },
  message = function(m) {
     shinyjs::html(id = "evaluomeROutput", html = paste0(m$message, "<br>"), add = TRUE)
    })
  shinyjs::enable("btnExecute")

  return(stabilityData)
}

# Renders a table given a stability output 'stabilityData'
renderTables <- function(output, stabilityData) {
  output$stabilityResult <- renderUI({
    tabsetPanel(id = "stabilityTables",
        tabPanel("Hello", "This is the hello tab"),
        tabPanel("Foo", "This is the foo tab"),
        tabPanel("Bar", "This is the bar tab")
    )
    #df <- data.frame(assay(stabilityData))
    #return(head(df))
  })
}