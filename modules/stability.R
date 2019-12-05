library(shiny)

stabilityUI <- function(id) {
  ns <- NS(id)
  # Stability configuration parameters ----
  tagList(
    tags$h3("Stability analysis"),
    #tags$hr(),
    fluidRow(
      column(12, wellPanel(id = "stabilityConf", 
                           style = "overflow: hidden;", 
                           fluidRow(
                            column(12, tags$h5("Configuration parameters")),
                            column(3, numericInput(ns("kmin"), "Min. num. of clusters:", 2, min = 2, max = 15)),
                            column(3, numericInput(ns("kmax"), "Max. num. of clusters:", 3, min = 2, max = 15)),
                            column(3, numericInput(ns("bs"), "Bootstrap:", 20, min = 20, max = 500, step=10))
                           ),
                           fluidRow(actionButton(ns("execute"), "Execute", icon("terminal"), 
                                     style="color: #fff; background-color: #337ab7; "))
                          )
             )
      #, column(12, tags$hr())
    ),
    # Stability output div handled by 'stability' function ----
    uiOutput(ns("stabilityResult"))
    #fluidRow(
    #  tags$div(id='stabilityResult')
    #)
  )
}

stability <- function(input, output, session, data) {
  
  output$stabilityResult <- renderUI({
    if (is.null(data$inputData) || data$inputData == "") {
      return(fluidRow(column(12, tags$h3(MSG_NO_INPUT_DATA))))
    }
  })
  
  observeEvent(input$execute, {
    if (!is.null(data$inputDf)) {
      cat("File -> !", data$inputData,"\n") 
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
  })
  
  #return (reactive({cat("--> ", data$inputData, "\n")}))
  
  
}