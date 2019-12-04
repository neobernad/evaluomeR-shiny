library(shiny)

stabilityUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$h3("Stability analysis from new UI"),
    tags$hr()
  )
}

stability <- function(input, output, session, data) {
  #data$inputData = "data/dataset-metrics-Imbeaudetal-NAR2005.csv"
  
  if (is.null(data$inputData) || data$inputData == "") {
    return(tags$h3("No input data selected"))
  }
  
  
}