server <- function(input, output) {
    rv <- reactiveValues(
        inputData = NULL
    )
    
    observeEvent(input$inputSample, {
        rv$inputData=input$inputSample
    })
    
    observeEvent(input$inputCSV, {
        rv$inputData=input$inputCSV$datapath
    })
    
    output$tableContent <- renderTable({
        tryCatch(
            {
                df <- read.csv(rv$inputData,
                               header = TRUE,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        return(head(df))
    })
    
    # Logic of uiOutput("tabTable") ----
    output$tabTable <- renderUI({
        
        if (is.null(rv$inputData) || rv$inputData == "") {
            return(tags$h3("No input data selected"))
        }
        
        tagList(
            tags$h3("Data table head:"),
            tags$hr(),
            tableOutput("tableContent")
        )
        
        
    })
}