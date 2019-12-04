wd = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")

SAMPLE_FILE_MAP = c("Gene expression differences" = paste0(wd, "data/dataset-metrics-Imbeaudetal-NAR2005.csv"),
                    "78 AgroPortal ontologies" = paste0(wd, "data/agro.csv"),
                    "119 OBO Foundry ontologies" = paste0(wd, "data/obo-119.csv"))


ui <- fluidPage(
    # Application title
    titlePanel("evaluomeR"),

    # Sidebar with a slider input for number of bins
    # Sidebar ----
    sidebarLayout(
        sidebarPanel(
            fileInput("inputCSV", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            selectInput("inputSample", "Or select a sample CSV:", 
                        choices=c("Choose one" = "", SAMPLE_FILE_MAP),
                        multiple = FALSE),
            
            tags$hr(),
            div(style="display:flex",
                
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            div(style="width: 100px;",HTML("<br>")),
            
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"')
            ),
            
            tags$hr(),
            helpText(paste0("evaluomeR v", package.version("evaluomeR")))
        ),
        
        # Main panel ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Stability", NULL),
                        tabPanel("Quality", NULL),
                        tabPanel("Correlations", NULL),
                        tabPanel("Table", uiOutput("tabTable"))
            )
        )
    )
)