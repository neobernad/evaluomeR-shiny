library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(evaluomeR)

source("modules/stability.R")
source("modules/quality.R")
source("modules/correlations.R")

ui <- navbarPage(theme = shinytheme("paper"),
    windowTitle = "evaluomeR App",
    title = "evaluomeR", # Application title
    footer = tagList(tags$hr()),
    inverse = TRUE, # Dark theme of cur. theme
    # Analysis tab  ----
    tabPanel("Analysis",
        useShinyalert(),
        useShinyjs(),
        # Analysis tab - Sidebar ====
        sidebarLayout(
            sidebarPanel(
                fileInput("inputCSV", "Choose a CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                selectInput("inputSample", "Or select a sample CSV:", 
                            #choices=c("Choose one" = "", SAMPLE_FILE_MAP),
                            choices=c(SAMPLE_FILE_MAP),
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
                helpText(paste0("evaluomeR v", package.version("evaluomeR"))),
                width = 2 # sidebarPanel width
            ),
            
            # Analysis tab - Main panel ====
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Correlations", correlationsUI("tabCorrelations")),
                            tabPanel("Stability", stabilityUI("tabStability")),
                            tabPanel("Quality", qualityUI("tabQuality")),
                            tabPanel("Table", uiOutput("tabTable"))
                )
            )
        ) #, tabPanel(...) for more tabs. Example: https://shiny.rstudio.com/gallery/navbar-example.html
    )
)