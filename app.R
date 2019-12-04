library(shiny)
library(evaluomeR)

source(paste0(wd, "server.R"))
source(paste0(wd, "ui.R"))



# Run the application 
shinyApp(ui = ui, server = server)
