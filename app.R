# App

set.seed(1234)

ui <- source("./ui.R")

server <- source("./server.R")

# Run the application 
shinyApp(ui = ui, server = server, display.mode = "normal")