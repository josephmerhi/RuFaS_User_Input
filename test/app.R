library(shiny)
library(jsonlite)

ui <- basicPage(
    downloadButton("downloadData", "Download Data")
)

server <- function(input, output) {
    
    data <- reactive({
        df <- data.frame(name = c("Jon", "Bill", "Maria"),
                         age = c(23, 41, 32))
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            "data.json"
        },
        content = function(file) {
            write_json(data(), file)
        }
    )
}

shinyApp(ui, server)