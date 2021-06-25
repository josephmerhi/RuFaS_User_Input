library(shiny)

weather_tabs <- tabsetPanel(
    id = "coordinates",
    type = "hidden",
    tabPanel("No", 
             numericInput("long", "Longitude", min = -180, max = 180, value = 0),
    ),
    tabPanel("Yes",
    )
)

# Define UI ----
ui <- navbarPage("RuFas Input", id = "main",
                 tabPanel("General Configuration",
                          sidebarPanel(
                              dateRangeInput("start_end_dates", "Date range:",
                                             max = Sys.Date(),
                                             format = "mm-dd-yyyy"),
                              textOutput("date"),
                              selectInput("testing","Do you wish to run tests?",
                                          choices = c("Yes", "No")),
                              selectInput("weather_dataset","Can you provide a complete weather dataset?",
                                          choices = c("Yes", "No")),
                              numericInput("lat", "Latitude", min = -90, max = 90, value = 0),
                              weather_tabs,
                              textOutput("coord"),
                              width = 10
                          ),
                          sidebarPanel(
                              actionButton("goto_fields","Next"),width=3
                          )),
                 tabPanel("Fields"),
                 tabPanel("Animals"),
                 tabPanel("Feed"),
                 tabPanel("Output")
                 
)

# Define server logic ----
server <- function(input, output, session) {
    dates <- reactive({
        validate(
            need(as.character.Date(input$start_end_dates[1]) != "", "Please enter a valid start date"),
            need(as.character.Date(input$start_end_dates[2]) != "", "Please enter a valid end date"),
            need(input$start_end_dates[1] <= input$start_end_dates[2] , "Start date must be before end date")
        )
    })
    
    output$date <- renderText({
        dates()
    })
    
    observeEvent(input$weather_dataset, {
        updateTabsetPanel(inputId = "coordinates", selected = input$weather_dataset)
    })
    
    observeEvent(input$weather_dataset, {
        updateNumericInput(inputId = "long", value = 0)
    })
    
    coord <- reactive({
        validate(
            need(input$lat>=-90, "Latitude has to be greater than -90 and less than 90"),
            need(input$lat<=90, "Latitude has to be greater than -90 and less than 90"),
            need(input$long>=-180, "Longitude has to be greater than -180 and less than 180"),
            need(input$long<=180, "Longitude has to be greater than -180 and less than 180")
        )
    })
    
    output$coord <- renderText({
        coord()
    })
    
    observeEvent(input$goto_fields, {
        updateNavbarPage(inputId = "main", selected = "Fields")
    })
}
# Run the app ----
shinyApp(ui = ui, server = server)