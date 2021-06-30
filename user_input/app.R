library(shiny)
library(jsonlite)
library(hash)

weather_tabs <- tabsetPanel(
    id = "coordinates",
    type = "hidden",
    tabPanel("No", 
             numericInput("long", "Longitude (in \u00B0)", min = -180, max = 180, value = 0),
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
                 tabPanel("Animals",
                          sidebarPanel(
                              tabsetPanel(
                                  tabPanel("Herd Information",
                                           numericInput("calf_num", "Number of calves randomly selected from initialization herd: ",
                                                        min = 0, value = 0),
                                           numericInput("heiferI_num", "Number of heifers between weaning and first breeding randomly selected from initialization herd: ",
                                                        min = 0, value = 0),
                                           numericInput("heiferII_num", "Number of heifers between first breeding and close to parturition randomly selected from the initialization herd : ",
                                                        min = 0, value = 0),
                                           numericInput("heiferIII_num", "Number of heifers close to parturition randomly selected from the initialization herd: ",
                                                        min = 0, value = 0),
                                           numericInput("cow_num", "Number of cows randomly selected from the initializa-tion herd: ",
                                                        min = 0, value = 0),
                                           numericInput("replace_num", "Number of replacements: ",
                                                        min = 0, value = 0), # TODO: check prompt
                                           numericInput("herd_num", "Goal for number of cows in the herd: ",
                                                        min = 0, value = 0),
                                           selectInput("herd_init","Do you wish to simulate a replacement herd database?",
                                                       choices = c("Yes", "No")),
                                           selectInput("breed", "Breed of cattle in the simulation: ",
                                                       choices = c("HO (Holsteins)","JE (Jerseys)"))),
                                  tabPanel("Management Decisions",
                                           numericInput("breeding_start_day_h", "Target start days born of reproduction protocols: ",
                                                       min = 1, value = 1),
                                           selectInput("heifer_repro_method", "Reproductive protocol for heifers: ",
                                                       choices = c("TAI")), # TODO: add more methods?
                                           selectInput("cow_repro_method", "Reproductive protocol for cows: ",
                                                       choices = c("TAI")), # TODO: add more methods?
                                           selectInput("semen_type", "Type of semen used in reproduction protocols: ",
                                                       choices = c("Conventional","Sexed")),
                                           numericInput("days_in_preg_when_dry", "Days when the cow is dried off after parturition: ",
                                                        min = 1, value = 1),
                                           selectInput("lactation_curve", "Model selection for milk production: ",
                                                       choices = c("Wood","Milkbot")),
                                           numericInput("heifer_repro_cull_time", "Days old when a heifer would be culled if unsuccessful in bredding: ",
                                                        min = 1, value = 1),
                                           numericInput("repro_cull_time", "Threshold of heifer culling age, when the heifer is not pregnant at this age, she will be culled for repro failure: ",
                                                        min = 1, value = 1),
                                           numericInput("do_not_breed_time", "Days in pregnancy when reproduction protocols are stopped, when the cow is not pregnant at this DIM, it would not be bred anymore and will be culled when her milk production drops below the production culling line.: ",
                                                        min = 1, value = 1),
                                           numericInput("cull_milk_production", "Minimum milk production before animal is culled: ",
                                                        min = 0, value = 1), #TODO: lower/upper limits?
                                           sliderInput("cow_times_milked_per_day", "Number of times per day cows are milked: ",
                                                       min = 1, max = 5, value = 1), #TODO: upper limit?
                                           downloadLink("download_animal_json","Download Animal JSON File")
                                           )
                              ), width = 12)),
                 tabPanel("Feed"),
                 tabPanel("Output")
                 
)

# Define server logic ----
server <- function(input, output, session) {
    dates <- reactive({
        shiny::validate(
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
        shiny::validate(
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
    
   # herd_info <- reactive({
   #     df <- data.frame(
   #         calf_num = input$calf_num,
   #         heiferI_num = input$heiferI_num)
   # })
   # 
   # management_decisions <- reactive({
   #     df <- data.frame(
   #     breeding_start_day_h = input$breeding_start_day_h,
   #     heifer_repro_method = input$heifer_repro_method,
   #     cow_repro_method = input$cow_repro_method)
   # })
   # 
   # animal_df <- reactive({
   #     df <-data.frame(
   #     herd_information = toJSON(herd_info()),
   #     animal_configuration = toJSON(management_decisions()))
   # })
       
    # herd information
    calf_num <- (reactive({input$calf_num}))
    heiferI_num <- reactive({input$heiferI_num})
    heiferII_num <- reactive({input$heiferII_num})
    heiferIII_num <- reactive({input$heiferIII_num})
    cow_num <- reactive({input$cow_num})
    replace_num <- reactive({input$replace_num})
    herd_num <- reactive({input$herd_num})
    herd_init <- reactive({if(input$herd_init == "Yes") TRUE else FALSE})
    breed <- reactive({substr(input$breed ,start = 1, stop=unlist(gregexpr(" ",input$breed))[1]-1)})

    # animal configuration
    # management decisions
    breeding_start_day_h <- reactive({input$breeding_start_day_h})
    heifer_repro_method <- reactive({input$heifer_repro_method})
    cow_repro_method <- reactive({input$cow_repro_method})
    semen_type <- reactive(tolower({input$semen_type}))
    days_in_preg_when_dry <- reactive({input$days_in_preg_when_dry})
    lactation_curve <- reactive(tolower({input$lactation_curve}))
    heifer_repro_cull_time <- reactive({input$heifer_repro_cull_time})
    repro_cull_time <- reactive({input$repro_cull_time})
    do_not_breed_time <- reactive({input$do_not_breed_time})
    cull_milk_production <- reactive({input$cull_milk_production})
    cow_times_milked_per_day <- reactive({input$cow_times_milked_per_day})
    
    
    # repro
    heifer_repro_method <- reactive({input$heifer_repro_method})
    cow_repro_method <- reactive({input$cow_repro_method})
    
    animal_JSON <- reactive({
    sprintf('{
    "herd_information": {
        "calf_num": %i,
        "heiferI_num": %i,
        "heiferII_num": %i,
        "heiferIII_num": %i,
        "cow_num": %i,
        "replace_num": %i,
        "herd_num": %i,
        "herd_init": %d,
        "breed": "%s"
    },
    "animal_config": {
        "management_decisions": {
            "breeding_start_day_h": %i,
            "heifer_repro_method": "%s",
            "cow_repro_method": "%s",
            "semen_type": "%s",
            "days_in_preg_when_dry": %i,
            "lactation_curve": "%s",
            "heifer_repro_cull_time": %i,
            "repro_cull_time": %i,
            "do_not_breed_time": %i,
            "cull_milk_production": %i,
            "cow_times_milked_per_day": %i
        },
        "repro": {
            "heifer_repro_method": "%s",
            "cow_repro_method": "%s"
        }
    }
}'  ,calf_num(),heiferI_num(),heiferII_num(),heiferIII_num(),
    cow_num(),replace_num(),herd_num(),herd_init(),breed(),breeding_start_day_h(),
    heifer_repro_method(),cow_repro_method(),semen_type(),days_in_preg_when_dry(),
    lactation_curve(),heifer_repro_cull_time(),repro_cull_time(),do_not_breed_time(),
    cull_milk_production(),cow_times_milked_per_day(),
    heifer_repro_method(),cow_repro_method())
    })
    
    output$download_animal_json <- downloadHandler(
        filename = function() {
            paste("my_RuFaS_animal-", Sys.Date(), ".json")
        },
       content = function(file) {
           writeLines(animal_JSON(),file)
       }
    )
}
# Run the app ----
shinyApp(ui = ui, server = server)