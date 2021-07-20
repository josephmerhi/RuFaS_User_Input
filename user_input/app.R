library(shiny)
library(jsonlite)
library(shinyMatrix)
library(shinythemes)

weather_tabs <- tabsetPanel(
    id = "coordinates",
    type = "hidden",
    tabPanel("No", 
             numericInput("long", "Longitude (in \u00B0)", min = -180, max = 180, value = 0),
    ),
    tabPanel("Yes",
    )
)

soil_layers <- matrix("",18,5)
colnames(soil_layers) <- c("Layer 1","Layer 2","Layer 3","Layer 4", "Layer 5")
rownames(soil_layers) <- c("Lower depth of soil layer",
                 "Fractional value at which point soil water becomes plant unavailable",
                 "Fractional value of soil water at which the soil matric potential is zero",
                 "Saturated soil value (fractional)",
                 "saturated hydraulic conductivity (mm/h)",
                 "Fraction of porosity from which anions are excluded",
                 "Percent of clay per layer (%)",
                 "Soil temperature of each layer",
                 "Bulk density of the soil for the entire depth",
                 "Percentage of layer composed of organic carbon",
                 "NH4 Initializer variable",
                 "Active N %",
                 "Labile Phosphorus",
                 "Active mineral rate",
                 "Volatile exchange factor",
                 "Dentrification rate",
                 "Fraction of soil water in layer",
                 "Fraction of soil organic matter")

crop_rotation <- matrix("",12,5)
colnames(crop_rotation) <- c("Start Year","Repeat every _ years","Planting Date (in Julian day)",
                             "Harvest Date (in Julian Day)","Harvest Type (Scheduled or Optimal)")
rownames(crop_rotation) <- c("Alfalfa","Cereal Rye","Corn",
                             "Fall Oats","Potato","Soybean","Spring Barley",
                             "Spring Wheat","Sugar Beet","Tall Fescue",
                             "Triticale","Winter Wheat")


# Define UI ----
ui <- navbarPage("RuFas Input", theme = shinytheme("sandstone"), id = "main",
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
                              sliderInput("n_fields","Number of Fields: ",
                                          min = 1, max =100, value = 1),
                              actionButton("generate_fields","Generate Fields"),
                              width = 8
                          )),
                 
                 tabPanel("Fields",
                      tabsetPanel(id="fields",
                          tabPanel("Field 1",
                                   tabsetPanel(
                                       tabPanel("Soil",
                                                sidebarPanel(
                                                numericInput("profile_bulk_density_1","Bulk density of the soil for the entire depth:",
                                                             0, 0),
                                                numericInput("CN2_1", "Curve Number (SCS) represents surface runoff factor of water: ", 0, 0),
                                                numericInput("field_slope_1", "Slope of an individual field (%/100): ", 0, 0),
                                                numericInput("slope_length_1", "Length of slope: ", 0, 0),
                                                numericInput("manning_1", "Mannings roughness coefficient for ground use type: ", 0, 0),
                                                numericInput("field_size_1", "Size of individual field where slope was calculated: ", 0, 0),
                                                numericInput("practice_factor_1", "Ratio of soil loss with a specific support practice to corresponding loss with up-and-down slope culture: ", 0, 0),
                                                numericInput("sand_1", "Fraction of sand (%/100): ", 0, 0),
                                                numericInput("silt_1", "Fraction of silt (%/100): ", 0, 0),
                                                numericInput("soil_albedo_1", "Soil solar radiation absorbance factor: ", 0, 0),
                                                numericInput("initial_residue_1", "Initial amount of soil residue (kg/ha): ", 0, 0),
                                                numericInput("fresh_N_mineral_rate_1", "Nitrogen N mineralization rate from SWAT: ", 0, 0),
                                                selectInput("soil_cover_type_1", "Soil Cover Type: ", c("Bare","Residue Cover","Grassed")),
                                                sliderInput("n_Layers_1","Select the number of soil layers and fill out the corresponding columns in the next table (any
                                                            unused columns can be left empty)", 1, 5, 3)            
                                                            , width = 4
                                                ),
                                                sidebarPanel(matrixInput(
                                                    "soil_layers_1",
                                                    value = soil_layers,
                                                    rows = list(
                                                        extend = FALSE
                                                    ),
                                                    cols = list(
                                                        names = TRUE
                                                    )
                                                ),width = 8)),
                                       tabPanel("Crop",
                                                sidebarPanel(checkboxGroupInput("selected_crops_1","Select the desired crops and fill out the corresponding rows in the next table (any
                                                            unused rows can be left empty)",
                                                                   choices = c("Alfalfa","Cereal Rye","Corn",
                                                                               "Fall Oats","Potato","Soybean","Spring Barley",
                                                                               "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                               "Triticale","Winter Wheat")),width = 4),
                                                sidebarPanel(matrixInput(
                                                    "crop_rotation_1",
                                                    value = crop_rotation,
                                                    rows = list(
                                                        extend = FALSE
                                                    ),
                                                    cols = list(
                                                        names = TRUE
                                                    )
                                                ),
                                                width = 8)
                                                ),
                                       tabPanel("Field Management")
                                   ))))
                      ,
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
                                                        min = 0, value = 1),
                                           sliderInput("cow_times_milked_per_day", "Number of times per day cows are milked: ",
                                                       min = 1, max = 5, value = 1),
                                           downloadLink("download_animal_json","Download Animal JSON File")
                                           )
                              ), width = 12)),
                 tabPanel("Feed",
                          downloadLink("download_all_input","Submit")),
                 tabPanel("Output",
                              tabsetPanel(
                                  tabPanel("Pens",
                                           sidebarPanel(selectInput("pens_report_csv","Do you wish to produce csv files?",
                                                       choices = c("Yes","No")),
                                                       selectInput("pens_report_graph","Do you wish to produce graphics",
                                                       choices = c("Yes","No")),
                                                       textInput("pens_report_name","Pens Report Name: ",value = "pen_report"),width = 12),
                                           sidebarPanel(checkboxGroupInput("pens_ration_report","Ration Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("pens_ration_report_name","Ration Report Name: ",
                                                                  value ="ration_report"),width = 6),
                                           sidebarPanel(checkboxGroupInput("pens_manure_report","Manure Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("pens_manure_report_name","Manure Report Name: ",
                                                                  value ="manure_report"),width = 6),
                                           sidebarPanel(checkboxGroupInput("pens_growth_report","Growth Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("pens_growth_report_name","Growth Report Name: ",
                                                                  value ="growth_report"),width = 6),
                                           sidebarPanel(checkboxGroupInput("pens_summary_report","Summary Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("pens_summary_report_name","Summary Report Name: ",
                                                                  value ="pens_summary"),width = 6)),
                                  tabPanel("Fields",
                                           sidebarPanel(selectInput("fields_report_csv","Do you wish to produce csv files?",
                                                                    choices = c("Yes","No")),
                                                        selectInput("fields_report_graph","Do you wish to produce graphics",
                                                                    choices = c("Yes","No")),
                                                        textInput("fields_report_name","Fields Report Name: ",value = "field_report"),width = 12),
                                           sidebarPanel(checkboxGroupInput("fields_crop_report","Crop Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_crop_report_name","Crop Report Name: ",
                                                                  value ="crop_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_soil_report","Soil Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_soil_report_name","Soil Report Name: ",
                                                                  value ="soil_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_layer_report","Layer Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_layer_report_name","Layer Report Name: ",
                                                                  value ="layer_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_soil_N_report","Soil Nitrogen Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_soil_N_report_name","Soil Nitrogen Report Name: ",
                                                                  value ="soil_nitrogen_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_soil_P_report","Soil Phosphorus Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_soil_P_report_name","Soil Phosphorus Report Name: ",
                                                                  value ="soil_phosphorus_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_soil_C_report","Soil Carbon Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_soil_C_report_name","Soil Carbon Report Name: ",
                                                                  value ="soil_carbon_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_soil_summary_report","Soil Summary Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_soil_summary_report_name","Soil Summary Report Name: ",
                                                                  value ="soil_summary"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_field_management_report","Field Management Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_field_management_report_name","Field Management Report Name: ",
                                                                  value ="field_management_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_P_balance_report","Phosphorus Balance Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_P_balance_report_name","Phosphorus Balance Report Name: ",
                                                                  value ="phosphorus_balance_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_N_balance_report","Nitrogen Balance Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_N_balance_report_name","Nitrogen Balance Report Name: ",
                                                                  value ="nitrogen_balance_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_water_balance_report","Water Balance Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_water_balance_report_name","Water Balance Report Name: ",
                                                                  value ="water_balance_report"),width = 4),
                                           sidebarPanel(checkboxGroupInput("fields_summary_report","Fields Summary Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("fields_summary_report_name","Fields Summary Report Name: ",
                                                                  value ="fields_summary"),width = 4)),
                                  tabPanel("Mass Balance",
                                           sidebarPanel(selectInput("mass_balance_report_csv","Do you wish to produce csv files?",
                                                                    choices = c("Yes","No")),
                                                        selectInput("mass_balance_report_graph","Do you wish to produce graphics",
                                                                    choices = c("Yes","No")),
                                                        textInput("mass_balance_report_name","Mass Balance Report Name: ",value = "mass_balance_report"),width = 12)),
                                  tabPanel("Feed Storage",
                                           sidebarPanel(selectInput("feed_storage_report_csv","Do you wish to produce csv files?",
                                                                    choices = c("Yes","No")),
                                                        selectInput("feed_storage_report_graph","Do you wish to produce graphics",
                                                                    choices = c("Yes","No")),
                                                        textInput("feed_storage_report_name","Feed Storage Report Name: ",value = "feed_storage_report"),width = 12),
                                           sidebarPanel(checkboxGroupInput("storage_report","Storage Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("storage_report_name","Storage Report Name: ",
                                                                  value ="storage_report"),width = 6),
                                           sidebarPanel(checkboxGroupInput("feed_storage_summary","Feed Storage Summary Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("feed_storage_summary_report_name","Feed Storage Summary Report Name: ",
                                                                  value ="feed_storage_summary"),width = 6)),
                                  tabPanel("Manure Storage",
                                           sidebarPanel(selectInput("manure_storage_report_csv","Do you wish to produce csv files?",
                                                                    choices = c("Yes","No")),
                                                        selectInput("manure_storage_report_graph","Do you wish to produce graphics",
                                                                    choices = c("Yes","No")),
                                                        textInput("manure_storage_report_name","Manure Storage Report Name: ",value = "manure_storage_report"),width = 12),
                                           sidebarPanel(checkboxGroupInput("handling_report","Handling Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("handling_report_name","Handling Report Name: ",
                                                                  value ="handling_report"),width = 6),
                                           sidebarPanel(checkboxGroupInput("separator_report","Separator Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("separator_report_name","Separator Report Name: ",
                                                                  value ="separator_report"),width = 6),
                                           sidebarPanel(checkboxGroupInput("storage_report","Storage Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("storage_report_name","Storage Report Name: ",
                                                                  value ="storage_report"),width = 6),
                                           sidebarPanel(checkboxGroupInput("manure_storage_summary_report","Manure Storage Summary Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("manure_storage_summary_report_name","Manure Storage Summary Report Name: ",
                                                                  value ="manure_storage_summary"),width = 6)),
                                  tabPanel("Life Cycle",
                                           sidebarPanel(selectInput("lifecycle_report_csv","Do you wish to produce csv files?",
                                                                    choices = c("Yes","No")),
                                                        selectInput("lifecycle_report_graph","Do you wish to produce graphics",
                                                                    choices = c("Yes","No")),
                                                        textInput("lifecycle_report_name","Life Cycle Report Name: ",value = "life_cycle_report"),width = 12),
                                           sidebarPanel(checkboxGroupInput("initialization_db_summary_report","Initialization Database Summary Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("initialization_db_summary_report_name","Initialization Database Summary Report Name: ",
                                                                  value ="initialization_db_summary"),width = 4),
                                           sidebarPanel(checkboxGroupInput("individual_animal_report","Individual Animal Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("individual_animal_report_name","Individual Animal Report Name: ",
                                                                  value ="individual_animal_report"),
                                                        numericInput("report_n_animals","Number of Animals: ", min = 1, value = 3, step = 1),width = 4),
                                           sidebarPanel(checkboxGroupInput("herd_report","Herd Report",
                                                                           choices = c("Produce csv", "Produce graphics")),
                                                        textInput("herd_report_name","Herd Report Name: ",
                                                                  value ="herd_report"),width = 4)
                                  )
                          ))
                 
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
    
    new_n_fields = reactive({input$n_fields})

    observeEvent(input$generate_fields, {
        
        for (j in 2:100){
            remove_id = sprintf('Field %i',j)
            removeTab("fields",remove_id)
        }

        if (new_n_fields() > 1){
            for (j in 2:new_n_fields()){
                add_id = sprintf("Field %i",j)
                appendTab("fields",tabPanel(add_id,
                                            tabsetPanel(
                                                tabPanel("Soil",
                                                         sidebarPanel(
                                                             numericInput(sprintf("profile_bulk_density_%i",j),"Bulk density of the soil for the entire depth:",
                                                                          0, 0),
                                                             numericInput(sprintf("CN2_%i",j), "Curve Number (SCS) represents surface runoff factor of water: ", 0, 0),
                                                             numericInput(sprintf("field_slope_%i",j), "Slope of an individual field (%/100): ", 0, 0),
                                                             numericInput(sprintf("slope_length_%i",j), "Length of slope: ", 0, 0),
                                                             numericInput(sprintf("manning_%i",j), "Mannings roughness coefficient for ground use type: ", 0, 0),
                                                             numericInput(sprintf("field_size_%i",j), "Size of individual field where slope was calculated: ", 0, 0),
                                                             numericInput(sprintf("practice_factor_%i",j), "Ratio of soil loss with a specific support practice to corresponding loss with up-and-down slope culture: ", 0, 0),
                                                             numericInput(sprintf("sand_%i",j), "Fraction of sand (%/100): ", 0, 0),
                                                             numericInput(sprintf("silt_%i",j), "Fraction of silt (%/100): ", 0, 0),
                                                             numericInput(sprintf("soil_albedo_%i",j), "Soil solar radiation absorbance factor: ", 0, 0),
                                                             numericInput(sprintf("initial_residue_%i",j), "Initial amount of soil residue (kg/ha): ", 0, 0),
                                                             numericInput(sprintf("fresh_N_mineral_rate_%i",j), "Nitrogen N mineralization rate from SWAT: ", 0, 0),
                                                             selectInput(sprintf("soil_cover_type_%i",j), "Soil Cover Type: ", c("Bare","Residue Cover","Grassed")),
                                                             sliderInput(sprintf("n_Layers_%i",j),"Select the number of soil layers and fill out the corresponding columns in the next table (any
                                                            unused columns can be left empty)", 1, 5, 3), width = 4
                                                         ),
                                                         sidebarPanel(matrixInput(
                                                             sprintf("soil_layers_%i",j),
                                                             value = soil_layers,
                                                             rows = list(
                                                                 extend = FALSE
                                                             ),
                                                             cols = list(
                                                                 names = TRUE
                                                             )
                                                         ),width = 8)),
                                                tabPanel("Crop",
                                                         sidebarPanel(checkboxGroupInput(sprintf("selected_crops_%i",j),"Select the desired crops and fill out the corresponding rows in the next table (any
                                                            unused rows can be left empty)",
                                                                                         choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                                     "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                                     "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                                     "Triticale","Winter Wheat")),width = 4),
                                                         sidebarPanel(matrixInput(
                                                             sprintf("crop_rotation_%i",j),
                                                             value = crop_rotation,
                                                             rows = list(
                                                                 extend = FALSE
                                                             ),
                                                             cols = list(
                                                                 names = TRUE
                                                             )
                                                         ),
                                                         width = 8)
                                                ),
                                                tabPanel("Field Management")
                                                
                                            )))
            }
        }
        old_n_fields <- new_n_fields()
    })
    # TODO: FINISH EXPORTING TO JSON AUTOMATICALLY   
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
    
    output$download_all_input <- downloadHandler(
        filename = function() {
            paste("my_RuFaS_input", Sys.Date(), ".txt")
        },
        content = function(file){
            inputsList = sort(names(reactiveValuesToList(input)))
            exportVars <- paste0(inputsList, "=", sapply(inputsList, function(inpt) input[[inpt]]))
            write(exportVars, file)
        }
    )
}
# Run the app ----
shinyApp(ui = ui, server = server)