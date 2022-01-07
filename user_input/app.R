library(shiny)
library(jsonlite)
library(shinyMatrix)
library(shinythemes)
library(shinyBS)

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
rownames(soil_layers) <- c("Lower depth of soil layer (in cm)",
                           "Fractional value at which point soil water becomes plant unavailable (Dmnl)",
                           "Fractional value of soil water at which the soil matric potential is zero (Dmnl)",
                           "Saturated soil value (Dmnl)",
                           "saturated hydraulic conductivity (in mm/h)",
                           "Fraction of porosity from which anions are excluded (Dmnl)",
                           "Percent of clay per layer (%)",
                           "Soil temperature of each layer (in \u00B0 C)",
                           "Bulk density of the soil for the entire depth (in g/cm3)",
                           "Percentage of layer composed of organic carbon (%)",
                           "NH4 Initializer variable (in mg/Kg)",
                           "SWAT Factor (Dmnl)",
                           "Labile Phosphorus (in mg/Kg)",
                           "Active mineral rate (Dmnl)",
                           "Volatile exchange factor (Dmnl)",
                           "Dentrification rate (Dmnl)",
                           "Fraction of soil water in layer (Dmnl)",
                           "Fraction of soil organic matter (Dmnl)")

crop_rotation <- matrix("",5,3)
colnames(crop_rotation) <- c("Planting Date (in Julian day)",
                             "Harvest Date (in Julian Day)","Harvest Type (Scheduled or Optimal)")
rownames(crop_rotation) <- c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5")

feeds_ID <- c(1:34,38:52,56,57,61,65,69,73,77,81,85:87,91,92,96:157)

# Globals
field_count <- 1
pen_count <- 1
storage_options_count <- 1

# Define UI ----
ui <- navbarPage("RuFas Input", theme = shinytheme("darkly"), id = "main",
                 tabPanel("General Configuration",
                          sidebarPanel(
                            textInput("dataset_ID","Enter a name for your dataset: ", placeholder = "my_RuFaS_input"),
                            dateRangeInput("start_end_dates", "Date range:",
                                           max = Sys.Date(),
                                           format = "mm-dd-yyyy"),
                            textOutput("date"),
                            selectInput("testing","Do you wish to run tests?",
                                        choices = c("Yes", "No")),
                            selectInput("weather_dataset","Can you provide a complete weather dataset?",
                                        choices = c("No", "Yes")),
                            numericInput("lat", "Latitude (in \u00B0)", min = -90, max = 90, value = 0),
                            weather_tabs,
                            textOutput("coord"),
                            sliderInput("n_fields","Number of Fields: ",
                                        min = 1, max =100, value = 1),
                            actionButton("generate_fields","Generate Fields"),
                            width = 12
                          )),
                 
                 tabPanel("Fields",
                          tabsetPanel(id="fields",
                                      tabPanel("Field 1",
                                               tabsetPanel(
                                                 tabPanel("Soil",
                                                          sidebarPanel(
                                                            numericInput("profile_bulk_density_1","Bulk density of the soil for the entire depth (in g/cm3):",
                                                                         0, value = 1.3),
                                                            numericInput("CN2_1", "Curve Number (SCS) represents surface runoff factor of water (Dmnl): ", 0, value =85),
                                                            numericInput("field_slope_1", "Slope of an individual field (Dmnl): ", 0, value =0.02),
                                                            numericInput("slope_length_1", "Length of slope (in m): ", 0, value =3),
                                                            numericInput("manning_1", "Mannings roughness coefficient for ground use type (Dmnl): ", 0, value =0.4),
                                                            numericInput("field_size_1", "Size of individual field where slope was calculated (in Hectares): ", 0, value =1),
                                                            numericInput("practice_factor_1", "Ratio of soil loss with a specific support practice to corresponding loss with up-and-down slope culture (Dmnl): ", 0, value =0.08),
                                                            numericInput("sand_1", "Fraction of sand (Dmnl): ", 0, value =15),
                                                            numericInput("silt_1", "Fraction of silt (Dmnl): ", 0, value =65),
                                                            numericInput("soil_albedo_1", "Soil solar radiation absorbance factor (Dmnl): ", 0,value = 0.16),
                                                            numericInput("initial_residue_1", "Initial amount of soil residue (in kg/ha): ", 0, 0),
                                                            numericInput("fresh_N_mineral_rate_1", "Nitrogen N mineralization rate from SWAT (Dmnl): ", 0,value = 0.05),
                                                            selectInput("soil_cover_type_1", "Soil Cover Type: ", c("Bare","Residue Cover","Grassed")),
                                                            sliderInput("n_Layers_1","Select the number of soil layers (at least 3 layers are recommended for best results) and fill out the corresponding columns in the next table (any
                                                            unused columns can be left empty)", 1, 5, 3)            
                                                            , width = 4
                                                          ),
                                                          mainPanel(
                                                            uiOutput("soil_layers_1") # show the matrix
                                                          )),
                                                 tabPanel("Crop Rotation",
                                                          sidebarPanel(tags$h3("Build a 5-year crop rotation"),
                                                                       selectInput("year_1_crop_1","Select the desired crop for year 1:",
                                                                                   choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                               "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                               "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                               "Triticale","Winter Wheat")),
                                                                       selectInput("year_2_crop_1","Select the desired crop for year 2:",
                                                                                   choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                               "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                               "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                               "Triticale","Winter Wheat")),
                                                                       selectInput("year_3_crop_1","Select the desired crop for year 3:",
                                                                                   choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                               "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                               "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                               "Triticale","Winter Wheat")),
                                                                       selectInput("year_4_crop_1","Select the desired crop for year 4:",
                                                                                   choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                               "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                               "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                               "Triticale","Winter Wheat")),
                                                                       selectInput("year_5_crop_1","Select the desired crop for year 5:",
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
                                                 )
                                               ))))
                 ,
                 tabPanel("Animals",
                          sidebarPanel(
                            tabsetPanel(id = "animals",
                                        tabPanel("Herd Information",
                                                 numericInput("calf_num", "Number of calves randomly selected from initialization herd: ",
                                                              min = 0, value = 80),
                                                 numericInput("heiferI_num", "Number of heifers between weaning and first breeding randomly selected from initialization herd: ",
                                                              min = 0, value = 440),
                                                 numericInput("heiferII_num", "Number of heifers between first breeding and close to parturition randomly selected from the initialization herd : ",
                                                              min = 0, value = 380),
                                                 numericInput("heiferIII_num", "Number of heifers close to parturition randomly selected from the initialization herd: ",
                                                              min = 0, value = 50),
                                                 numericInput("cow_num", "Number of cows randomly selected from the initialization herd: ",
                                                              min = 0, value = 1000),
                                                 numericInput("replace_num", "Number of replacements: ",
                                                              min = 0, value = 5000), # TODO: check prompt
                                                 numericInput("herd_num", "Goal for number of cows in the herd: ",
                                                              min = 0, value = 1000),
                                                 selectInput("herd_init","Do you wish to simulate a replacement herd database? (Not recommended)",
                                                             choices = c("No", "Yes")),
                                                 selectInput("breed", "Breed of cattle in the simulation: ",
                                                             choices = c("HO (Holsteins)","JE (Jerseys)")),
                                                 selectInput("methane_model","Methane emissions model: ",
                                                             choices = c("IPCC","Mills","Mutian")),
                                                 sliderInput("n_pens","Number of pens",min = 0, max = 20, value =1),
                                                 actionButton("generate_pens","Generate Pens")),
                                        tabPanel("Management Decisions",
                                                 tags$h4("For all entries regarding reproduction protocols, please refer to the following documents: "),
                                                 tags$a(href = "https://www.dcrcouncil.org/wp-content/uploads/2019/04/Dairy-Cow-Protocol-Sheet-Updated-2018.pdf",
                                                        "Dairy Cow Protocol Sheet"),
                                                 tags$h1(""),
                                                 tags$a(href = "https://www.dcrcouncil.org/wp-content/uploads/2018/12/Dairy-Heifer-Protocol-Sheet-Updated-2018.pdf",
                                                        "Dairy Heifer Protocol Sheet"),
                                                 numericInput("breeding_start_day_h", "Target start days born of reproduction protocols: ",
                                                              min = 1, value = 360),
                                                 selectInput("heifer_repro_method", "Reproductive protocol for heifers: ",
                                                             choices = c("TAI","ED","synch-ED")),
                                                 selectInput("cow_repro_method", "Reproductive protocol for cows: ",
                                                             choices = c("TAI","ED","synch-ED")),
                                                 selectInput("semen_type", "Type of semen used in reproduction protocols: ",
                                                             choices = c("Conventional","Sexed")),
                                                 numericInput("days_in_preg_when_dry", "Days when the cow is dried off after parturition: ",
                                                              min = 1, value = 218),
                                                 selectInput("lactation_curve", "Model selection for milk production: ",
                                                             choices = c("Wood","Milkbot")),
                                                 numericInput("heifer_repro_cull_time", "Days old when a heifer would be culled if unsuccessful in breeding: ",
                                                              min = 1, value = 650),
                                                 numericInput("repro_cull_time", "Threshold of heifer culling age, when the heifer is not pregnant at this age, she will be culled for repro failure: ",
                                                              min = 1, value = 300),
                                                 numericInput("do_not_breed_time", "Days in pregnancy when reproduction protocols are stopped: ",
                                                              min = 1, value = 300),
                                                 numericInput("cull_milk_production", "Minimum daily milk production before animal is culled: (in kg/day)",
                                                              min = 0, value = 22),
                                                 sliderInput("cow_times_milked_per_day", "Number of times per day cows are milked: ",
                                                             min = 1, max = 4, value = 1)
                                        ),
                                        tabPanel("Farm Level",
                                                 sidebarPanel(
                                                   tags$h2("Fill out the required fields based on the reproduction protocols chosen in previous section: "),
                                                   numericInput("birth_weight_avg_ho","Average calf birth weight (in kg/head):",
                                                                min = 0, value = 43.9),
                                                   sliderInput("keep_female_calf_rate","The rate at which female calves are kept and raised on-farm",
                                                               min = 0, max = 1, value = 1, step = 0.01), width = 12,
                                                   numericInput("wean_day", "Day the calf is fully weaned from milk or milk replacer: ",
                                                                min = 0, value = 60),
                                                   numericInput("wean_length","Number of days it takes to wean a calf: ",
                                                                min = 0, value = 7),
                                                   selectInput("milk_type","Milk Type: ", choices = c("Milk","Milk Replacer")),
                                                   sliderInput("conception_rate_decrease","The decrease of the conception rate for later breeding",
                                                               min = 0, max = 0.1, value = 0.026, step = 0.001),
                                                   numericInput("avg_gestation_len","The average length of gestations (in days)",
                                                                min = 0, value = 278),
                                                   numericInput("std_gestation_len","The standard deviation of gestation length (in days)",
                                                                min = 0, value =6),
                                                   numericInput("mature_body_weight_avg","The average mature body weight of cows (in kg/head)",
                                                                min = 0, value = 740.1),
                                                   numericInput("mature_body_weight_std","The standard deviation of mature body weight of cows (in kg/head)",
                                                                min = 0, value = 73.5)
                                                 ),
                                                 sidebarPanel(bsCollapse(id = "farm_level",
                                                                         bsCollapsePanel(title = "-> If 'ED', 'synch-ED', or 'ED-TAI' is chosen for either heifer or cows",
                                                                                         sliderInput("estrus_detection_rate","Percentage of in heat animals that would be detected in the ED programs",
                                                                                                     min = 0, max = 1, value = 0.4, step = 0.01),
                                                                                         sliderInput("estrus_service_rate","Percentage of detected in heat animals that would be serviced inthe ED programs",
                                                                                                     min = 0, max = 1, value = 0.9, step = 0.01),
                                                                                         sliderInput("ed_conception_rate","Percentage of serviced animals that would be concepted in the ED programs",
                                                                                                     min = 0, max = 1, value = 0.4, step = 0.01),
                                                                                         numericInput("voluntary_waiting_period","Days after parturition when estrus detection followed by AI in ED programs",
                                                                                                      min = 0, value = 45), style = "default"),
                                                                         bsCollapsePanel(title = "-> If 'TAI' is chosen for heifers",
                                                                                         selectInput("heifer_TAI_protocol", "The protocol for heifer in TAI program: ",
                                                                                                     choices = c("dCG2P", "5dCGP")),
                                                                                         
                                                                                         bsCollapsePanel(title = "-> If '5dCG2P' is chosen for heifer TAI program",
                                                                                                         sliderInput("m5dCG2P_conception_rate","Conception rate for 5dCG2P protocol in heifer TAI program",
                                                                                                                     min = 0, max = 1, value = 0.6, step = 0.01)),
                                                                                         bsCollapsePanel(title = "-> If '5dCGP' is chosen for heifer TAI program",
                                                                                                         sliderInput("5dCGP_conception_rate","Conception rate for 5dCGP protocol in heifer TAI program",
                                                                                                                     min = 0, max = 1, value = 0.48, step = 0.01))
                                                                         ),
                                                                         bsCollapsePanel(title = "-> If 'synch-ED' is chosen for heifers",
                                                                                         selectInput("heifer_synchED_protocol", "The protocol for heifer in synch-ED program: ",
                                                                                                     choices = c("2P", "CP"))),
                                                                         bsCollapsePanel(title = "-> If 'TAI' is chosen for cows",
                                                                                         selectInput("cow_presynch_protocol", "The preSynch protocol (1st breeding) for cow in TAI program: ",
                                                                                                     choices = c("Double OvSynch", "Presynch", "G6G")),
                                                                                         selectInput("cow_TAI_protocol", "The timed AI protocol for cow in the TAI program: ",
                                                                                                     choices = c("OvSynch 56", "OvSynch 48", "CoSynch 72", "5d CoSynch")),
                                                                                         bsCollapsePanel(title = "-> If 'OvSynch 56' is chosen for cow TAI program",
                                                                                                         sliderInput("ovsynch56_conception_rate","Conception rate for OvSynch 56 protocol in cow TAI program",
                                                                                                                     min = 0, max = 1, value = 0.55, step = 0.01)),
                                                                                         bsCollapsePanel(title = "-> If 'OvSynch 48' is chosen for cow TAI program",
                                                                                                         sliderInput("ovsynch48_conception_rate","Conception rate for OvSynch 48 protocol in cow TAI program",
                                                                                                                     min = 0, max = 1, value = 0.4, step = 0.01)),
                                                                                         bsCollapsePanel(title = "-> If 'CoSynch 72' is chosen for cow TAI program",
                                                                                                         sliderInput("cosynch72_conception_rate","Conception rate for CoSynch 72 protocol in cow TAI program",
                                                                                                                     min = 0, max = 1, value = 0.4, step = 0.01)),
                                                                                         bsCollapsePanel(title = "-> If '5d CoSynch' is chosen for cow TAI program",
                                                                                                         sliderInput("cosynch5d_conception_rate","Conception rate for 5d CoSynch protocol in cow TAI program",
                                                                                                                     min = 0, max = 1, value = 0.4, step = 0.01)),
                                                                                         selectInput("cow_resynch_protocol","The resynch protocol for cow that is diagnosed open at pregnancy in the TAI program: ",
                                                                                                     choices = c("TAIafterPD", "TAIbeforePD", "PGFat PD")),
                                                                                         numericInput("voluntary_waiting_period","Days after parturition when the 1st hormonal injection is made in TAI programs:",
                                                                                                      min = 0, value = 45)),
                                                                         bsCollapsePanel(title = "-> If 'ED-TAI' is chosen for cows",
                                                                                         selectInput("cow_presynch_protocol", "The preSynch protocol (1st breeding) for cow in ED-TAI program: ",
                                                                                                     choices = c("Double OvSynch", "Presynch", "G6G")),
                                                                                         selectInput("cow_TAI_protocol", "The timed AI protocol for cow in the ED-TAI program: ",
                                                                                                     choices = c("OvSynch 56", "OvSynch 48", "CoSynch 72", "5d CoSynch")),
                                                                                         bsCollapsePanel(title = "-> If 'OvSynch 56' is chosen for cow ED-TAI program",
                                                                                                         sliderInput("ovsynch56_conception_rate","Conception rate for OvSynch 56 protocol in cow ED-TAI program",
                                                                                                                     min = 0, max = 1, value = 0.55, step = 0.01)),
                                                                                         bsCollapsePanel(title = "-> If 'OvSynch 48' is chosen for cow ED-TAI program",
                                                                                                         sliderInput("ovsynch48_conception_rate","Conception rate for OvSynch 48 protocol in cow ED-TAI program",
                                                                                                                     min = 0, max = 1, value = 0.4, step = 0.01)),
                                                                                         bsCollapsePanel(title = "-> If 'CoSynch 72' is chosen for cow ED-TAI program",
                                                                                                         sliderInput("cosynch72_conception_rate","Conception rate for CoSynch 72 protocol in cow ED-TAI program",
                                                                                                                     min = 0, max = 1, value = 0.4, step = 0.01)),
                                                                                         bsCollapsePanel(title = "-> If '5d CoSynch' is chosen for cow ED-TAI program",
                                                                                                         sliderInput("cosynch5d_conception_rate","Conception rate for 5d CoSynch protocol in cow ED-TAI program",
                                                                                                                     min = 0, max = 1, value = 0.4, step = 0.01)),
                                                                                         selectInput("cow_resynch_protocol","The resynch protocol for cow that is diagnosed open at pregnancy in the ED-TAI program: ",
                                                                                                     choices = c("TAIafterPD", "TAIbeforePD", "PGFat PD")),
                                                                                         numericInput("voluntary_waiting_period","Days after parturition when estrus detection followed by AI in ED-TAI programs:",
                                                                                                      min = 0, value = 45),
                                                                                         numericInput("tai_program_start_day","Days after parturition when TAI protocol starts in ED-TAI programs",
                                                                                                      min = 0, value = 72),
                                                                                         textOutput("TAI_waiting_period"))
                                                                         
                                                 ),width = 12),
                                                 
                                        ),
                                        tabPanel("Pen 1",
                                                 textInput("pen_id_1", "Pen name", placeholder = "pen1"),
                                                 numericInput("vertical_dist_to_milking_parlor_1", "Vertical distance to milking parlor (in meters):",
                                                              min = 0, value = 0.1),
                                                 numericInput("horizontal_dist_to_milking_parlor_1", "Horizontal distance to milking parlor (in meters):",
                                                              min = 0, value = 1.6),
                                                 numericInput("number_of_stalls_1","Number of stalls:", min = 0, value = 100),
                                                 selectInput("pen_type_1","Pen type: ", choices = c("Free Stall","Tie Stall")),
                                                 selectInput("bedding_type_1","Bedding type: ", choices = c("Organic","Sand")),
                                                 selectInput("manure_handling_1","Manure handling: ", choices = c("Scraping System","Flush System")),
                                                 selectInput("manure_separator_1","Manure separator: ", choices = c("Sedimentation","Rotary Screen")),
                                                 selectInput("manure_storage_1","Manure storage: ", choices = c("Storage Pit","Storage Pond","Anaerobic Lagoon"))
                                        )), width = 12)),
                 tabPanel("Feed",
                          tabsetPanel(id = "feeds",
                                      tabPanel("Feeds Selection",
                                               sidebarPanel(
                                                 tags$h4("Select the purchased feeds from the feed library below: "),
                                                 tags$a(href = "https://docs.google.com/spreadsheets/d/1IfZdBPxKjYVM4XvAeUTsG61H1xbg6N7ib0AqM_Em_Mo/edit#gid=0",
                                                        "Feeds Library")
                                                 ,selectInput("purchased_feeds", "Purchased Feeds: ",
                                                              choices = feeds_ID ,multiple = TRUE),
                                                 selectInput("growing_feeds","Growing Feeds",
                                                             choices = feeds_ID, multiple = TRUE),
                                                 sliderInput("n_storage_options","Number of storage options",
                                                             min = 1, max = 5, value = 1),
                                                 actionButton("generate_storage_options","Generate Storage Options"), width = 12)),
                                      tabPanel("Storage Option 1",
                                               sidebarPanel(
                                                 selectInput("storage_type_1","Ensiling method used to store the feeds:",
                                                             choices = c("Bag","Tower","Bunk","Pile")),
                                                 selectInput("moisture_1","Storage system as adopted in the loss reduction table:",
                                                             choices = c("Direct Cut","Wilted","Baleage","Haylage","Moist Hay","Dry Hay")),
                                                 selectInput("additive_1","Type of preservative used for the silage: ",
                                                             choices = c("Preservative")),
                                                 numericInput("packing_density_1","Stocking density of the storage system:",min = 0 , value = 14),
                                                 selectInput("inoculation_1","Type of lactic acid bacteria (LAB) inoculants: ",
                                                             choices = c("Homofermentative","Heterofermentative ")),
                                                 selectInput("bunk_type_1","Bunk Type:",choices = c("Open floor")),
                                                 selectInput("ventilation_1","Should the storage space be ventilated ?",choices = c("Yes","No")),
                                                 numericInput("removal_rate_1","Removal rate", min = 0, value =6),
                                                 numericInput("initial_dry_matter_1","Initial dry matter of the cut feeds (in kg)", min = 0, value = 0), width = 12
                                               )))),
                 tabPanel("Output",
                          tabsetPanel(
                            tabPanel("Pens",
                                     sidebarPanel(selectInput("pens_report_csv","Do you wish to produce csv files?",
                                                              choices = c("Yes","No")),
                                                  selectInput("pens_report_graph","Do you wish to produce graphics?",
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
                                                  selectInput("fields_report_graph","Do you wish to produce graphics?",
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
                                                  selectInput("mass_balance_report_graph","Do you wish to produce graphics?",
                                                              choices = c("Yes","No")),
                                                  textInput("mass_balance_report_name","Mass Balance Report Name: ",value = "mass_balance_report"),width = 12)),
                            tabPanel("Feed Storage",
                                     sidebarPanel(selectInput("feed_storage_report_csv","Do you wish to produce csv files?",
                                                              choices = c("Yes","No")),
                                                  selectInput("feed_storage_report_graph","Do you wish to produce graphics?",
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
                                                  selectInput("manure_storage_report_graph","Do you wish to produce graphics?",
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
                                                  selectInput("lifecycle_report_graph","Do you wish to produce graphics?",
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
                          )),
                 tabPanel("Download",
                          sidebarPanel(
                            downloadLink("download_all_input","Submit")),
                          sidebarPanel(
                            downloadLink("download_animal_json","Download Animal JSON File")
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
  

  output$soil_layers_1 <- renderUI({
    
    # setting up the rows - variables
    
    row_variables <- c("Lower depth of soil layer (in cm)",
                       "Fractional value at which point soil water becomes plant unavailable (Dmnl)",
                       "Fractional value of soil water at which the soil matric potential is zero (Dmnl)",
                       "Saturated soil value (Dmnl)",
                       "saturated hydraulic conductivity (in mm/h)",
                       "Fraction of porosity from which anions are excluded (Dmnl)",
                       "Percent of clay per layer (%)",
                       "Soil temperature of each layer (in \u00B0 C)",
                       "Bulk density of the soil for the entire depth (in g/cm3)",
                       "Percentage of layer composed of organic carbon (%)",
                       "NH4 Initializer variable (in mg/Kg)",
                       "SWAT Factor (Dmnl)",
                       "Labile Phosphorus (in mg/Kg)",
                       "Active mineral rate (Dmnl)",
                       "Volatile exchange factor (Dmnl)",
                       "Dentrification rate (Dmnl)",
                       "Fraction of soil water in layer (Dmnl)",
                       "Fraction of soil organic matter (Dmnl)")
    
    # creating the matrix
    
    matrix_layer <- matrix(" ", ncol = input$n_Layers_1, nrow = length(row_variables))
    
    # renaming the rows
    
    rownames(matrix_layer) <- row_variables
    
    # setting up the number of layers
    
    n_layers <- input$n_Layers_1
    
    vector_layers <- vector()
    
    for (i in 1:n_layers){
      
      vector_layers[i] <- paste("Layer", i)
    }
    
    # renaming the columns
    
    colnames(matrix_layer) <- vector_layers
    
    # matrixInput
    
    matrixInput("soil_layers_1", "Soil Layers", matrix_layer, rows = list(names = TRUE), cols = list(names = TRUE))
    
  })  
  
  
  output$date <- renderText({
    dates()
  })
  
  repro <- reactive({
    shiny::validate(
      need(input$voluntary_waiting_period <= input$tai_program_start_day , " 'Days after parturition when estrus detection followed by AI in ED-TAI programs' should be less than or equal to 
                 'Days after parturition when TAI protocol starts in ED-TAI programs'")
    )
  })
  
  output$TAI_waiting_period <- renderText({
    repro()
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
  new_n_pens = reactive({input$n_pens})
  new_n_storage = reactive({input$n_storage_options})
  
  observeEvent(input$generate_storage_options, {
    n_storage_options <- new_n_storage()
    if (n_storage_options < storage_options_count) {
      start <- n_storage_options + 1
      for (j in start:storage_options_count){
        remove_id = sprintf('Storage Option %i',j)
        removeTab("feeds", remove_id)
      }
      storage_options_count <<- n_storage_options
    }
    
    
    if (n_storage_options > storage_options_count) {
      start <- storage_options_count + 1
      for (j in start:n_storage_options){
        add_id = sprintf("Storage Option %i",j)
        appendTab("feeds",tabPanel(add_id,
                                   sidebarPanel(
                                     selectInput(sprintf("storage_type_%i",j),"Ensiling method used to store the feeds:",
                                                 choices = c("Bag","Tower","Bunk","Pile")),
                                     selectInput(sprintf("moisture_%i",j),"Storage system as adopted in the loss reduction table:",
                                                 choices = c("Direct Cut","Wilted","Baleage","Haylage","Moist Hay","Dry Hay")),
                                     selectInput(sprintf("additive_%i",j),"Type of preservative used for the silage: ",
                                                 choices = c("Preservative")),
                                     numericInput(sprintf("packing_density_%i",j),"Stocking density of the storage system:",min = 0 , value = 14),
                                     selectInput(sprintf("inoculation_%i",j),"Type of lactic acid bacteria (LAB) inoculants: ",
                                                 choices = c("Homofermentative","Heterofermentative ")),
                                     selectInput(sprintf("bunk_type_%i",j),"Bunk Type:",choices = c("Open floor")),
                                     selectInput(sprintf("ventilation_%i",j),"Should the storage space be ventilated ?",choices = c("Yes","No")),
                                     numericInput(sprintf("removal_rate_%i",j),"Removal rate", min = 0, value =6),
                                     numericInput(sprintf("initial_dry_matter_%i",j),"Initial dry matter of the cut feeds (in kg)", min = 0, value = 0), width = 12)))
      }
      storage_options_count <<- n_storage_options
    }
  })
  
  observeEvent(input$generate_pens, { #TODO: delete hard coded value 10
    n_pens <- new_n_pens()
    if (n_pens < pen_count) {
      start <- n_pens + 1
      for (j in start:pen_count){
        remove_id = sprintf('Pen %i',j)
        removeTab("animals",remove_id)
      }
      pen_count <<- n_pens
    }
    
    if (n_pens > pen_count){
      start <- pen_count + 1
      for (j in start:n_pens){
        add_id = sprintf('Pen %i',j)
        appendTab("animals",tabPanel(add_id,
                                     textInput(sprintf("pen_id_%i",j), "Pen name", placeholder = sprintf("pen%i",j)),
                                     numericInput(sprintf("vertical_dist_to_milking_parlor_%i",j), "Vertical distance to milking parlor (in meters):",
                                                  min = 0, value = 0.1),
                                     numericInput(sprintf("horizontal_dist_to_milking_parlor_%i",j), "Horizontal distance to milking parlor (in meters):",
                                                  min = 0, value = 1.6),
                                     numericInput(sprintf("number_of_stalls_%i",j),"Number of stalls:", min = 0, value = 100),
                                     selectInput(sprintf("pen_type_%i",j),"Pen type: ", choices = c("Free Stall","Tie Stall")),
                                     selectInput(sprintf("bedding_type_%i",j),"Bedding type: ", choices = c("Organic","Sand")),
                                     selectInput(sprintf("manure_handling_%i",j),"Manure handling: ", choices = c("Scraping System","Flush System")),
                                     selectInput(sprintf("manure_separator_%i",j),"Manure separator: ", choices = c("Sedimentation","Rotary Screen")),
                                     selectInput(sprintf("manure_storage_%i",j),"Manure storage: ", choices = c("Storage Pit","Storage Pond","Anaerobic Lagoon"))))
      }
      pen_count <<-n_pens
    }
  })
  
  observeEvent(input$generate_fields, { #TODO: delete hard coded value 100
    n_fields <- new_n_fields()
    if (n_fields < field_count) {
      start <- n_fields + 1
      for (j in start: field_count){
        remove_id = sprintf('Field %i',j)
        removeTab("fields",remove_id)
      }
      field_count <<- n_fields
    }
    if (n_fields > field_count){
      start <- field_count + 1
      print(n_fields)
      print("start_loop")
      for (j in start : n_fields){
        print(j)
        add_id = sprintf("Field %i",j)
        appendTab("fields",tabPanel(add_id,
                                    tabsetPanel(
                                      tabPanel("Soil",
                                               sidebarPanel(
                                                 numericInput(sprintf("profile_bulk_density_%i",j),"Bulk density of the soil for the entire depth (in g/cm3):",
                                                              0,value = 1.3),
                                                 numericInput(sprintf("CN2_%i",j), "Curve Number (SCS) represents surface runoff factor of water (Dmnl): ", 0,value = 85),
                                                 numericInput(sprintf("field_slope_%i",j), "Slope of an individual field (Dmnl): ", 0,value = 0.02),
                                                 numericInput(sprintf("slope_length_%i",j), "Length of slope (in m): ", 0,value = 3),
                                                 numericInput(sprintf("manning_%i",j), "Mannings roughness coefficient for ground use type (Dmnl): ", 0,value = 0.4),
                                                 numericInput(sprintf("field_size_%i",j), "Size of individual field where slope was calculated (in Hectares): ", 0,value = 1),
                                                 numericInput(sprintf("practice_factor_%i",j), "Ratio of soil loss with a specific support practice to corresponding loss with up-and-down slope culture (Dmnl): ", 0,value = 0.08),
                                                 numericInput(sprintf("sand_%i",j), "Fraction of sand (Dmnl): ", 0,value = 15),
                                                 numericInput(sprintf("silt_%i",j), "Fraction of silt (Dmnl): ", 0,value = 65),
                                                 numericInput(sprintf("soil_albedo_%i",j), "Soil solar radiation absorbance factor (Dmnl): ", 0,value = 0.16),
                                                 numericInput(sprintf("initial_residue_%i",j), "Initial amount of soil residue (in kg/ha): ", 0,value = 0),
                                                 numericInput(sprintf("fresh_N_mineral_rate_%i",j), "Nitrogen N mineralization rate from SWAT (Dmnl): ", 0,value = 0.05),
                                                 selectInput(sprintf("soil_cover_type_%i",j), "Soil Cover Type: ", c("Bare","Residue Cover","Grassed")),
                                                 sliderInput(sprintf("n_Layers_%i",j),"Select the number of soil layers (at least 3 layers are recommended for best results) and fill out the corresponding columns in the next table (any
                                                            unused columns can be left empty)", 1, 5, 3), width = 4
                                               ),
                                               mainPanel(
                                                 uiOutput(sprintf("soil_layers_%i",j)) # show the matrix
                                               )),
                                      tabPanel("Crop Rotation",
                                               sidebarPanel(tags$h3("Build a 5-year crop rotation"),
                                                            selectInput(sprintf("year_1_crop_%i",j),"Select the desired crop for year 1:",
                                                                        choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                    "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                    "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                    "Triticale","Winter Wheat")),
                                                            selectInput(sprintf("year_2_crop_%i",j),"Select the desired crop for year 2:",
                                                                        choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                    "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                    "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                    "Triticale","Winter Wheat")),
                                                            selectInput(sprintf("year_3_crop_%i",j),"Select the desired crop for year 3:",
                                                                        choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                    "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                    "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                    "Triticale","Winter Wheat")),
                                                            selectInput(sprintf("year_4_crop_%i",j),"Select the desired crop for year 4:",
                                                                        choices = c("Alfalfa","Cereal Rye","Corn",
                                                                                    "Fall Oats","Potato","Soybean","Spring Barley",
                                                                                    "Spring Wheat","Sugar Beet","Tall Fescue",
                                                                                    "Triticale","Winter Wheat")),
                                                            selectInput(sprintf("year_5_crop_%i",j),"Select the desired crop for year 5:",
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
                                      )
                                      
                                    )))
      }
      field_count <<- n_fields
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
        },'
            ,calf_num(),heiferI_num(),heiferII_num(),heiferIII_num(),
            cow_num(),replace_num(),herd_num(),herd_init(),breed(),breeding_start_day_h(),
            heifer_repro_method(),cow_repro_method(),semen_type(),days_in_preg_when_dry(),
            lactation_curve(),heifer_repro_cull_time(),repro_cull_time(),do_not_breed_time(),
            cull_milk_production(),cow_times_milked_per_day())
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