# Description - Describe what the app does (e.g. visualizes births data)
# User interface - how your app looks and elements users can interact with

# test commentary


shinyUI(
  
  navbarPage(title = "Unscheduled Care - HSCP Portal", 
             #windowTitle = div(
            #   div(
            #     id = "img-id",
            #     img(src = "MicrosoftTeams-image.png")
            #   )),

             
             
             tabPanel("About",
                      ### Add info about Portal
                      h3("HSCP Portal Concept Dashboard", style = "text-align:center;"),
                      hr(),
                      p("This concept dashboard showcases data visualisation tools through Shiny to explore Unscheduled Care data.", style = "text-align:center;"),
                      br(),
                      strong(p("To see HSCP level information, select 'Summary' in the Navigation Bar. To see Intermediate Zone information, select 'Data Explorer'.", style = "text-align:center;"),
                      br()),
                      img(src='MicrosoftTeams-image.png', style="display: block; margin-left: auto; margin-right: auto;")),
             
             ### SUMMARY TAB
             tabPanel("Summary",
                      fluidPage(
                        br(),
                          includeCSS(path = "www/AdminLTE.css"),
                          includeCSS(path = "www/shinydashboard.css"),
                          includeCSS(path = "www/font-awesome.css"),
                        br(),
                        sidebarPanel(style = "position:fixed;width:inherit;", width = 3, selectInput("selectHSCPsummary", "Select HSCP", choices = unique(iz$hscp), selected = "Glasgow City"),
                                     hr(),
                                     sliderInput("timeframesummary", "Weeks", min = 18, max = 30, 
                                                 ticks = TRUE, step = 1,  value = c(18,30),
                                                 dragRange = TRUE),
                                     radioButtons("select_indsummary", "Measure",  choices = measure_list, selected = "cases")),
                        mainPanel( box(width = 12, 
                          infoBox("A&E (weekly average)", value=htmlOutput("aebox"), icon=icon("user-injured", lib = "font-awesome"), fill = TRUE, color = "purple"),
                         # infoBox("Non-Elective", paste(90, "cases"), icon=icon("hospital", lib = "font-awesome"), fill = TRUE, color = "purple"),
                          infoBox("SAS (weekly average)", value=htmlOutput("sasbox"), icon=icon("ambulance", lib = "font-awesome"), fill = TRUE, color = "purple"),
                          infoBox("GP OOH (weekly average)", value=htmlOutput("oohbox"), icon=icon("clock", lib = "font-awesome"), fill = TRUE, color = "purple"),
                          infoBox("NHS24 records (weekly average)", value=htmlOutput("nhs24box"), icon=icon("phone", lib = "font-awesome"), fill = TRUE, color = "purple")),

                          box(plotlyOutput("sc1"), width = 9),
                          br(),
                          box(textOutput("text1"), width = 3),
                          br(),
                          box(plotlyOutput("sc2"), width = 9),
                          br(),
                          box(textOutput("text2"),  width = 3),
                          br(),
                          box(plotlyOutput("sc3"), width = 9),
                          br(),
                          box(textOutput("text3"),  width = 3),
                          br(),
                          box(plotlyOutput("sc4"), width = 9),
                          br(),
                          box(textOutput("text4"),  width = 3)
                          
                          
                          
                        )
                      )),
             
             ### MAIN DATA EXPLORE TAB
  tabPanel("Data Explorer",
  fluidPage(
             sidebarPanel(
               selectInput("selectHSCP", "Select HSCP", choices = unique(iz$hscp), selected = "Glasgow City"),
               hr(),
               selectInput("select_service", label = "Indicator", choices = source_list,
                           selected = "NHS24"),
               radioButtons("select_ind", "Measure",  choices = measure_list, selected = "cases"),
               sliderInput("timeframe", "Weeks", min = 18, max = 30, 
                           ticks = TRUE, step = 1,  value = c(18,30),
                           dragRange = TRUE)),
               
               # sliderInput("timeframe", "Date", min = dmy(start_date), max = dmy(end_date),
               #             ticks = TRUE, step = 1,  value = c(1,25),
               #             dragRange = TRUE))
             mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel("Map",
                                  leafletOutput("map_iz", height = 800)),
                         tabPanel("Heatchart",
                                  plotOutput("heatchart_iz", height = 800)
                                  # tags$div("Loading...", id = "loadmessage"),
                                  # tags$script(
                                  #   HTML(
                                  #     paste0("$(document).on('shiny:busy', function(event) {",
                                  #            "$('#loadmessage').css('display', 'inline');",
                                  #            "});",
                                  #            "$(document).on('shiny:idle', function(event) {",
                                  #            "$('#loadmessage').css('display', 'none');",
                                  #            "});"
                                  #   ))
                                  # )
                                  ),
                         
                         tabPanel("Data Table",
                                  dataTableOutput("datatable_iz")))
  
  ))
  ),
  tabPanel("Create Report",
           inputPanel(
             selectInput("selectHSCPrmd", "Select HSCP", choices = unique(iz$hscp), selected = "Glasgow City"),
             checkboxGroupInput("select_service_rmd", label = "Indicator", 
                                choices = source_list,
                                selected = "A&E"),
             radioButtons("select_ind_rmd", "Number of Cases/Rate (per 1,000 population)",  choices = measure_list, selected = "cases"),
             sliderInput("timeframe_rmd", "Weeks", min = 18, max = 30, 
                         ticks = TRUE, step = 1,  value = c(18,30),
                         dragRange = TRUE),
             downloadButton("RMD", "Knit Report")),
           fluidRow(
             
             # create plots for user selected services
             column(conditionalPanel(condition = "'A&E' %in% input.select_service_rmd",
                              plotOutput("ae_plot_rmd")),width = 4),
             column(conditionalPanel(condition = "'EA' %in% input.select_service_rmd",
                                     plotOutput("EA_plot_rmd")), width = 4),
             column(conditionalPanel(condition = "'NHS24' %in% input.select_service_rmd",
                              plotOutput("nhs24_plot_rmd")), width = 4),
             column(conditionalPanel(condition = "'SAS' %in% input.select_service_rmd",
                                     plotOutput("SAS_plot_rmd")), width = 4),
             column(conditionalPanel(condition = "'GP OOH' %in% input.select_service_rmd",
                              plotOutput("gpooh_plot_rmd")), width = 4)
             
             
           )
  ),  # tabPanel bracket
  tabPanel("Notes",
           ### Add info about Portal
           h3("HSCP Portal Concept Dashboard - Notes", style = "text-align:left;"),
           hr(),
           p("This tab will show information regarding the timeliness of the data as well as important details around it", style = "text-align:left;"),
           br(),
           strong(p("NHS24 Records", style = "text-align:left;")),
           p("*insert defintiion of NHS24 records included*", style = "text-align:left;"),
           br(),
           strong(p("GP Out of Hours Records", style = "text-align:left;")),
           p("The statistics included cover patients attending Out of Hours (OOH) Primary Care services. Patients attend these services when their registered GP Practices are closed and they need urgent care.", style = "text-align:left;"),
           p("A single case may be made up of multiple consultations.", style = "text-align:left;"),
           br(),
           strong(p("Scottish Ambulance Service Records", style = "text-align:left;")),
           p("*insert defintiion of NHS24 records included*", style = "text-align:left;"),
           br(),
           strong(p("Accident and Emergency Admission Records", style = "text-align:left;")),
           p(" The statistics included cover attendances to all A&E services and are derived from the A&E datamart which includes more detailed information.", style = "text-align:left;"),
           p("Includes ED and MIU attendances only, does not include Assessent Unit attendances.", style = "text-align:left;")
)
  
  
  ) # navbarPage bracket

## END
)
