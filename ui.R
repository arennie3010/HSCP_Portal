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
                      p("This dashboard contains a number of visualisations highlighting the monthly usage of Unscheduled Care Services across Scotland with focus on small area localities.", style = "text-align:center;"),
                      br(),
                      strong(p("To see HSCP level information, select 'Summary' in the Navigation Bar. To see Intermediate Zone information, select 'Data Explorer'.
                               For more information regarding the data, select 'Notes'.", style = "text-align:center;")),
                      br(),
                      img(src='MicrosoftTeams-image.png', style="display: block; margin-left: auto; margin-right: auto;")),
             
             ### SUMMARY TAB
             tabPanel("Summary",
                      fluidPage(
                        br(),
                          includeCSS(path = "www/AdminLTE.css"),
                          includeCSS(path = "www/shinydashboard.css"),
                          includeCSS(path = "www/font-awesome.css"),
                        br(),
                        sidebarPanel(style = "position:fixed;width:inherit;", width = 3, selectInput("selectHSCPsummary", "Select HSCP", choices = unique(iz.m$hscp), selected = "Glasgow City"),
                                     hr(),
                                     sliderInput("timeframesummary", 
                                                 "Month",
                                                 min = 1,
                                                 max = 12,
                                                 value = c(1,12),
                                                 #min = as.Date(start_date,"%Y-%m-%d"),
                                                 #max = as.Date(end_date,"%Y-%m-%d"),
                                                 #value=c(as.Date(start_date,"%Y-%m-%d"),
                                                 #         as.Date(end_date,"%Y-%m-%d")),
                                                 #timeFormat="%Y-%m-%d",
                                                 ticks = TRUE, step = 1,
                                                 dragRange = TRUE),
                                     radioButtons("select_indsummary", "Measure",  choices = measure_list, selected = "cases"),
                                     measure_tooltip_s,
                                     loc_tooltip_s,
                                     time_tooltip_s
                                     ),
                        mainPanel( box(width = 12, 
                          infoBox("A&E Cases (monthly average)", value=htmlOutput("aebox"), icon=icon("user-injured", lib = "font-awesome"), fill = TRUE, color = "purple"),
                          infoBox("Emergency Admissions (monthly average)", value=htmlOutput("eabox"), icon=icon("user-injured", lib = "font-awesome"), fill = TRUE, color = "purple"),
                         # infoBox("Non-Elective", paste(90, "cases"), icon=icon("hospital", lib = "font-awesome"), fill = TRUE, color = "purple"),
                          infoBox("SAS Records (monthly average)", value=htmlOutput("sasbox"), icon=icon("ambulance", lib = "font-awesome"), fill = TRUE, color = "purple"),
                          infoBox("GP OOH Cases (monthly average)", value=htmlOutput("oohbox"), icon=icon("clock", lib = "font-awesome"), fill = TRUE, color = "purple"),
                          infoBox("NHS24 Records (monthly average)", value=htmlOutput("nhs24box"), icon=icon("phone", lib = "font-awesome"), fill = TRUE, color = "purple")),

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
                          box(textOutput("text5"),  width = 3),
                          box(plotlyOutput("sc5"), width = 9)
                        )
                      )),
             
        ### MAIN DATA EXPLORE TAB
  tabPanel("Area Comparison",
  fluidPage(
             sidebarPanel(
               selectInput("selectHSCP", "Select HSCP", choices = unique(iz.m$hscp), selected = "Glasgow City"),
               hr(),
               selectInput("select_service", label = "Service", choices = source_list,
                           selected = "NHS24"),
               radioButtons("select_ind", "Measure",  choices = measure_list_2, selected = "cases"),
               sliderInput("timeframe", "Month",
                           min = 1,
                           max = 12,
                           value = c(1,12),
                           #min = as.Date(start_date,"%Y-%m-%d"),
                           #max = as.Date(end_date,"%Y-%m-%d"),
                           #value=c(as.Date(start_date,"%Y-%m-%d"),
                          #         as.Date(end_date,"%Y-%m-%d")),
                           #timeFormat="%Y-%m-%d",
                           ticks = TRUE, step = 1,
                           dragRange = TRUE),
               measure_tooltip_d,
               loc_tooltip_d,
               service_tooltip_d,
               time_tooltip_d
               ),
               
               # sliderInput("timeframe", "Date", min = dmy(start_date), max = dmy(end_date),
               #             ticks = TRUE, step = 1,  value = c(1,25),
               #             dragRange = TRUE))
             mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel("Map",
                                  leafletOutput("map_pois", height = "80vh")), #### changeleaflet output to "map_iz" to revert to old map - in testing at the moment
                         tabPanel("Heatchart",
                                  plotOutput("heatchart_pois", height = 900)
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
  
  # Annual change tab
  
  
  tabPanel("Previous Year Comparison",
           fluidPage(
             sidebarPanel(
               selectInput("selectHSCP_1", "Select HSCP", choices = unique(iz.m$hscp), selected = "Glasgow City"),
               hr(),
               selectInput("select_service_1", label = "Service", choices = source_list,
                           selected = "NHS24"),
            #   radioButtons("select_ind_1", "Measure",  choices = measure_list_2, selected = "cases"),
               sliderInput("timeframe_1", "Month",
                           min = 1,
                           max = 12,
                           value = c(1,12),
                           #min = as.Date(start_date,"%Y-%m-%d"),
                           #max = as.Date(end_date,"%Y-%m-%d"),
                           #value=c(as.Date(start_date,"%Y-%m-%d"),
                           #         as.Date(end_date,"%Y-%m-%d")),
                           #timeFormat="%Y-%m-%d",
                           ticks = TRUE, step = 1,
                           dragRange = TRUE),
               measure_tooltip_d,
               loc_tooltip_d,
               service_tooltip_d,
               time_tooltip_d
             ),
             
  
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Map",
                                    leafletOutput("map_iz", height = "80vh")), #### changeleaflet output to "map_iz" to revert to old map - in testing at the moment
                           tabPanel("Heatchart",
                                    plotOutput("heatchart_iz", height = 900)
                  
                           ))
               
             ))
  ),
  
  
  
  tabPanel("Create Report",
           inputPanel(
             selectInput("selectHSCPrmd", "Select HSCP", choices = unique(iz.m$hscp), selected = "Glasgow City"),
             checkboxGroupInput("select_service_rmd", label = "Service", 
                                choices = source_list,
                                selected = "A&E"),
             radioButtons("select_ind_rmd", "Measure",  choices = measure_list, selected = "cases"),
             sliderInput("timeframe_rmd", "Month",
                         min = 1,
                         max = 12,
                         value = c(1,12),
                         #min = as.Date(start_date,"%Y-%m-%d"),
                         #max = as.Date(end_date,"%Y-%m-%d"),
                         #value=c(as.Date(start_date,"%Y-%m-%d"),
                         #         as.Date(end_date,"%Y-%m-%d")),
                         #timeFormat="%Y-%m-%d",
                         ticks = TRUE, step = 1,
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
           strong(p("Background", style = "text-align:left;")),
           p("This dashboard is designed to provide an insight to the usage of a number of unscheduled care services.
             The three main measures include total number, rate per 1,000 population and annual change from the previous year.
             The figures are aggregated to monthly totals, at Intermediate Zone and HSCP level for NHS Scotland.", style = "text-align:left;"),
           strong(p("NHS24 Records", style = "text-align:left;")),
           p("Source: All data presented here is extracted from the Unscheduled Care datamart in Public Health Scotland.", style = "text-align:left;"),
           br(),
           strong(p("GP Out of Hours Records", style = "text-align:left;")),
           p("Source: All data presented here is extracted from the Out of Hours datamart (OOH). Data is recorded locally on a system called ADASTRA, which routinely hosts all OOH activity, and submitted daily to Public Health Scotland.", style = "text-align:left;"),
           br(),
           strong(p("Scottish Ambulance Service Records", style = "text-align:left;")),
           p("Source: All data presented here is extracted from the Unscheduled Care Datamart in Public Health Scotland. Scottish Ambulance Service data in the UCD includes incidents where a vehicle arrived on the scene of the incident,
             excluding data from resources which were cleared with a reason of 'dealt with by another vehicle' and air ambulance data. Scottish Ambulance Service provide an upload to the datamart each morning reflecting activity for two days previous.", style = "text-align:left;"),
           br(),
           strong(p("Accident and Emergency Admission Records", style = "text-align:left;")),
           p("Source: All data presented here is extracted from the A&E Data Mart in Public Health Scotland.
             Includes ED and MIU attendances only, does not include Assessent Unit attendances.", style = "text-align:left;"),
           br(),
           strong(p("Emergency Admissions", style = "text-align:left;")),
           p("Source: All data presented here is extracted from the ...", style = "text-align:left;")
)
  
  
  ) # navbarPage bracket

## END
)
