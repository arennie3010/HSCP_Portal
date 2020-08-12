# Description - Describe what the app does (e.g. visualizes births data)
# User interface - how your app looks and elements users can interact with



shinyUI(
  navbarPage(title = "Unscheduled Care - HSCP Portal",
             tabPanel("About",
                      ### Add info about Portal
                      h3("HSCP Portal Concept Dashboard", style = "text-align:center;"),
                      hr(),
                      p("This concept dashboard showcases data visualisation tools through Shiny to explore Unscheduled Care data.", style = "text-align:center;"),
                      br(),
                      strong(p("To see HSCP level information, select 'Summary' in the Navigation Bar. To see Intermediate Zone information, select 'Data Explorer.", style = "text-align:center;"),
                      br())),
             
             ### SUMMARY TAB
             tabPanel("Summary",
                      fluidPage(
                          includeCSS(path = "www/AdminLTE.css"),
                          includeCSS(path = "www/shinydashboard.css"),
                          includeCSS(path = "www/font-awesome.css"),
                        sidebarPanel(style = "position:fixed;width:inherit;", width = 3, selectInput("selectHSCPsummary", "Select HSCP", choices = unique(iz$hscp), selected = "Glasgow City"),
                                     hr(),
                                     sliderInput("timeframesummary", "Weeks", min = 18, max = 32, 
                                                 ticks = TRUE, step = 1,  value = c(18,32),
                                                 dragRange = TRUE),
                                     radioButtons("select_indsummary", "Cases/Rate",  choices = c("cases","rate", "% change"), selected = "cases")),
                        mainPanel( box(width = 12, 
                          infoBox("A&E Attendances", paste(20, "cases"), icon=icon("user-injured", lib = "font-awesome"), fill = TRUE),
                          infoBox("Non-Elective", paste(20, "cases"), icon=icon("hospital", lib = "font-awesome"), fill = TRUE),
                          infoBox("SAS", paste(20, "cases"), icon=icon("ambulance", lib = "font-awesome"), fill = TRUE),
                          infoBox("GP OOH", paste(20, "cases"), icon=icon("clock", lib = "font-awesome"), fill = TRUE),
                          infoBox("NHS 24", paste(20, "cases"), icon=icon("phone", lib = "font-awesome"), fill = TRUE),
                          infoBox("Deaths", paste(20, "cases"), icon=icon("user", lib = "font-awesome"), fill = TRUE)),
                          
                          box(plotlyOutput("sc1"), width = 9),
                          br(),
                          box(textOutput("text1"), width = 3),
                          br(),
                          box(h3(plotlyOutput("sc2"), width = 9)),
                          br(),
                          box(textOutput("text2"),  width = 3),
                          br(),
                          box(plotlyOutput("sc3"), width = 9),
                          box(textOutput("text3"),  width = 3)
                          
                          
                        )
                      )),
             
             ### MAIN DATA EXPLORE TAB
  tabPanel("Data Explorer",
  fluidPage(
             sidebarPanel(
               selectInput("selectHSCP", "Select HSCP", choices = unique(iz$hscp), selected = "Glasgow City"),
               hr(),
               selectInput("select_service", label = "Indicator", 
                           choices = list("A&E" = "A&E", 
                                          "NHS24" = "NHS24",
                                          "GP OOH" = "OOH"), 
                           selected = "NHS24"),
               radioButtons("select_ind", "Number of Cases/Rate (per 1,000 population)",  choices = c("cases","rate"), selected = "cases"),
               sliderInput("timeframe", "Weeks", min = 18, max = 32, 
                           ticks = TRUE, step = 1,  value = c(18,32),
                           dragRange = TRUE)),
               
               # sliderInput("timeframe", "Date", min = dmy(start_date), max = dmy(end_date),
               #             ticks = TRUE, step = 1,  value = c(1,25),
               #             dragRange = TRUE))
             mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel("Map",
                                  leafletOutput("map_iz")),
                         tabPanel("Heatchart",
                                  plotOutput("heatchart_iz", height = "2000px")
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
  )# tabPanel bracket
) # navbarPage bracket

## END
)
