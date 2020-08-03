# Description - Describe what the app does (e.g. visualizes births data)
# User interface - how your app looks and elements users can interact with



shinyUI(fluidPage(
  
  titlePanel("Unscheduled Care - HSCP Portal"),
  
  navlistPanel(
    widths = c(2,10),
    "Select HSCP:",
    tabPanel("East Dunbartonshire",
             inputPanel(
               radioButtons("select_ind", "COVID19/Total Cases",  choices = c("covid","total"), selected = "covid"),
               selectInput("select_service", label = "Indicator", 
                           choices = list("Unplanned Admissions" = "ea",
                                          "A&E" = "ae", 
                                          "NHS24" = "nhs24",
                                          "GP OOH" = "ooh",
                                          "SAS" = "sas",
                                          "ECOSS" = "ecoss",
                                          "Deaths" = "deaths"), 
                           selected = "ecoss")),
               sliderInput("timeframe", "Weeks", min = 1, max = 25, 
                           ticks = TRUE, step = 1,  value = c(1,25),
                           dragRange = TRUE)),
    tabPanel("East Renfrewshire",
             inputPanel(
               radioButtons("select_ind", "COVID19/Total Cases",  choices = c("covid","total"), selected = "covid"),
               selectInput("select_service", label = "Indicator", 
                           choices = list("Unplanned Admissions" = "ea",
                                          "A&E" = "ae", 
                                          "NHS24" = "nhs24",
                                          "GP OOH" = "ooh",
                                          "SAS" = "sas",
                                          "ECOSS" = "ecoss",
                                          "Deaths" = "deaths"), 
                           selected = "ecoss")),
             sliderInput("timeframe", "Weeks", min = 1, max = 25, 
                         ticks = TRUE, step = 1,  value = c(1,25),
                         dragRange = TRUE)),
    tabPanel("Glasgow City",
             inputPanel(
               radioButtons("select_ind", "COVID19/Total Cases",  choices = c("covid","total"), selected = "covid"),
               selectInput("select_service", label = "Indicator", 
                           choices = list("Unplanned Admissions" = "ea",
                                          "A&E" = "ae", 
                                          "NHS24" = "nhs24",
                                          "GP OOH" = "ooh",
                                          "SAS" = "sas",
                                          "ECOSS" = "ecoss",
                                          "Deaths" = "deaths"), 
                           selected = "ecoss")),
             sliderInput("timeframe", "Weeks", min = 1, max = 25, 
                         ticks = TRUE, step = 1,  value = c(1,25),
                         dragRange = TRUE)),
    tabPanel("Inverclyde",
             inputPanel(
               radioButtons("select_ind", "COVID19/Total Cases",  choices = c("covid","total"), selected = "covid"),
               selectInput("select_service", label = "Indicator", 
                           choices = list("Unplanned Admissions" = "ea",
                                          "A&E" = "ae", 
                                          "NHS24" = "nhs24",
                                          "GP OOH" = "ooh",
                                          "SAS" = "sas",
                                          "ECOSS" = "ecoss",
                                          "Deaths" = "deaths"), 
                           selected = "ecoss")),
             sliderInput("timeframe", "Weeks", min = 1, max = 25, 
                         ticks = TRUE, step = 1,  value = c(1,25),
                         dragRange = TRUE)),
    tabPanel("West Dunbartonshire",
             inputPanel(
               radioButtons("select_ind", "COVID19/Total Cases",  choices = c("covid","total"), selected = "covid"),
               selectInput("select_service", label = "Indicator", 
                           choices = list("Unplanned Admissions" = "ea",
                                          "A&E" = "ae", 
                                          "NHS24" = "nhs24",
                                          "GP OOH" = "ooh",
                                          "SAS" = "sas",
                                          "ECOSS" = "ecoss",
                                          "Deaths" = "deaths"), 
                           selected = "ecoss")),
             sliderInput("timeframe", "Weeks", min = 1, max = 25, 
                         ticks = TRUE, step = 1,  value = c(1,25),
                         dragRange = TRUE))
  
  )
) # fluidPage bracket

## END
)