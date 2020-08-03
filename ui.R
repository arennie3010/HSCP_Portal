# Description - Describe what the app does (e.g. visualizes births data)
# User interface - how your app looks and elements users can interact with



shinyUI(fluidPage(
  
  titlePanel("Unscheduled Care - HSCP Portal"),
  
  navlistPanel(
    "Select HSCP:",
    tabPanel("East Dunbartonshire",
             h3("This panel is for East Dunbartonshire")
             ),
    tabPanel("East Renfrewshire"),
    tabPanel("Glasgow City"),
    tabPanel("Inverclyde"),
    tabPanel("West Dunbartonshire")
  
  )
) # fluidPage bracket

## END
)