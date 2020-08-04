# Description - Describe what the app does (e.g. visualizes births data)
# This is the server script, it produces the outputs and reactive objects
# of the app: charts, text,

nhs24 <- read.csv(gzfile("data/nhs24.csv.gz"))

shinyServer(function(input, output, session) {
  
  
  selectedDada <- reactive({
    d<-nhs24 %>%
      filter(case_type == input$select_ind) %>%
      filter()
    
    
  })
  
})

## END
