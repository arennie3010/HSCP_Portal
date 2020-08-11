# Description - Describe what the app does (e.g. visualizes births data)
# This is the server script, it produces the outputs and reactive objects
# of the app: charts, text,

#nhs24 <- read.csv(gzfile("data/nhs24.csv.gz"))
iz <- read.csv("data/CONCEPT_DATA by IZ for GC.csv")

shinyServer(function(input, output, session) {
  
  
  # selectedDada <- reactive({
  #   d<-nhs24 %>%
  #     filter(case_type == input$select_ind) %>%
  #     filter(ind == input$select_ind)
  #   
  #   
  # })
  
  selected_IZ_data <- reactive({
    
    iz %>%
      filter(hscp == input$selectHSCP,
             variable == input$select_service,
             week %in% input$timeframe[1]:input$timeframe[2],
             ind == input$select_ind) %>%
      mutate(text = paste0("Interzone: ", intzone, "\n", 
                           "Week: ", week, "\n", 
                           "Rate: ",round_half_up(value,1)))
    
  })
  
  map_dat <- reactive({
    merge_dat <- selected_IZ_data() %>% 
      group_by(intzone) %>%
      summarise(value = mean(value)) %>%
      ungroup() %>%
      mutate(text = paste0("Interzone: ", intzone, " \n", 
                           "Weeks: ", input$timeframe[1], " to ", input$timeframe[2], " \n", 
                           "Mean Rate: ",round_half_up(value,1))) %>%
      rename(area_name = intzone)
    
    sp::merge(iz_bounds[iz_bounds$council == input$selectHSCP,], 
          merge_dat, by = "area_name")
  })
  
  ## MAP
  output$map_iz <- renderLeaflet({
      
    pal <- colorNumeric(
      palette = colorRampPalette(c("#FFFFB7","#FF9100","red"), bias = 2.5)(length(map_dat()$area_name)), 
      domain = c(min(map_dat()$value):max(map_dat()$value+1)))
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=map_dat(),
                  color = "#444444", weight = 2, smoothFactor = 0.5,
                  #tooltip
                  label = (map_dat()$text),
                  opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal(map_dat()$value), #Colours
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      )
      
  })
  
  ## HEATCHART
  output$heatchart_iz <- renderPlot({
    

    ggplot(selected_IZ_data(), aes(week, intzone, fill= value, text=text)) + 
                scale_y_discrete(limits = unique(rev(selected_IZ_data()$intzone))) + # reverses y axis - alphabetical from top
                geom_tile(aes(fill = value)) +
                geom_text(aes(label = format(round_half_up(value,digits = 1), nsmall = 1)), size=4) +
                labs(title=paste0(input$selectHSCP," HSCP Intermediate Zones \n Weekly ", 
                                  names(which(choice_list == input$select_service)), " Cases (", input$select_ind, ")"), 
                     x="", y="") + 
                scale_fill_viridis(option = "viridis") +
                theme_grey(base_size = 16) + labs(fill = "Rate per\n1,000 population") +
                theme(legend.position = "top")
    
  })
  
  ## DATATABLE
  output$datatable_iz <- renderDataTable({
    selected_IZ_data() %>% 
      mutate(value = round_half_up(value,2)) %>%
      
      select(W = week, `Week` = intzone, 
                  `Rate (per 1,000 population)`=value) %>%
      spread(W, `Rate (per 1,000 population)`) %>%
      datatable(rownames = FALSE)
  })
  
})

## END
