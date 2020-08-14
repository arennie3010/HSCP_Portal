# Description - Describe what the app does (e.g. visualizes births data)
# This is the server script, it produces the outputs and reactive objects
# of the app: charts, text,

#nhs24 <- read.csv(gzfile("data/nhs24.csv.gz"))
# iz <- read.csv("data/2020_data/UCdata-week-iz.csv")
# hscp <- read.csv("data/2020_data/UCdata-week-hscp.csv")

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
             source == input$select_service,
             year == 2020,
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
                           "Mean Weekly", input$select_ind,":", round_half_up(value,1))) %>%
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
                                  names(which(choice_list == input$select_service)), " Cases (", input$select_service, ")"), 
                     x="Week", y="") + 
                scale_x_discrete(position = "top") +
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
  


#### SUMMARY 

  selected_summary_data <- reactive({
    
    hscp %>%
      filter(hscp == input$selectHSCPsummary,
             week %in% input$timeframesummary[1]:input$timeframesummary[2],
             ind == input$select_indsummary) %>%
      mutate(text = paste0("HSCP: ", hscp, "\n", 
                           "Week: ", week, "\n", 
                           "Rate: ",round_half_up(value,1)))
    
  })
  
  selected_text <- reactive({
    
    
  })
  
  
# CHART ! - SUMMARY

output$sc1 <- renderPlotly({
  
    ggplotly(ggplot(subset(selected_summary_data(), source == "OOH"), aes(week, value)) +
    geom_line(aes(group = year, colour = factor(year)), size = 1.4) +
    theme_classic()  +
      theme(legend.title = element_blank()) +
    labs(title = "GP OOH Cases", subtitle = paste("Weeks",input$timeframesummary[1],"to",input$timeframesummary[2]),
         caption = "Data source: Unscheduled Care database", x = "Week", y = paste(input$select_indsummary))) %>%
    config(displayModeBar = FALSE)
 
})

output$text1 <- renderText({"Chart commentary for GP OOH chart which will be automated via RMarkdown."})

output$sc2 <- renderPlotly({
  
  ggplotly(ggplot(subset(selected_summary_data(), source == "A&E"), aes(week, value)) +
    geom_line(aes(group = year, colour = factor(year)), size = 1.4) +
    theme_classic()  +
      theme(legend.title = element_blank()) +
    labs(title = "A&E Cases", subtitle = paste("Weeks",input$timeframesummary[1],"to",input$timeframesummary[2]),
         caption = "Data source: Unscheduled Care database", x = "Week", y = paste(input$select_indsummary))) %>%
    config(displayModeBar = FALSE)
})

output$text2 <- renderText({"Chart commentary for A&E chart which will be automated via RMarkdown."})

output$sc3 <- renderPlotly({
  
  ggplotly(ggplot(subset(selected_summary_data(), source == "NHS24"), aes(week, value)) +
    geom_line(aes(group = year, colour = factor(year)), size = 1.4) +
    theme_classic() +
      theme(legend.title = element_blank()) +
    labs(title = "NHS24 Cases", subtitle = paste("Weeks",input$timeframesummary[1],"to",input$timeframesummary[2]),
         caption = "Data source: Unscheduled Care database", x = "Week", y = paste(input$select_indsummary))) %>%
    config(displayModeBar = FALSE)
})

output$text3 <- renderText({"Chart commentary for NHS24 chart which will be automated via RMarkdown."})

## REPORT
selected_report_data <- reactive({
  
  hscp %>%
    filter(hscp == input$selectHSCPrmd,
           week %in% input$timeframe_rmd[1]:input$timeframe_rmd[2],
           ind == input$select_ind_rmd,
           source %in% input$select_service_rmd)
  
})


P1_RMD <- reactive({
  if("A&E" %in% input$select_service_rmd){
  selected_report_data() %>%
    spread(year,value) %>%
    mutate(difference = `2020` - `2019`)  %>% filter(source == "A&E") %>%
    ggplot(aes(x= week, ymin = 0, ymax = difference)) +
    geom_ribbon(alpha = 0.7, fill = "#43358b") +
    theme_minimal() +
    labs(x = "Week of the Year", paste("Difference in", input$select_ind_rmd, "from 2019"),
         title = paste("Difference between", input$select_ind_rmd, "at", 
                       "A&E in 2019 and 2020 by week"))
  }else{}
})

output$ae_plot_rmd <- renderPlot({
    P1_RMD()
})


P2_RMD <- reactive({
  if("NHS24" %in% input$select_service_rmd){
    selected_report_data() %>% filter(source == "NHS24") %>%
      ggplot(aes(x= week, y = value)) +
      geom_line(alpha = 0.7, colour = "#43358b", size = 1.5) +
      theme_minimal() +
      labs(x = "Week of the Year", paste("Difference in", input$select_ind_rmd, "from 2019"),
           title = paste("NHS24", input$select_ind_rmd, "in 2020 by week"))}else{}
})

output$nhs24_plot_rmd <- renderPlot({
    P2_RMD()
})

P3_RMD <- reactive({
  if("OOH" %in% input$select_service_rmd){
  selected_report_data() %>%
    spread(year,value) %>%
    mutate(difference = `2020` - `2019`)  %>% filter(source == "OOH") %>%
    ggplot(aes(x= week, ymin = 0, ymax = difference)) +
    geom_ribbon(alpha = 0.7, fill = "#43358b") +
    theme_minimal() +
    labs(x = "Week of the Year", paste("Difference in", input$select_ind_rmd, "from 2019"),
         title = paste("Difference between", input$select_ind_rmd, "at", 
                       "GP OOH in 2019 and 2020 by week"))}else{}
})

output$gpooh_plot_rmd<- renderPlot({
    P3_RMD()
  })

# observeEvent(input$RMD, {
#   rmarkdown::render(input = "RMarkdown script.Rmd")
# }
#              )

output$RMD<- downloadHandler(
  filename = function() {
    paste(input$selectHSCPrmd, "HSCP Report.docx")
  },
content = function(file) {
  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  tempReport <- file.path(tempdir(), "report.Rmd")
  file.copy("RMarkdown script.Rmd", tempReport, overwrite = TRUE)
  
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  rmarkdown::render(input = "RMarkdown script.Rmd", output_file = file, output_format = "word_document")
  })


})
## END
