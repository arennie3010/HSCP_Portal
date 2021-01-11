# Description - Describe what the app does (e.g. visualizes births data)
# This is the server script, it produces the outputs and reactive objects
# of the app: charts, text,

# test

#nhs24 <- read.csv(gzfile("data/nhs24.csv.gz"))
# iz <- read.csv("data/2020_data/UCdata--iz.csv")
# hscp <- read.csv("data/2020_data/UCdata--hscp.csv")

shinyServer(function(input, output, session) {
  
  
  # selectedDada <- reactive({
  #   d<-nhs24 %>%
  #     filter(case_type == input$select_ind) %>%
  #     filter(ind == input$select_ind)
  #   
  #   
  # })
  
 
  ########## Annual change data  ###############################################
  selected_IZ_data_test <- reactive({
    
    iz.m %>%
      filter(ind == "cases") %>%
      filter(hscp == input$selectHSCP_1,
             source == input$select_service_1,
             month %in% input$timeframe_1[1]:input$timeframe_1[2]) %>%
      group_by(year,intzone) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(c_change = round(100*((`2020`-`2019`)/`2019`),1)) %>%
      # create percentiles for heatchart fill
      mutate(tile_rank = ifelse(c_change > colour_cat, 2, 
                                ifelse(c_change > -colour_cat, 1, 0))) %>%
     # mutate(rate_flag_name = ifelse(tile_rank == 0, "lower",  # 2 = high, 0 = low, 1 = same
    #                                 ifelse(tile_rank == 1, "same",
    #                                        ifelse(tile_rank == 2, "higher", NA)))) %>%
      mutate(text = paste0("Interzone: ", intzone, "\n", 
                           "Annual Change: ",round_half_up(c_change,1)))
    
  })
  
############ Annual Change Map ####################################################
  
  map_dat <- reactive({
    merge_dat <- selected_IZ_data_test() %>% 
     # group_by(intzone) %>%
    #  summarise(value = mean(value)) %>%
     # ungroup() %>%
      # Create percentiles for the colours being used
    #  mutate(tile_rank = ntile(value,10)) %>%
      rename(area_name = intzone) %>%
      left_join(distinct(select(iz_bounds@data, area_name, code)), by = "area_name") %>%
      left_join(int_pops, by = "code") %>%
      #
      mutate(text = paste0("InterZone Code: ", code, " <br/>", 
                           "InterZone Name: ", area_name, "<br/>", 
                           "InterZone Population:",format(pop, big.mark = ","), "<br/>",
                           "Months: ", month(input$timeframe_1[1], label = TRUE, abbr = FALSE), 
                           " to ", 
                           month(input$timeframe_1[2], label = TRUE, abbr = FALSE), " ", 
                           "<i>(MB:", input$timeframe_1[1], "- MB:",
                           input$timeframe_1[2], ")</i><br/>",
                           #
                           "Annual change: ", round_half_up(c_change,1),"%"))
    
    sp::merge(iz_bounds[iz_bounds$council == input$selectHSCP_1,], 
          merge_dat, by = c("area_name", "code"))
  })
  
  
  tag.map.text <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 65%;
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 12px;
  }
"))
  
  
  guidance_1 <- tags$div(
    tag.map.text, HTML(paste("Explore the map using your mouse. Click on an area to view information concerning the annual change in the number of cases for the chosen service and area.
The colours categorise the type of change in the service usage for the selected service and time period:<br/>
                             blue = A decrease of more than 10% from the previous year<br/>
                             orange = An increase of more than 10% from the previous year<br/>
                             grey = Increase or decrease less than 10% either way"))
  ) 
  
  
  
  ## MAP
  output$map_iz <- renderLeaflet(
    {
      
      pal1 <- colorNumeric(
        palette = colorRampPalette(c("blue","grey","orange"))(3), 
        domain = c(0:2))
    
    legend_title <- paste(case_when(input$select_ind_1 == "change" ~ "Percent annual change"))

    
    leaflet() %>% 
      addControl(guidance_1, position="topright", className = "map-title")%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=map_dat(),
                  color = "#444444", weight = 2, smoothFactor = 0.5,
                  #tooltip
                  label = lapply(map_dat()$text, htmltools::HTML),
                  opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal1(map_dat()$tile_rank), #Colours
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) #%>%
     # addLegend("topright", pal = pal1, values = 0:2, 
    #            title = "Annual change category",
    #            opacity = 1)
      
  })
  
  ############ annual change heatchart data ######################
  
  heatchart_data_change <- reactive({
    
    iz.m %>%
      filter(ind == "cases") %>%
      filter(hscp == input$selectHSCP_1,
             source == input$select_service_1,
             month %in% input$timeframe_1[1]:input$timeframe_1[2]) %>%
      group_by(year, month, intzone) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(c_change = round(100*((`2020`-`2019`)/`2019`),1)) %>%
      # create percentiles for heatchart fill
      mutate(tile_rank = ifelse(c_change > colour_cat, 2, 
                                ifelse(c_change > -colour_cat, 1, 0))) %>%
      # mutate(rate_flag_name = ifelse(tile_rank == 0, "lower",  # 2 = high, 0 = low, 1 = same
      #                                 ifelse(tile_rank == 1, "same",
      #                                        ifelse(tile_rank == 2, "higher", NA)))) %>%
      mutate(text = paste0("Interzone: ", intzone, "\n", 
                           "Annual Change: ",round_half_up(c_change,1)))
    
  })
  
  
  
  
  
  ##################### Annual change  HEATCHART #########################################
  output$heatchart_iz <- renderPlot(
    height = function () 800+10*length(unique(heatchart_data_change()$intzone)),
    
    
    {
      
      
      ggplot(heatchart_data_change(), aes(factor(month, levels = input$timeframe[1]:input$timeframe[2]), 
                                     intzone, 
                                     #fill= tile_rank, 
                                     text=text)) + 
        scale_y_discrete(limits = unique(rev(heatchart_data_change()$intzone))) + # reverses y axis - alphabetical from top
        geom_tile(aes(fill = tile_rank)) +
        geom_text(aes(label = format(round_half_up(c_change,digits = 0), nsmall = 0)), size=4.5) +
        labs(title=paste0(input$selectHSCP," HSCP Intermediate Zones \n Monthly ", 
                          names(which(source_list == input$select_service_1)), " Cases (", input$select_service_1, ")"), 
             x="Month", y="") + 
        scale_x_discrete(position = "top") +
        # Can choose different colour scales if required
        #scale_fill_viridis_c(option = "C") +
       # scale_fill_continuous(low = "#FFFFB7",
      #                        high = "red") +
        scale_fill_manual(values = c("#FF9100","#3586FF", "gray"), guide = guide_legend(reverse = TRUE)) +
        theme_grey(base_size = 16) + 
        labs(fill = "% Change from Previous Year") +
        theme(legend.position = "none",
              plot.title = element_text(family="helvetica", face = "bold"))
      
      
    })
  

  
  
  
  #################### annual change data end####################################
  
  
  
  
  
  
  ################## poisson test  ###################################
  
  selected_IZ_data_pois <- reactive({
    
    test <- pois_dt %>%
      filter(hscp == input$selectHSCP,
             source == input$select_service,
             year == 2020,
             month %in% input$timeframe[1]:input$timeframe[2]) %>%
      group_by(hscp, intzone) %>%
      summarise(population = first(population),
                cases = sum(cases),
                hscp_population = first(hscp_population),
                hscp_cases = sum(hscp_cases)) %>%
      mutate(rate = round(r.m*(cases/population),1)) %>%
      mutate(olow = qchisq(p = 1-(alpha/2), df = 2*cases, lower.tail = FALSE)/2) %>%
      mutate(oup = qchisq(p=alpha/2, df = (2*cases)+2, lower.tail = FALSE)/2) %>%
      mutate(rlow = round(olow/population*r.m,1)) %>%
      mutate(rup = round(oup/population*r.m,1)) %>%
      mutate(crude_hscp = round(r.m*(hscp_cases/hscp_population)),1) %>%
      mutate(rate_flag = ifelse(crude_hscp < rlow, 2,  # 2 = high, 0 = low, 1 = same
                                ifelse(crude_hscp > rup, 0, # low
                                       ifelse(crude_hscp <= rup & crude_hscp >= rlow, 1, NA)))) %>%
      select(hscp, intzone, rate_flag, cases, rate, rlow, rup, crude_hscp) %>%
      mutate(rate_flag_name = ifelse(rate_flag == 0, "low",  # 2 = high, 0 = low, 1 = same
                                     ifelse(rate_flag == 1, "same",
                                            ifelse(rate_flag == 2, "high", NA)))
      )
  })
  
  
  
  map_dat_pois <- reactive({
   test_1 <- selected_IZ_data_pois() %>% 
      # pivot_longer(cols = 3:5, names_to = "ind", values_to = "value") %>%
      rename(area_name = intzone) %>%
      left_join(distinct(select(iz_bounds@data, area_name, code)), by = "area_name") %>%
      left_join(int_pops, by = "code")  %>%
      mutate(text = paste0(
        #"InterZone Code: ", code, " <br/>", 
                           "InterZone Name: ", area_name, "<br/>", 
                           #"InterZone Population:",format(pop, big.mark = ","), "<br/>",
                           "Months: ", month(input$timeframe[1], label = TRUE, abbr = FALSE)," to ", 
                           month(input$timeframe[2], label = TRUE, abbr = FALSE), "</i><br/>",
                           "InterZone rate: ", round(rate,0), " per 1,000 population", "<br/>",
                            100*(1-alpha), "%", " Confidence Interval: ", round(rlow,0), " to ",  round(rup,0), "<br/>",
                        #   "InterZone rate compared to HSCP rate: ", rate_flag_name, "<br/>",
                           "HSCP rate (Comparator): ",round(crude_hscp,0)))
    
    
    sp::merge(iz_bounds[iz_bounds$council == input$selectHSCP,], 
              test_1, by = c("area_name", "code"))
  })
  


  
  guidance <- tags$div(
    tag.map.text, HTML(paste("Explore the map using your mouse. Click on an area to view information concerning the number of cases and rate of cases per 1,000
popultation. The colours represent the IZ rate of cases compared to the overall HSCP rate:<br/>
                             blue = IZ rate is significantly lower than the HSCP rate<br/>
                             orange = IZ rate is significantly higher than the HSCP rate<br/>
                             grey = IZ is neither significantly higher or lower than the HSCP rate"))
  ) 
  
  
  
  
  ## MAP
  output$map_pois <- renderLeaflet(
    {
      
      pal <- colorNumeric(
        palette = colorRampPalette(c("blue","grey","orange"))(3), 
        domain = c(0:2))
      
      
      leaflet() %>% 
        addControl(guidance, position="topright", className = "map-title")%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data=map_dat_pois(),
                    color = "#444444", weight = 2, smoothFactor = 0.5,
                    #tooltip
                    label = lapply(map_dat_pois()$text, htmltools::HTML),
                    opacity = 1.0, fillOpacity = 0.75, fillColor = ~pal(map_dat_pois()$rate_flag), #Colours
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE)) 
      
    })
  
  
  
  ###################################################################
  ###       HEATCHART WITH DIFFERENT COLOURS                     ###
  
 heatchart_pois_dat <- reactive({
    
    pois_dt %>%
      filter(hscp == input$selectHSCP,
             source == input$select_service,
             year == 2020,
             month %in% input$timeframe[1]:input$timeframe[2]) %>%
      group_by(hscp, intzone, month) %>%
      summarise(population = first(population),
                cases = sum(cases),
                hscp_population = first(hscp_population),
                hscp_cases = sum(hscp_cases)) %>%
      mutate(rate = round(r.m*(cases/population),1)) %>%
      mutate(olow = qchisq(p = 1-(alpha/2), df = 2*cases, lower.tail = FALSE)/2) %>%
      mutate(oup = qchisq(p=alpha/2, df = (2*cases)+2, lower.tail = FALSE)/2) %>%
      mutate(rlow = round(olow/population*r.m,1)) %>%
      mutate(rup = round(oup/population*r.m,1)) %>%
      mutate(crude_hscp = round(r.m*(hscp_cases/hscp_population)),1) %>%
      mutate(rate_flag = ifelse(crude_hscp < rlow, 2,  # 2 = high, 0 = low, 1 = same
                                ifelse(crude_hscp > rup, 0, # low
                                       ifelse(crude_hscp <= rup & crude_hscp >= rlow, 1, NA)))) %>%
      select(hscp, intzone, month, rate_flag, cases, rate, rlow, rup, crude_hscp) %>%
      mutate(rate_flag_name = ifelse(rate_flag == 0, "low",  # 2 = high, 0 = low, 1 = same
                                     ifelse(rate_flag == 1, "same",
                                            ifelse(rate_flag == 2, "high", NA))))  %>%
     pivot_longer(cols = 5:6, names_to = "ind", values_to = "value") %>%
     filter(ind == input$select_ind) %>%
     mutate(text = paste0("HSCP: ", hscp, "\n", 
                          "Year: ", intzone, "\n",
                          "Month: ", month(month, label = TRUE, abbr = FALSE), "\n",
                          "Value: ",format(round_half_up(value,1), big.mark = ",")))
   
  })
  
  
  output$heatchart_pois <- renderPlot(
    height = function () 800+10*length(unique(heatchart_pois_dat()$intzone)),
    
    
    {
      
      
     # ggplotly(
        ggplot(heatchart_pois_dat(), aes(factor(month, levels = input$timeframe[1]:input$timeframe[2]), 
                                     intzone, fill= rate_flag_name)) + 
        scale_y_discrete(limits = unique(rev(heatchart_pois_dat()$intzone))) + # reverses y axis - alphabetical from top
        geom_tile(aes(fill = rate_flag_name)) +
        geom_text(aes(label = format(round_half_up(value,digits = 0), nsmall = 0)), size=4.5) +
        labs(title=paste0(input$selectHSCP," HSCP Intermediate Zones \n Monthly ", 
                          names(which(source_list == input$select_service)), " (", names(which(measure_list == input$select_ind)),")"), 
             x="Month", y="") + 
        scale_x_discrete(position = "top") +
        # Can choose different colour scales if required
        #scale_fill_viridis_c(option = "C") +
        scale_fill_manual(values = c("#FF9100","#3586FF", "gray"), guide = guide_legend(reverse = TRUE)) +
        theme_grey(base_size = 16) + 
        labs(fill = "IZ rate vs HSCP rate (Comparator)") +
        theme(legend.position = "top",
              plot.title = element_text(family="helvetica", face = "bold")) 
      
        #, tooltip = c("text")) %>%
   # config(displayModeBar = FALSE) %>% 
   #     layout(height = 800+10*length(unique(heatchart_dat()$intzone)))
    })
  
  
  
  ## DATATABLE
  output$datatable_iz <- renderDataTable({
    selected_IZ_data_pois() %>% 
      mutate(value = round_half_up(rate,2)) %>%
      
      select(M = month, `Month` = intzone, 
             `Rate (per 1,000 population)`=rate) %>%
      spread(M, `Rate (per 1,000 population)`) %>%
      datatable(rownames = FALSE)
  })
  
  
  
  ################################
  
  


#### SUMMARY 

  selected_summary_data <- reactive({
    
    hscp.m %>%
      filter(hscp == input$selectHSCPsummary,
             month %in% input$timeframesummary[1]:input$timeframesummary[2],
             ind == input$select_indsummary) %>%
      mutate(text = paste0("HSCP: ", hscp, "\n", 
                           "Year: ", year, "\n",
                           "Month: ", month(month, label = TRUE, abbr = FALSE), "\n",
                           "Value: ",format(round_half_up(value,1), big.mark = ",")))
    
  })
  
  selected_text <- reactive({
    
    
  })
 
info_data <- reactive({
  hscp.m %>%
    filter(hscp == input$selectHSCPsummary,
           month %in% input$timeframesummary[1]:input$timeframesummary[2],
           ind == input$select_indsummary) %>%
    group_by(source) %>%
    summarise(value = format(round(mean(value),0),big.mark=",")) %>%
    ungroup() 
  
})
  
#infoboxes 
output$aebox <- renderText({as.character(info_data() %>% filter(source == "A&E") %>% select(value))})
output$eabox <- renderText({as.character(info_data() %>% filter(source == "EA") %>% select(value))})
output$oohbox <- renderText({as.character(info_data() %>% filter(source == "OOH") %>% select(value))})
output$nhs24box <- renderText({as.character(info_data() %>% filter(source == "NHS24") %>% select(value))})
output$sasbox <- renderText({as.character(info_data() %>% filter(source == "SAS") %>% select(value))})



output$sc1 <- renderPlotly({
  
  
  
    ggplotly(ggplot(subset(selected_summary_data(), source == "OOH"), aes(month, value)) +
    geom_line(aes(text = text, colour = year, group = year), size = 1.4) +
    theme_light()  +
      theme(legend.title = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5)) +
      scale_color_manual(values=c("mediumpurple1", "#43358b")) +
    labs(title = "GP OOH Cases", x = "Month", y = paste(input$select_indsummary)), tooltip = c("text")) %>%
    config(displayModeBar = FALSE)
 
})

output$text1 <- renderText({paste0("This chart shows the change in GP out of hours service usage from month beginning ", input$timeframesummary[1], " to ", input$timeframesummary[2], ". ",
                                  "\n",  "There has been an *increase/decrease* in ", input$select_indsummary, " for GP OOH cases in ", input$selectHSCPsummary)})

output$sc2 <- renderPlotly({
  
  ggplotly(ggplot(subset(selected_summary_data(), source == "A&E"), aes(month, value)) +
    geom_line(aes(text = text, group = year, colour = year), size = 1.4) +
    theme_light()  +
      theme(legend.title = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5)) +
      scale_color_manual(values=c("mediumpurple1", "#43358b")) +
    labs(title = "A&E Cases", subtitle = paste("Months", input$timeframesummary[1],"to",input$timeframesummary[2]),
         caption = "Data source: Unscheduled Care database", x = "Month", y = paste(input$select_indsummary)), tooltip = c("text")) %>%
    config(displayModeBar = FALSE)
})

output$text2 <- renderText({"Chart commentary for A&E chart which will be automated via RMarkdown."})

output$sc3 <- renderPlotly({
  
  ggplotly(ggplot(subset(selected_summary_data(), source == "NHS24"), aes(month, value)) +
    geom_line(aes(text = text, group = year, colour = year), size = 1.4) +
    theme_light() +
      theme(legend.title = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5)) +
      scale_color_manual(values=c("mediumpurple1", "#43358b")) +
    labs(title = "NHS24 Cases", subtitle = paste("Months", input$timeframesummary[1],"to", input$timeframesummary[2]),
         caption = "Data source: Unscheduled Care database", x = "Month", y = paste(input$select_indsummary)), tooltip = c("text")) %>%
    config(displayModeBar = FALSE)
})
  output$text3 <- renderText({"Chart commentary for NHS24 chart which will be automated via RMarkdown."})
  
  output$sc4 <- renderPlotly({
    
    ggplotly(ggplot(subset(selected_summary_data(), source == "SAS"), aes(month, value)) +
               geom_line(aes(text = text, group = year, colour = year), size = 1.4) +
               theme_light()  +
               theme(legend.title = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5)) +
               scale_color_manual(values=c("mediumpurple1", "#43358b")) +
               labs(title = "SAS Cases", subtitle = paste("Months",input$timeframesummary[1],"to",input$timeframesummary[2]),
                    caption = "Data source: Unscheduled Care database", x = "Month", y = paste(input$select_indsummary)), tooltip = c("text")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$text4 <- renderText({"Chart commentary for SAS chart which will be automated via RMarkdown."})
  
  
  output$sc5 <- renderPlotly({
    
    ggplotly(ggplot(subset(selected_summary_data(), source == "EA"), aes(month, value)) +
               geom_line(aes(text = text, group = year, colour = year), size = 1.4) +
               theme_light()  +
               theme(legend.title = element_blank(), plot.title = element_text(face = "bold", hjust = 0.5)) +
               scale_color_manual(values=c("mediumpurple1", "#43358b")) +
               labs(title = "EA Cases", subtitle = paste("Months", input$timeframesummary[1],"to",input$timeframesummary[2]),
                    caption = "Data source: RAPID datamart", x = "Month", y = paste(input$select_indsummary)), tooltip = c("text")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$text5 <- renderText({"Chart commentary for Emergency Admissions chart which will be automated via RMarkdown."})



## REPORT
selected_report_data <- reactive({
  
  hscp.m %>%
    filter(hscp == input$selectHSCPrmd,
           month %in% input$timeframe_rmd[1]:input$timeframe_rmd[2],
           ind == input$select_ind_rmd,
           source %in% input$select_service_rmd)
  
})


P1_RMD <- reactive({
  if("A&E" %in% input$select_service_rmd){
  selected_report_data() %>%
    spread(year,value) %>%
    mutate(difference = `2020` - `2019`)  %>% filter(source == "A&E") %>%
    ggplot(aes(x= month, ymin = 0, ymax = difference)) +
    geom_ribbon(alpha = 0.7, fill = "#43358b") +
    theme_minimal() +
    labs(x = "Month of the Year", paste("Difference in", input$select_ind_rmd, "from 2019"),
         title = paste("Difference between", input$select_ind_rmd, "at", 
                       "A&E in 2019 and 2020 by month"))
  }else{}
})

output$ae_plot_rmd <- renderPlot({
    P1_RMD()
})


P2_RMD <- reactive({
  if("NHS24" %in% input$select_service_rmd){
    selected_report_data() %>% filter(source == "NHS24") %>%
      ggplot(aes(x= month, y = value)) +
      geom_line(alpha = 0.7, colour = "#43358b", size = 1.5) +
      theme_minimal() +
      labs(x = "Month of the Year", paste("Difference in", input$select_ind_rmd, "from 2019"),
           title = paste("NHS24", input$select_ind_rmd, "in 2020 by month"))}else{}
})

output$nhs24_plot_rmd <- renderPlot({
    P2_RMD()
})

P3_RMD <- reactive({
  if("OOH" %in% input$select_service_rmd){
  selected_report_data() %>%
    spread(year,value) %>%
    mutate(difference = `2020` - `2019`)  %>% filter(source == "OOH") %>%
    ggplot(aes(x= month, ymin = 0, ymax = difference)) +
    geom_ribbon(alpha = 0.7, fill = "#43358b") +
    theme_minimal() +
    labs(x = "Month of the Year", paste("Difference in", input$select_ind_rmd, "from 2019"),
         title = paste("Difference between", input$select_ind_rmd, "at", 
                       "GP OOH in 2019 and 2020 by month"))}else{}
})

output$gpooh_plot_rmd<- renderPlot({
    P3_RMD()
  })

P4_RMD <- reactive({
  if("SAS" %in% input$select_service_rmd){
    selected_report_data() %>% filter(source == "SAS") %>%
      ggplot(aes(x= month, y = value)) +
      geom_line(alpha = 0.7, colour = "#43358b", size = 1.5) +
      theme_minimal() +
      labs(x = "Month of the Year", paste("Difference in", input$select_ind_rmd, "from 2019"),
           title = paste("SAS", input$select_ind_rmd, "in 2020 by month"))}else{}
})

output$sas_plot_rmd <- renderPlot({
  P4_RMD()
})

P5_RMD <- reactive({
  if("EA" %in% input$select_service_rmd){
    selected_report_data() %>% filter(source == "EA") %>%
      ggplot(aes(x= month, y = value)) +
      geom_line(alpha = 0.7, colour = "#43358b", size = 1.5) +
      theme_minimal() +
      labs(x = "Month of the Year", paste("Difference in", input$select_ind_rmd, "from 2019"),
           title = paste("EA", input$select_ind_rmd, "in 2020 by month"))}else{}
})

output$ea_plot_rmd <- renderPlot({
  P5_RMD()
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
