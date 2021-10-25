function(input, output, session) {
  
  #===================================================
  #=========        Map View tab           =========== 
  #===================================================
  
  #--------- Render base map ----------
  
  output$myMap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(lng = 144.946457, lat = -37.020100, zoom = 8)
  })
  
  #--------- Reactively update selected data info,
  #          when UI input data selection is changed. ---------
  mapData <- reactive({
    data_all %>%
    filter( 
      (input$mapName == 'All' | LGA_NAME == input$mapName) &
        (input$mapYear == 'All' | year    == input$mapYear) &
        (month >= input$mapSliderMonth[1] & month <= input$mapSliderMonth[2]) &
        ((SEVERITY == 'Serious injury accident'  & 'Serious injury accident' %in% input$mapSeverity) | 
           (SEVERITY == 'Fatal accident' & 'Fatal accident' %in% input$mapSeverity) | 
           (SEVERITY == 'Other injury accident'  & 'Other injury accident'  %in% input$mapSeverity)) &
        ((SPEED_ZONE == 30  & 30 %in% input$mapSpeed) | 
            (SPEED_ZONE == 40 & 40 %in% input$mapSpeed) | 
            (SPEED_ZONE == 50  & 50  %in% input$mapSpeed) |
            (SPEED_ZONE == 60  & 60 %in% input$mapSpeed) | 
            (SPEED_ZONE == 70 & 70 %in% input$mapSpeed) | 
           (SPEED_ZONE == 75 & 75 %in% input$mapSpeed) | 
            (SPEED_ZONE == 80  & 80  %in% input$mapSpeed) |
            (SPEED_ZONE == 90  & 90 %in% input$mapSpeed) | 
            (SPEED_ZONE == 100 & 100 %in% input$mapSpeed) | 
            (SPEED_ZONE == 110  & 110  %in% input$mapSpeed)) 
         )
  })
  
  #-------- instantly update map with updated data,
  #         when UI input data selection is changed. -----------
  observe({
    if (input$showHeatMap) {
      leafletProxy('myMap', data = mapData()) %>% #don't forget mapData()
        clearWebGLHeatmap() %>%
        addWebGLHeatmap(~LONGITUDE, ~LATITUDE, size = 20, unit = "px", opacity = 0.6)
    } else {
      leafletProxy('myMap', data = mapData()) %>% 
        clearWebGLHeatmap()
    }
    
    if (input$showClusterMap) {
      factpal <- colorFactor(c('green', 'blue', 'red'), Sevr)
      
      leafletProxy('myMap', data = mapData()) %>% #don't forget mapData()
        clearMarkerClusters() %>% 
        addCircleMarkers(lng = ~LONGITUDE, lat=~LATITUDE, radius = 3, stroke = F,
                         color = ~factpal(SEVERITY), fillOpacity = 0.2,
                         clusterOptions = markerClusterOptions())
    } else {
      leafletProxy('myMap', data = mapData()) %>% 
        clearMarkerClusters()  
    }
    })
  
  #===================================================
  #=========          Factors tab          =========== 
  #===================================================
  
  
  #--------- bar chart Traffic Accident by year and REGION_NAME -------
  output$barPlotYearRegion = renderPlotly({
    
    p <- ggplot(data_all) +
      geom_bar(aes(x = year,fill = REGION_NAME), position='dodge', width=0.6) +
      labs(title='Traffic Accident by Region over Years',
           x=NULL, 
           y='Count') +
      scale_fill_brewer(palette='Set1', name=NULL) +
      scale_y_continuous_hua(limits = c(0, 5000)) + 
      theme_hua()
    ggplotly(p)
  }) 
  
  #--------- Traffic Accident by month ----------
  output$freqpolyPlotMonth = renderPlotly({
    
    p <- ggplot(data_all) +
      geom_freqpoly(aes(x=month, color=REGION_NAME), bins=12) +
      labs(title='Traffic Accident by Month', 
           x=NULL, 
           y='Count') + 
      scale_color_discrete(name=NULL, labels=unique(data_all$REGION_NAME)) +
      scale_x_continuous_hua(limits = c(1, 12), breaks = 1:12, 
                             labels=c("Jan","Feb","Mar","Apr","May","Jun",
                                      "Jul","Aug","Sep","Oct","Nov","Dec")) +
      scale_y_continuous_hua(limits=c(0, 2500)) +
      annotate("rect", xmin = 3, xmax = 7, ymin = 0, ymax = 6000,
               alpha = .05, fill='darkred') +
      annotate("rect", xmin = 10, xmax = 12, ymin = 0, ymax = 6000,
               alpha = .05, fill='darkred') +
      theme_hua()
    ggplotly(p)
  })
  
  #--------- Traffic Accident by Speed_Zone ----------
  output$freqpolyPlotSpeed_Zone = renderPlotly({
      
    p <- ggplot(data_all) +
      geom_freqpoly(aes(x=SPEED_ZONE, color=REGION_NAME), stat='count') +
      labs(title='Traffic Accident by Speed Zone',
           x='Speed Zone', 
           y='Count') + 
      scale_color_discrete(name=NULL, labels=Speed) +
      geom_vline(xintercept = 60, colour='grey', linetype='longdash') +
      geom_vline(xintercept = 80, colour='grey', linetype='longdash') +
      geom_vline(xintercept = 100, colour='grey', linetype='longdash') +  
      scale_y_continuous_hua(limits = c(0, 12000)) + 
      theme_hua()
    
    ggplotly(p)
  })
  
  #--------- Traffic Accident by weekday ----------
  output$freqpolyPlotHour = renderPlotly({
    
    p <- ggplot(data_all) +
      geom_freqpoly(aes(x = weekday ,color= REGION_NAME), stat = 'count') +
      labs(title='Traffic Accident by weekdays',
           x='Weekday',
           y='Count') + 
      scale_color_discrete(name=NULL, labels = c('Mon', 'Tue', 'Wed', 
                                                 'Thu','Fri', 'Sat', 'Sun')) +
      # use scale_x/y_continous(limits=..) to exactly control x,y lim:
      scale_x_continuous_hua(limits = c(1, 7),
                             breaks = 1:7, 
                             labels=c('Mon', 'Tue', 'Wed', 
                                      'Thu','Fri', 'Sat', 'Sun')) +
      geom_vline(xintercept = 6, colour='grey', linetype='longdash') +
      scale_y_continuous_hua(limits = c(0, 4500)) + 
      theme_hua()
    
    ggplotly(p)
  })  
  #===================================================
  #=========     Severity & Speed tab      =========== 
  #===================================================
  
  #-------- collisions by year and severity -------
  #         not quite interesting --> may use pie-chart or table...
  output$barPlotYearSeverity = renderPlotly({
    
    # check related input:
    if (input$plotSeverityLGA != 'All') {
      df <- data_all %>%
        filter(LGA_NAME == input$plotSeverityLGA)
    } else {
      df <- data_all
    }
    
    p <- ggplot(data=df) +
      geom_bar(aes(x=year, fill=SEVERITY), position='dodge', width=0.6) +
      # change plot title based on user input
      labs(title = paste0("Traffic Accident in ", input$plotSeverityLGA, " Area by Severity over Years"),
           x = "Year", 
           y = 'Count') +
      scale_fill_manual(values=c("#00CC66", "#3399FF",'red'), name=NULL, 
                        labels=Sevr) + 
      scale_y_continuous_hua() +
      theme_hua() +
      scale_fill_brewer(palette='Set2', name=NULL)
    ggplotly(p)
  })
  
  #---------- collisions by year and type of victims ------
  output$barPlotYearSpeed = renderPlotly({
    
    # check related input:
    if (input$plotSpeedLGA != 'All') {
      df2 <- data_all %>%
        filter(LGA_NAME == input$plotSpeedLGA,
               SPEED_ZONE != 75)
    } else {
      df2 <- data_all %>%
        filter(SPEED_ZONE != 75)
    }
    
    p2 <- ggplot(data=df2) +
      geom_bar(aes(x = year, fill = as.factor(SPEED_ZONE)), position='dodge', width=0.8) +
      # change plot title based on user input
      labs(title = paste0("Traffic Accident in ", input$plotSpeedLGA, " Area by Speed Zone over Years"),
           x = "year", 
           y = 'Countt') +
      scale_fill_brewer(palette='Set1', name=NULL) + 
      scale_y_continuous_hua() +
      theme_hua() 
      
    ggplotly(p2)
  })
  
  #------- Bar chart severity composition --------
  output$PlotSeverity = renderPlotly({
    
    # check related input:
    if (input$plotSeverityLGA != 'All') {
      df <- data_all %>%
        filter(LGA_NAME == input$plotSeverityLGA)
    } else {
      df <- data_all
    }
      
    if (input$plotSeverityYear != 'All') {
      df2 <- df %>%
        filter(year == input$plotSeverityYear)
    } else {
      df2 <- df
    }  
    
    pie_data <- df2 %>% 
      group_by(SEVERITY) %>%
      count(sort = TRUE) %>%
      ungroup()
    pie_data <- pie_data %>%  mutate(prop = percent(n / sum(n)),
                                     SEVERITY = as.factor(SEVERITY),
                                     prop = as.factor(prop))

    p <- ggplot(pie_data) +
      geom_col(aes(x= SEVERITY, y = prop,fill=SEVERITY), position='dodge', width=0.6) +
      # change plot title based on user input
      labs(title = paste0("Accident Types in ", input$plotSeverityLGA, " Area"),
           x = "Accident Type", 
           y = 'Percent') +
      theme_hua() +
      scale_fill_brewer(palette='Set2', name=NULL) +
      theme(axis.text.x = element_blank())   
    
    ggplotly(p)
  })
  
  #------- Bar chart Speed composition --------
  output$piePlotSpeed = renderPlotly({
    
    # check related input:
    if (input$plotSpeedLGA != 'All') {
      df <- data_all %>%
        filter(LGA_NAME == input$plotSpeedLGA,
               SPEED_ZONE != 75)
    } else {
      df <- data_all %>%
        filter(SPEED_ZONE != 75)
    }
    
    if (input$plotSpeedYear != 'All') {
      df2 <- df %>%
        filter(year == input$plotSpeedYear)
    } else {
      df2 <- df
    }  
    
    pie_data <- df2 %>% 
      group_by(SPEED_ZONE) %>%
      count(sort = TRUE) %>%
      ungroup()
    pie_data <- pie_data %>%  mutate(prop = percent(n / sum(n)),
                                     SPEED_ZONE = as.factor(SPEED_ZONE),
                                     prop = as.factor(prop))
    
    p <- ggplot(pie_data) +
      geom_col(aes(x= SPEED_ZONE, y = prop,fill=SPEED_ZONE), position='dodge', width=0.6) +
      # change plot title based on user input
      labs(title = paste0("Accident Types in ", input$plotSpeedLGA, " Area"),
           x = "Speed Zone", 
           y = 'Percent') +
      theme_hua() +
      scale_fill_brewer(palette='Set1', name=NULL) +
      theme(axis.text.x = element_blank())
    
    ggplotly(p)
  })
}