counties_map_data <- reactive({
  
  state_selected <- input$state
  input_year  <- lubridate::year(ymd(input$date_maps))
  input_month <- lubridate::month(ymd(input$date_maps))
  
  
  base_aux_county <- df_counties %>%
    filter(year == input_year,
           month == input_month,
           ST_Name == state_selected)# %>%
  #arrange(Desc_Reg)
  
  # Maps
  map_ST <- map_counties[map_counties$STATEFP == str_pad(unique(filter(county_corresp, ST_Name == state_selected)$STATEFP), 2, pad = "0"),]
  
  
  df_mapa <- merge(map_ST,
                   base_aux_county,
                   by.x = "NAMELSAD",
                   by.y = "Desc_Reg")
  
})


states_map_data <- reactive({
  
  input_year  <- lubridate::year(ymd(input$date_maps))
  input_month <- lubridate::month(ymd(input$date_maps))
  
  base_aux_state <- df_states %>%
    filter(year == input_year,
           month == input_month)
  
  # Maps
  df_mapa <- merge(map_states,
                   base_aux_state,
                   by.x = "STUSPS",
                   by.y = "ST",
                   all.x = FALSE)
  
  
})





output$counties_map <- renderLeaflet({
  
  df_mapa <- counties_map_data()
  
  gradiente = colorNumeric(rev(c("#6A1103",
                                 "#BA0004",
                                 "#E7400B",
                                 "#FEA527",
                                 "#FDFE65",
                                 "#9CE400",
                                 "#359800")), domain = df_mapa$UR)
  
  leaflet(data = df_mapa) %>%
    addTiles() %>%
    
    addPolygons(data = df_mapa[df_mapa@data$NAMELSAD == clicked_county(),], 
                fill = FALSE, 
                color = '#141400',
                opacity = 10,
                layerId = 'sel_county') %>%
    
    addPolygons(weight = 0.5, # Border thichkness
                fillColor = ~gradiente(df_mapa$UR),
                color = "grey", # Border color
                fillOpacity = 0.5, # Transparency
                smoothFactor = 0.25,
                popup = paste0("<b>",df_mapa$NAMELSAD,"</b>", "<br>",
                               "Rate: ", df_mapa$UR, "<br>",
                               "Var.: ", round(df_mapa$Var,2), "<br>",
                               "Ace.: ", round(df_mapa$Ace,2))) %>%
    
    addLegend(position = "bottomright",
              pal = gradiente,
              values = ~UR)
  
})




output$states_map <- renderLeaflet({
  
  df_mapa <- states_map_data()
  
  gradiente = colorNumeric(rev(c("#6A1103",
                                 "#BA0004",
                                 "#E7400B",
                                 "#FEA527",
                                 "#FDFE65",
                                 "#9CE400",
                                 "#359800")), domain = df_mapa$UR)
  
  leaflet(data = df_mapa) %>%
    addTiles() %>%
    addPolygons(weight = 0.5, # Border thichkness
                fillColor = ~gradiente(df_mapa$UR),
                color = "grey", # Border color
                fillOpacity = 0.5, # Transparency
                smoothFactor = 0.25,
                popup = paste0("<b>",df_mapa$ST_Name,"</b>", "<br>",
                               "Rate: ", df_mapa$UR, "<br>",
                               "Var.: ", round(df_mapa$Var,2), "<br>",
                               "Ace.: ", round(df_mapa$Ace,2))) %>%
    addLegend(position = "bottomright",
              pal = gradiente,
              values = ~UR) %>%
    setView(-100, 38, zoom = 3.5)
  
})






county_map_click <- eventReactive(input$counties_map_shape_click, {
  
  x <- input$counties_map_shape_click # "_shape_click" is created internally by leaflet
  
  y <- x$id
  
  return(y)
  
})

observe({
  
  updateSelectInput(session, 'counties_monitor', selected = county_map_click())
  
})

