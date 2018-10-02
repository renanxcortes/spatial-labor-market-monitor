# Define server logic
shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    
    state_selected <- input$state
    
    base_aux_county = df_counties %>%
                      filter(year == 2018,
                             month == 1,
                             ST == state_selected) %>%
                      arrange(Desc_Reg)
    
    # Maps
    map_ST <- map_COUNTIES[map_COUNTIES$STATEFP == str_pad(unique(filter(county_corresp, STATE == state_selected)$STATEFP), 2, pad = "0"),]
    
    
    df_mapa <- merge(map_ST, 
                     base_aux_county, 
                     by.x = "NAMELSAD",
                     by.y = "Desc_Reg")
    
    
    gradiente = colorNumeric(rev(c("#6A1103", 
                                   "#BA0004", 
                                   "#E7400B", 
                                   "#FEA527", 
                                   "#FDFE65", 
                                   "#9CE400", 
                                   "#359800")), domain = df_mapa$UR)
    
    leaflet(data = df_mapa) %>% 
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
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
  
  output$state_monitor <- renderPlotly({
    
    state_selected <- input$state
    
    base_aux_county = df_counties %>%
                      filter(year == 2018,
                             month == 1,
                             ST == state_selected) %>%
                      arrange(Desc_Reg)
    
    opacidade_cor <- 0.15
    cores_estagios <- c("red", "yellow", "green", "orange")
    
    limite_cores_x <- 1.05 * max(abs(base_aux_county$Ace))
    limite_cores_y <- 1.05 * max(abs(base_aux_county$Var))
    f <- list(size = 14, color = "black")
    
    base_aux_county %>%
      plot_ly(x = ~Ace, 
              y = ~Var, 
              type = 'scatter', 
              mode = 'markers', 
              marker = list(#size = ifelse(base_aux$State == "US", 20, 10),
                #sizeref = .10, 
                #color = ifelse(base_aux$State == "US", "red","#004B82")#,
                #symbol = 1:length(base_aux$State)
              ),  
              hoverinfo = "text",
              text = paste("", base_aux_county$Desc_Reg, "<br>",
                           "Growth: ", round(base_aux_county$Var, 3), "<br>",
                           "Acelleration: ", round(base_aux_county$Ace, 3)),
              showlegend = TRUE) %>%
              layout(title = paste0("Labor Unemployment Rate tracer: Counties of ", state_selected),
                     
                     shapes = list(
                       list(type = "rect",
                            fillcolor = cores_estagios[1], line = list(color = cores_estagios[1]), opacity = opacidade_cor,
                            x0 = -limite_cores_x, x1 = 0, xref = "x",
                            y0 = -limite_cores_y, y1 = 0, yref = "y",
                            layer = "below"),
                       list(type = "rect",
                            fillcolor = cores_estagios[2], line = list(color = cores_estagios[2]), opacity = opacidade_cor,
                            x0 = -limite_cores_x, x1 = 0, xref = "x",
                            y0 = 0, y1 = limite_cores_y, yref = "y",
                            layer = "below"),
                       list(type = "rect",
                            fillcolor = cores_estagios[3], line = list(color = cores_estagios[3]), opacity = opacidade_cor,
                            x0 = 0, x1 = limite_cores_x, xref = "x",
                            y0 = 0, y1 = limite_cores_y, yref = "y",
                            layer = "below"),
                       list(type = "rect",
                            fillcolor = cores_estagios[4], line = list(color = cores_estagios[4]), opacity = opacidade_cor,
                            x0 = 0, x1 = limite_cores_x, xref = "x",
                            y0 = -limite_cores_y, y1 = 0, yref = "y",
                            layer = "below")),
                     
                     xaxis = list(
                       zeroline = FALSE,
                       showline = FALSE,
                       showgrid = FALSE,
                       range = c(-limite_cores_x, limite_cores_x),
                       title = "Acceleration (p.p.)",
                       titlefont = f
                     ),
                     
                     yaxis = list(
                       zeroline = FALSE,
                       showline = FALSE,
                       showgrid = FALSE,
                       range = c(-limite_cores_y, limite_cores_y),
                       title = "Growth (%)",
                       titlefont = f
                     )
                     
                     
                     )
  
  })
  
})
