counties_monitor_data <- reactive({
  
  state_selected <- input$state
  input_year  <- lubridate::year(ymd(input$date_maps))
  input_month <- lubridate::month(ymd(input$date_maps))
  
  base_aux_benchm <- df_states %>%
    filter(year == input_year,
           month == input_month,
           ST_Name == state_selected) %>%
    add_column(Desc_Reg = as.character(state_selected), .before = "ST_Name")
  
  base_aux_county_pre <- df_counties %>%
    filter(year == input_year,
           month == input_month,
           ST_Name == state_selected) %>%
    bind_rows(base_aux_benchm) %>%
    mutate(aux_pop = sqrt(Pop)) %>%
    arrange(Desc_Reg)
  
  if (input$checkbox_include_benchmark){
    
    base_aux_county <- base_aux_county_pre 
    
  }
  
  if (!input$checkbox_include_benchmark){
    
    base_aux_county <- base_aux_county_pre %>%
      filter(Desc_Reg != state_selected) # In this case, we filter not the state name column!
    
  }
  
  return(base_aux_county)
  
})


states_monitor_data <- reactive({
  
  input_year  <- lubridate::year(ymd(input$date_maps))
  input_month <- lubridate::month(ymd(input$date_maps))
  
  base_aux_states_pre <- df_states %>%
    filter(year == input_year,
           month == input_month) %>%
    arrange(ST_Name) %>%
    mutate(aux_pop = sqrt(Pop)) #%>%
  #inner_join(select(tab_ok, -ST_Name), by = "ST")
  
  if (input$checkbox_include_benchmark){
    
    base_aux_states <- base_aux_states_pre
    
  }
  
  if (!input$checkbox_include_benchmark){
    
    base_aux_states <- base_aux_states_pre %>%
      filter(ST_Name != "United States")
  }
  
  return(base_aux_states)
  
})








output$counties_monitor <- renderPlotly({
  
  base_aux_county <- counties_monitor_data()
  
  #state_selected <- input$state
  
  opacidade_cor <- 0.10
  cores_estagios <- c("green", "yellow", "red", "orange")
  
  limite_cores_x <- 1.05 * max(abs(base_aux_county$Ace))
  limite_cores_y <- 1.05 * max(abs(base_aux_county$Var))
  f <- list(size = 14, color = "black")
  
  base_aux_county %>%
    plot_ly(source = "counties_monitor",
            x = ~Ace, 
            y = ~Var, 
            type = 'scatter', 
            mode = 'markers', 
            marker = list(size = ~base_aux_county$aux_pop,
                          sizeref = max(base_aux_county_pre$aux_pop, na.rm = T) * 0.01,
                          color = ifelse(base_aux_county$Desc_Reg == state_selected, "red","#004B82")
                          #symbol = 1:length(base_aux$State)
            ),  
            hoverinfo = "text",
            text = paste("", base_aux_county$Desc_Reg, "<br>",
                         "Growth: ", round(base_aux_county$Var, 3), "<br>",
                         "Acelleration: ", round(base_aux_county$Ace, 3)),
            showlegend = TRUE) %>%
    layout(title = paste0("Unemployment Rate Cycle: Counties of ", state_selected),
           
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
           
           
    ) %>%
    config(
      modeBarButtonsToRemove = list(
        'pan2d',
        'resetScale2d',
        'autoScale2d',
        'zoomIn2d',
        'zoomOut2d',
        'select2d',
        'zoom2d',
        'hoverClosestCartesian',
        'lasso2d',
        'toggleSpikelines',
        'sendDataToCloud'
      )
    )
  
})





output$states_monitor <- renderPlotly({
  
  base_aux_states <- states_monitor_data()
  
  opacidade_cor <- 0.10
  cores_estagios <- c("green", "yellow", "red", "orange")
  
  limite_cores_x <- 1.05 * max(abs(base_aux_states$Ace))
  limite_cores_y <- 1.05 * max(abs(base_aux_states$Var))
  f <- list(size = 14, color = "black")
  
  base_aux_states %>%
    plot_ly(source = "states_monitor",
            x = ~Ace,
            y = ~Var,
            type = 'scatter',
            mode = 'markers',
            #color = base_aux_states$wl,
            marker =
              list(#size = ifelse(base_aux_states$ST_Name == "United States", 20, 10),
                size = ~base_aux_states$aux_pop,
                sizeref =  max(base_aux_states_pre$aux_pop, na.rm = T) * 0.01, # 556.182 * 0.0075,
                color = ifelse(base_aux_states$ST_Name == "United States", "red","#004B82")#,
                #color = ~base_aux_states$wl
                #symbol = 1:length(base_aux$State)
              ),
            hoverinfo = "text",
            text = paste("", base_aux_states$ST_Name, "<br>",
                         "Pop. (1000 people): ", base_aux_states$Pop, "<br>",
                         "Growth: ", round(base_aux_states$Var, 3), "<br>",
                         "Acelleration: ", round(base_aux_states$Ace, 3)),
            showlegend = TRUE) %>%
    layout(title = paste0("Unemployment Rate Cycle: States of US"),
           
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
           
           
    ) %>%
    config(
      modeBarButtonsToRemove = list(
        'pan2d',
        'resetScale2d',
        'autoScale2d',
        'zoomIn2d',
        'zoomOut2d',
        'select2d',
        'zoom2d',
        'hoverClosestCartesian',
        'lasso2d',
        'toggleSpikelines',
        'sendDataToCloud'
      )
    )
  
})





output$hover <- renderPrint({
  
  clicked_county()
  
})


clicked_county <- reactive({
  
  d <- event_data("plotly_click", source = "counties_monitor")
  if (is.null(d)) return("Click not clicked") else 
    
    return(
      counties_monitor_data()[d[["pointNumber"]] + 1,]$Desc_Reg # Sum one because javascript is zero index based
    )
  
})




