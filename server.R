# Define server logic
shinyServer(function(input, output, session) {
  
  output$overview_title <- renderText({
    
    input_year  <- lubridate::year(ymd(input$date_maps))
    input_month <- lubridate::month(ymd(input$date_maps))
    
    paste0("Overview of ", return_month(input_month), ", ", input_year)
    
  })


source('leaflets_server.R', local = TRUE)
source('plotly_dynamics_server.R', local = TRUE)

  
})
