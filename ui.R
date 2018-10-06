# Define UI for application
shinyUI(fluidPage(
  

    fluidRow(
      column(12, align="center",
      img(src = "CGS_logo.png", height = 42, width = 54),
      
      titlePanel("Counties and States Labor Market Monitor"),      
             
      selectInput("state",
                  "Select the state:",
                  choices = unique(df_states$ST),
                  selected = "CA"))),
    
      div(sliderInput("date_maps",
                  "Choose the Month and the Year of analysis:",
                  min = min(df_counties$date) + years(2), # Add two years, because there's no way to evaluate the tracer in the first two years
                  max = max(df_counties$date),
                  value = max(df_counties$date),
                  timeFormat = "%Y-%m",
                  width = "90%",
                  step = 31, # Number of days in the slider variation
                  animate = animationOptions(interval = 2500, loop = FALSE)), align = "center"),
    
    hr(),
    
    # Show the plots
    fluidRow(
       column(6,
               leafletOutput("states_map"),
               br(),
               leafletOutput("counties_map")
            ),
       column(6,
              plotlyOutput("states_monitor"),
              br(),
              plotlyOutput("counties_monitor")
              )
    )
))
