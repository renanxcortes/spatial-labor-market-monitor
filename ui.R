# Define UI for application
shinyUI(fluidPage(
  
  titlePanel("Counties and States Labor Market Monitor"),
  
  sidebarLayout(
    sidebarPanel(
       
       selectInput("state",
                   "Select the state:",
                   choices = unique(df_states$State),
                   selected = "CA")
    ),
    
    # Show the plots
    mainPanel(
       leafletOutput("map"),
       br(),
       plotlyOutput("state_monitor")
    )
  )
))
