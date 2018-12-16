# Define UI for application
shinyUI(fluidPage(
  
  theme = shinytheme("readable"), # readable
  # cerulean, cosmo, cyborg, darkly, flatly, journal, lumen
  
             
             
   fluidRow(
     column(12, align="center",
            img(src = "CGS_logo.png", height = 42, width = 54),
            
            titlePanel("Counties and States Unemployment Monitor (CSUM)"))),
             
    navbarPage("CSUM",         
             
    tabPanel("Presentation",
             
             
             sidebarLayout(
               sidebarPanel(
                 h2("CSUM"),
                 p("Short Description of CSUM"),
                 br(),
                 br(),
                 img(src = "CGS_logo.png", height = 132, width = 162), 
                 p('Center for Geospatial Sciences'),
                 br(),
                 br(),
                 br(),
                 "This dashboard was made using Shiny", 
                 span("RStudio", style = "color:blue"),".", 
                 br(), 
                 br(),
                 img(src = "RStudio_ball.png", height = 82, width = 92)
               ),
               mainPanel(
                 h1("Main Title"),
                 p("Paragraphs"),
               br(),
               br(),
               h2("Features"),
               p("* F1."),
               p("* F2."),
               br(),
               h3("Contact:"),
               p("Renan Xavier Cortes ",
                 a("(CONTACT)", 
                   href = "https://spatial.ucr.edu/peopleCortes.html", target="_blank")),
               br(),
               div(img(href = "http://creativecommons.org/licenses/by/4.0/", src="https://i.creativecommons.org/l/by/4.0/88x31.png"), align = "center"),
               div(p("License"), align = "center"),
               div(a("Creative Commons 4.0",
                     href = "http://creativecommons.org/licenses/by/4.0/", target="_blank"), align = "center")
             ))

             ),
             
    tabPanel("Maps and Macroeconomics Dynamics", 
             
      verbatimTextOutput("hover"),
    
      div(sliderInput("date_maps",
                  "Choose the Month and the Year of analysis:",
                  min = min(df_counties$date) + years(20), # years(2), # Add two years, because there's no way to evaluate the tracer in the first two years
                  max = max(df_counties$date),
                  value = max(df_counties$date),
                  timeFormat = "%Y-%m",
                  width = "90%",
                  step = 31, # Number of days in the slider variation
                  animate = animationOptions(interval = 1500, loop = FALSE)), align = "center"),


    
    div(h3(textOutput("overview_title")), align = "center"),
    
    hr(),
    
    fluidRow(
      column(12, align="center",
             
             selectInput("state",
                         "Select a state:",
                         choices = unique(df_states$ST_Name)[unique(df_states$ST_Name) != "United States"],
                         selected = "California"),
             
             checkboxInput("checkbox_include_benchmark", label = "Include regional benchmarks", value = FALSE)
             
             )),
    
    br(),
    
    # Show the plots
    fluidRow(
       column(6,
              leafletOutput("counties_map"),
              br(),
              leafletOutput("states_map")
              
               
            ),
       column(6,
              plotlyOutput("counties_monitor"),
              br(),
              plotlyOutput("states_monitor")
              
              
              )
    )
    ))
))
