# Code to extract data from FRED
library(alfred)
library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)
library(data.table)
library(BBmisc)
library(rgdal)
library(leaflet)
library(jsonlite)

# To avoid masking
select <- dplyr::select
filter <- dplyr::filter

# Auxiliary Functions
calculate_acum_growth_S_months    <- function(x, s) { aux <- rollapply(x, width = s, FUN = sum); return(c(rep(NA, s - 1),(aux / lag(aux, s) - 1) * 100)) }
calc_ace <- function(x) {aux <- diff(x); return(c(NA, aux))}

return_month <- Vectorize(function(num_mon) {switch(as.character(num_mon),
                                                   "1" = "January",
                                                   "2" = "February",
                                                   "3" = "March",
                                                   "4" = "April",
                                                   "5" = "May",
                                                   "6" = "June",
                                                   "7" = "July",
                                                   "8" = "August",
                                                   "9" = "September",
                                                   "10" = "October",
                                                   "11" = "November",
                                                   "12" = "December")})

return_state <- Vectorize(function(state_initial) {switch(as.character(state_initial),
                                                          "AL" = "Alabama",
                                                          "AK" = "Alaska",
                                                          "AZ" = "Arizona",
                                                          "AR" = "Arkansas",
                                                          "CA" = "California",
                                                          "CO" = "Colorado",
                                                          "CT" = "Connecticut",
                                                          "DE" = "Delaware",
                                                          "DC" = "District of Columbia",
                                                          "FL" = "Florida",
                                                          "GA" = "Georgia",
                                                          "HI" = "Hawaii",
                                                          "ID" = "Idaho",
                                                          "IL" = "Illinois",
                                                          "IN" = "Indiana",
                                                          "IA" = "Iowa",
                                                          "KS" = "Kansas",
                                                          "KY" = "Kentucky",
                                                          "LA" = "Louisiana",
                                                          "ME" = "Maine",
                                                          "MD" = "Maryland",
                                                          "MA" = "Massachusetts",
                                                          "MI" = "Michigan",
                                                          "MN" = "Minnesota",
                                                          "MS" = "Mississippi",
                                                          "MO" = "Missouri",
                                                          "MT" = "Montana",
                                                          "NE" = "Nebraska",
                                                          "NV" = "Nevada",
                                                          "NH" = "New Hampshire",
                                                          "NJ" = "New Jersey",
                                                          "NM" = "New Mexico",
                                                          "NY" = "New York",
                                                          "NC" = "North Carolina",
                                                          "ND" = "North Dakota",
                                                          "OH" = "Ohio",
                                                          "OK" = "Oklahoma",
                                                          "OR" = "Oregon",
                                                          "PA" = "Pennsylvania",
                                                          "RI" = "Rhode Island",
                                                          "SC" = "South Carolina",
                                                          "SD" = "South Dakota",
                                                          "TN" = "Tennessee",
                                                          "TX" = "Texas",
                                                          "UT" = "Utah",
                                                          "VT" = "Vermont",
                                                          "VA" = "Virginia",
                                                          "WA" = "Washington",
                                                          "WV" = "West Virginia",
                                                          "WI" = "Wisconsin",
                                                          "WY" = "Wyoming",
                                                          "US" = "United States")})



extract_4_upper <- function(x) {x %>% str_replace_all(" ", "") %>% toupper %>% str_sub(1, 4)}

# Maps
map_STATES   <- readOGR("C:\\Users\\renan\\Desktop\\CSI Project\\cb_2017_us_state_500k", 'cb_2017_us_state_500k')
map_COUNTIES <- readOGR("C:\\Users\\renan\\Desktop\\CSI Project\\tl_2017_us_county", 'tl_2017_us_county')


# County correspondence table and List of US States initials
state_init_correp <- readRDS("state_init_corresp.rds") %>% mutate(ST_Initial = as.character(ST_Initial))

county_corresp <- tbl_df(fread("national_county.txt",
                               col.names = c("STATE", "STATEFP", "COUNTYFP", "COUNTYNAME", "CLASSFP"))) %>%
                  mutate(COUNTYFP = str_pad(COUNTYFP, width = 3, pad = "0"),
                         CN4 = extract_4_upper(COUNTYNAME),
                         STCN4 = paste0(STATE, CN4)) %>%
                  inner_join(state_init_correp, by = c("STATE" = "ST_Initial"))

states_initials <- unique(county_corresp$STATE)

# United States first (all join will be after the first grab)
string_grab = c("UNRATENSA", paste0(states_initials, "URN"))

# Inspired in https://stackoverflow.com/questions/11254524/omit-rows-containing-specific-column-of-na
# Just modified a little bit in order to use the column name "dplyr's wise"
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


get_fred_series_paese <- function(series_id, series_name = NULL,
                                  observation_start = NULL, observation_end = NULL) {
  
  
  length_series_id <- nchar(series_id)
  
  if (is.character(series_id) == FALSE) {
    stop("series_id is always in characters")
  }
  
  if (is.null(series_name) == TRUE ) {
    series_name <- series_id
  }
  
  if (is.null(observation_start) == TRUE) {
    observation_start <- "1776-07-04"
  }
  
  if (is.null(observation_end) == TRUE) {
    observation_end <- "9999-12-31"
  }
  
  df_series <-
    try({
      fromJSON(
        paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
               series_id,
               "&observation_start=",
               observation_start,
               "&observation_end=",
               observation_end,
               "&output_type=2",
               "&api_key=98f9f5cad7212e246dc5955e9b744b24&file_type=json")
      )$observations %>%
        mutate_(date = ~as_date(date))
    }, silent = TRUE)
  
  print(class(df_series))
  
  if (!class(df_series) == "try-error") {
    colnames(df_series)[!(colnames(df_series) %in% "date")] <- series_name
    df_series[, 2] <- as.numeric(unlist(df_series[, 2]))
    df_series
    
  } else {
    df_series <- tibble(date = as.Date(NA))
  }
  
}

create_df <- function(i) {
      x <- get_fred_series_paese(string_grab[i])
      if (!is.error(x)) # Even if FRED doesn't the series, it won't stop
      {x} else 
      {tibble(date = as.Date(NA))}}

data_states = 1:length(string_grab) %>% 
       map(create_df) %>% 
       reduce(full_join, by = "date") %>%
       completeFun("date")


data_clean_states <- data_states %>%
              rename(USURN = UNRATENSA) %>%
              gather(Raw_Type, UR, -date) %>%
              separate(Raw_Type, into = c("ST", "Seasonal_Type"), sep = 2) %>%
              mutate(year = year(date),
                     month = month(date)) %>%
              group_by(ST, Seasonal_Type) %>%
              mutate(Var = calculate_acum_growth_S_months(UR, 12),
                     Ace = calc_ace(Var))


base_aux_states = data_clean_states %>%
              filter(Seasonal_Type == "URN",
                     year == 2018,
                     month == 1) %>%
              arrange(ST)


# State Spatial Unemployment tracer of a specific month and year
base_aux_states %>%
  plot_ly(x = ~Ace, 
          y = ~Var, 
          type = 'scatter', 
          mode = 'markers', 
          marker = list(size = ifelse(base_aux_states$ST == "US", 20, 10),
                        #sizeref = .10, 
                        color = ifelse(base_aux_states$ST == "US", "red","#004B82")#,
                        #symbol = 1:length(base_aux$ST)
          ),  
          hoverinfo = "text",
          text = paste("", base_aux_states$ST, "<br>",
                       "Growth: ", round(base_aux_states$Var, 3), "<br>",
                       "Acelleration: ", round(base_aux_states$Ace, 3)),
          showlegend = TRUE) %>%
  layout(title = paste0("Labor Unemployment Rate tracer: States of US"))





# The unemployment rate and civilian labor force is id = 116: https://fred.stlouisfed.org/release?rid=116

# Offset is a parameter to start in the middle of the search! Very important parameter.

#labor_string_json <- "https://api.stlouisfed.org/fred/release/series?release_id=116&limit=20&snid=253717&api_key=6341430c553dbddb816b619d36198e0f&file_type=json"

#labor_string_json <- "https://api.stlouisfed.org/fred/tags?tags_name='County;Ca'&api_key=6341430c553dbddb816b619d36198e0f&file_type=json"

# THERE'S A LIMIT of requests of the FRED's API (1000 series): https://research.stlouisfed.org/docs/api/fred/category_series.html#limit

# MAJOR PROBLEM: for example, California has two counties initiating with MIRA, that's when the FRED's API got messy.


# Loop that takes ALL series ID of the labor market (release_id = 116)
complete_id_string <- character()
titles_string <- character()
aux <- c(0)
i = 0
while(length(aux) > 0) {
  labor_string_json <- paste0("https://api.stlouisfed.org/fred/release/series?release_id=116&offset=",i,"&api_key=6341430c553dbddb816b619d36198e0f&file_type=json")
  get_data = fromJSON(url(labor_string_json))
  complete_id_string <- c(complete_id_string, get_data$seriess$id)
  titles_string <- c(titles_string, get_data$seriess$title)
  
  aux <- get_data$seriess
  i <- i + 1000
  print(paste0("Offset begins with: ", i))
}

region_names <- str_match(titles_string, " in (.*?), ")[,2]
corresp_aux <- tibble(series_id = complete_id_string, Desc_Reg = region_names)







# Start of constructing graphs for a specific State



state_selected <- "CA"


state_county_ur = complete_id_string[str_sub(complete_id_string, 1, 2) == state_selected & str_sub(complete_id_string, nchar(complete_id_string)-2, nchar(complete_id_string)) == "URN"]


create_df_county <- function(i) {
  x <- get_fred_series_paese(state_county_ur[i])
  if (!is.error(x)) # Even if FRED doesn't the series, it won't stop
  {x} else 
  {tibble(date = as.Date(NA))}}

data_county = 1:length(state_county_ur) %>% 
        map(create_df_county) %>% 
        reduce(full_join, by = "date") %>%
        completeFun("date")

data_clean_county <- data_county %>%
              gather(Raw_Type, UR, -date) %>%
              mutate(series_id = Raw_Type) %>%
              separate(Raw_Type, into = c("ST", "CN4",  "FRED_NUMBER", "VAR"), sep = c(2,6,7)) %>%
              select(-FRED_NUMBER, -VAR) %>%
              mutate(year = year(date),
                     month = month(date),
                     STCN4 = paste0(ST, CN4)) %>%
              group_by(STCN4) %>%
              mutate(Var = calculate_acum_growth_S_months(UR, 12),
                     Ace = calc_ace(Var)) %>%
              full_join(corresp_aux, on = c("series_id"))
              


base_aux_county = data_clean %>%
            filter(year == 2018,
                   month == 1) %>%
            arrange(Desc_Reg)


# Spatial Unemployment tracer for counties level
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
          text = paste("", base_aux$Desc_Reg, "<br>",
                       "Growth: ", round(base_aux$Var, 3), "<br>",
                       "Acelleration: ", round(base_aux$Ace, 3)),
          showlegend = TRUE) %>%
  layout(title = paste0("Labor Unemployment Rate tracer: Counties of ", state_selected))




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





# Constructing an entire counties data base :

counties_df <- tibble()
for (i in 1:length(states_initials)){
  
  state_selected = states_initials[i]
  
  state_county_ur = complete_id_string[str_sub(complete_id_string, 1, 2) == state_selected & str_sub(complete_id_string, nchar(complete_id_string)-2, nchar(complete_id_string)) == "URN"]
  
  create_df_county <- function(i) {
    x <- get_fred_series_paese(state_county_ur[i])
    if (!is.error(x)) # Even if FRED doesn't the series, it won't stop
    {x} else 
    {tibble(date = as.Date(NA))}}
  
  data_county = 1:length(state_county_ur) %>% 
    map(create_df_county) %>% 
    reduce(full_join, by = "date") %>%
    completeFun("date")
  
  data_clean_county <- data_county %>%
    gather(Raw_Type, UR, -date) %>%
    mutate(series_id = Raw_Type) %>%
    separate(Raw_Type, into = c("ST", "CN4",  "FRED_NUMBER", "VAR"), sep = c(2,6,7)) %>%
    select(-FRED_NUMBER, -VAR) %>%
    mutate(year = year(date),
           month = month(date),
           STCN4 = paste0(ST, CN4)) %>%
    group_by(STCN4) %>%
    mutate(Var = calculate_acum_growth_S_months(UR, 12),
           Ace = calc_ace(Var)) %>%
    full_join(corresp_aux, on = c("series_id"))
  
  counties_df <- bind_rows(counties_df,  data_clean_county) %>% 
                 completeFun("ST")
  print(i)
  
  
}

# Saving Objects
county_corresp_ok <- county_corresp %>% select(STATE, ST_Name)

data_clean_states_ok <- data_clean_states %>% 
                        mutate(ST_Name = return_state(ST)) %>%
                        ungroup() %>%
                        select(-Seasonal_Type)

df_counties_ok  <- df_counties %>%
                   select(-CN4, -series_id, -STCN4) %>%
                   mutate(ST_Name = return_state(ST))
                   

saveRDS(data_clean_states_ok, "df_states.rds")
saveRDS(df_counties_ok, "df_counties.rds")
saveRDS(map_COUNTIES, "map_counties.rds")
saveRDS(map_STATES, "map_states.rds")















# Working Age Population Grab Data
string_grab = c("POPTOTUSA647NWDB", paste0(states_initials, "POP"))

create_df <- function(i) {
              x <- get_fred_series_paese(string_grab[i])
              if (!is.error(x)) # Even if FRED doesn't the series, it won't stop
              {x} else 
              {tibble(date = as.Date(NA))}}

data_states_pop = 1:length(string_grab) %>% 
                  map(create_df) %>% 
                  reduce(full_join, by = "date") %>%
                  completeFun("date")


data_clean_states_pop <- data_states_pop %>%
                         gather(series_id, Pop, -date) %>%
                         separate(series_id, into = c("ST", "Rest"), sep = c(2)) %>%
                         mutate(year = year(date),
                                ST = ifelse(ST == "PO", "US", ST)) %>%
                         select(-Rest, -date) %>%
                         arrange(ST, year)
  
                         

