library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)
library(data.table)
library(rmapshaper)
library(lubridate)
library(mFilter)

# To avoid masking
select <- dplyr::select
filter <- dplyr::filter

county_corresp <- readRDS('county_corresp.rds')

df_states_pre    <- readRDS("df_states.rds")
df_counties_pre  <- readRDS("df_counties.rds")
map_counties <- readRDS("map_counties_simplified.rds") # ms_simplify(readRDS("map_counties.rds"))
map_states   <- readRDS("map_states_simplified.rds") # ms_simplify(readRDS("map_states.rds"))


states_pop   <- readRDS("states_yearly_population.rds")
counties_pop <- readRDS("counties_yearly_population.rds")

df_states <- df_states_pre %>%
             left_join(states_pop, by = c("ST" = "ST", "year" = "year"))

df_counties <- df_counties_pre %>%
               #select(-ST) %>%
               left_join(counties_pop, by = c("ST" = "ST", "Desc_Reg" = "Desc_Reg", "year" = "year"))

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





# To inspect app performance:
# library(profvis)
# profvis({
#   runApp("path_to_app", display.mode = "normal")
# })