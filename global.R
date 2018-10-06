library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)
library(data.table)
library(rmapshaper)
library(lubridate)

# To avoid masking
select <- dplyr::select
filter <- dplyr::filter

county_corresp <- readRDS('county_corresp.rds')

df_states    <- readRDS("df_states.rds")
df_counties  <- readRDS("df_counties.rds")
map_counties <- readRDS("map_counties_simplified.rds") # ms_simplify(readRDS("map_counties.rds"))
map_states   <- readRDS("map_states_simplified.rds") # ms_simplify(readRDS("map_states.rds"))


