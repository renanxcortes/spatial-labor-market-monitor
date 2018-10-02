library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)
library(data.table)

county_corresp <- readRDS('county_corresp.rds')

df_states    <- readRDS("df_states.rds")
df_counties  <- readRDS("df_counties.rds")
map_counties <- readRDS("map_counties.rds")
map_states   <- readRDS("map_states.rds")
