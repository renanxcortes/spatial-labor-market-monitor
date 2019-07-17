# This code tries to create the labels of either leading, lagging or coincident for the spatial units of the app
# Draw inspiration from Colombo, J. A., Cortes, R. X., Cruz, F. I., and Paese, L. H. (2018). Building State-Level Business Cycle Tracer Tools: Evidence from a Large Emerging Economy. International Journal of Economics and Finance, 10(5), 14.

library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)
library(data.table)
library(rmapshaper)
library(lubridate)

library(zoo)
library(mFilter)

calculate_acum_growth_S_months    <- function(x, s) { aux <- rollapply(x, width = s, FUN = sum); return(c(rep(NA, s - 1),(aux / lag(aux, s) - 1) * 100)) }


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
             left_join(states_pop, by = c("ST" = "ST", "year" = "year")) #%>%
             #filter(year >= 2000, year <= 2011)

df_counties <- df_counties_pre %>%

               left_join(counties_pop, by = c("ST" = "ST", "Desc_Reg" = "Desc_Reg", "year" = "year"))



# Determining each class
df_aux <- df_states %>%
          filter(ST %in% c("US", "HI")) %>%
          select(date, ST, UR) %>%
          spread(ST, UR) %>%
          na.omit()



states_initials_without_US <- str_subset(unique(df_states$ST), "[^(US)]")

hs <- data.frame(ST      = NA, 
                 level   = NA,
                 diff    = NA,
                 acum_12 = NA,
                 acum_24 = NA,
                 d_hp    = NA)

whs <- data.frame(ST      = NA, 
                  level   = NA,
                  diff    = NA,
                  acum_12 = NA,
                  acum_24 = NA,
                  d_hp    = NA)


for(i in 1:length(states_initials_without_US)) {
  
  st <- states_initials_without_US[i]
  
  hs[i, "ST"] = st
  whs[i, "ST"] = st
  
  df_aux <- df_states %>%
            filter(ST %in% c("US", st)) %>%
            select(date, ST, UR) %>%
            spread(ST, UR) %>%
            na.omit()
  
  bench <- select(df_aux, US)
  curre <- select(df_aux, st)
  
  # Level
  ccs <- ccf(bench, curre)
  h   <- ccs$lag[which.max(abs(ccs$acf))]
  wh  <- weighted.mean(ccs$lag, abs(ccs$acf))
  hs[i, "level"] = h
  whs[i, "level"] = wh
  
  # Diff
  ccs <- ccf(diff(pull(bench)), diff(pull(curre)))
  h <- ccs$lag[which.max(abs(ccs$acf))]
  wh  <- weighted.mean(ccs$lag, abs(ccs$acf))
  hs[i, "diff"] = h
  whs[i, "diff"] = wh
  
  # Acumulated 12 
  ccs <- ccf(na.omit(calculate_acum_growth_S_months(pull(bench), s = 12)), 
             na.omit(calculate_acum_growth_S_months(pull(curre), s = 12)))
  
  h <- ccs$lag[which.max(abs(ccs$acf))]
  wh  <- weighted.mean(ccs$lag, abs(ccs$acf))
  hs[i, "acum_12"] = h
  whs[i, "acum_12"] = wh
  
  # Acumulated 24 
  ccs <- ccf(na.omit(calculate_acum_growth_S_months(pull(bench), s = 24)), 
             na.omit(calculate_acum_growth_S_months(pull(curre), s = 24)))
  
  h <- ccs$lag[which.max(abs(ccs$acf))]
  wh  <- weighted.mean(ccs$lag, abs(ccs$acf))
  hs[i, "acum_24"] = h
  whs[i, "acum_24"] = wh
  
  # HP-Filter
  #ccs <- ccf(hpfilter(bench, freq = 12)$cycle, 
  #           hpfilter(curre, freq = 12)$cycle)
  
  # Double HP-Filter
  ccs <- ccf(hpfilter(hpfilter(bench, freq = 133107.938, type = "lambda")$cycle, freq = 13.9282, type = "lambda")$cycle, 
             hpfilter(hpfilter(curre, freq = 133107.938, type = "lambda")$cycle, freq = 13.9282, type = "lambda")$cycle)
  
  h <- ccs$lag[which.max(abs(ccs$acf))]
  wh  <- weighted.mean(ccs$lag, abs(ccs$acf))
  hs[i, "d_hp"] = h
  whs[i, "d_hp"] = wh
  
}

corresp_aux <- unique(select(county_corresp, STATE, ST_Name))

tab_ok_h  <- hs %>%
             inner_join(corresp_aux, by = c("ST" = "STATE")) %>%
             mutate(avg_h = (level + diff + acum_12 + acum_24 + d_hp) / 5)

tab_ok_wh <- whs %>% 
             inner_join(corresp_aux, by = c("ST" = "STATE")) %>%
             mutate(avg_h = (level + diff + acum_12 + acum_24 + d_hp) / 5)

arrange(tab_ok_h, avg_h)
arrange(tab_ok_wh, avg_h)


# YOU CAN USE THE WIEGHTED LAG WITH THE ABSOLUTE CORRELATION AS WEIGHTS. IN THE ORIGINAL PAPER, THE ABSOLUTE VALUE IS TAKEN IN CONSIDERAITON ALSO.


cor(select(tab_ok, level, diff, acum_12, acum_24, d_hp), method = "spearman")


