library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(countrycode)
library(scales)
library(ggthemes)
library(hrbrthemes)
library(corrplot)
library(maps)


SS <- read_csv("000001.SS.csv")
FTSE <- read_csv("ISF.L.csv")
SPY <- read_csv("SPY.csv")

global_index = data.frame()

SS <- SS %>% 
  mutate(index = "SS") %>%
  mutate(Growth_rate = (Close - lag(Close))/lag(Close))

FTSE <- FTSE %>% 
  mutate(index = "FTSE") %>%
  mutate(Growth_rate = (Close - lag(Close))/lag(Close))

SPY <- SPY %>% 
  mutate(index = "SPY") %>%
  mutate(Growth_rate = (Close - lag(Close))/lag(Close))

global_index <- global_index %>%
  rbind(SS) %>%
  rbind(FTSE) %>%
  rbind(SPY) 

global_index[is.na(global_index)] <- 0

global_index$Year = format(global_index$Date, format="%Y")
global_index$Month = months(global_index$Date)

write_csv(global_index, "global_index.csv") 
