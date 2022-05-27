#Slow Meteodata Analysis EC02 Beton and EC04 Kiebitz
#global settings
options(digits.secs=3)
Sys.setenv(TZ='Etc/GMT') #ewige Winterzeit
#load libraries
library(tidyverse)
library(dplyr)
#source script to load slow data and perform QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R")

#summarize all variables to mean
dat.meteo.agg<-dat.meteo.merge %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="30 min")) %>%
  summarize_all(~mean(.))

#summarize rain to sum
dat.rain.agg<-dat.rain.merge %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="30 min")) %>%
  summarize_all(~sum(.))

#summarize soil to mean
dat.soil.agg<-dat.soil.merge %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="30 min")) %>%
  summarize_all(~mean(.))
