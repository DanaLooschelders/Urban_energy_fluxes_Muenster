#Slow Meteodata Analysis EC02 Beton and EC04 Kiebitz
#global settings
options(digits.secs=3)
Sys.setenv(TZ='Etc/GMT') #ewige Winterzeit
#load libraries
library(tidyverse)
library(dplyr)
#source script to load slow data and perform QAQC
source("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/QAQC_slow_data.R")

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
