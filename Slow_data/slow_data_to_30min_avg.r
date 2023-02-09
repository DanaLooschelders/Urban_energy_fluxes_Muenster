#Slow Meteodata Analysis EC02 Beton and EC04 Kiebitz
#global settings
options(digits.secs=3)
Sys.setenv(TZ='Etc/GMT') #ewige Winterzeit
#load libraries
library(tidyverse)
library(dplyr)
#source script to load slow data and perform QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R")

#check how many NAs there are per 30 min 
#dat.meteo.merge %>% summarise(across(everything(), ~ sum(is.na(.))))
dat.meteo.NA<-dat.meteo.merge %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="30 min")) %>%
  summarise_all(funs(sum(is.na(.))))
#test
#mean(c(1,NA,2,3,4), na.rm=F)
#summarize all variables to mean
dat.meteo.agg<-dat.meteo.merge %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="30 min")) %>%
  summarize_all(~mean(.,na.rm=F))

#summarize rain to sum
dat.rain.agg<-dat.rain.merge %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="30 min")) %>%
  summarize_all(~sum(.,na.rm=F))

#summarize soil to mean
dat.soil.agg<-dat.soil.merge %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="30 min")) %>%
  summarize_all(~mean(.,na.rm=F))
