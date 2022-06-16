library(tidyverse)
library(dplyr)
library(gridExtra)
#source scripts
getwd()
#30min slow data
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/slow_data_to_30min_avg.r")
#heat flux data
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Flux_data/prep_flux_data.R")

dat.meteo.agg$Rain_mm_Tot<-dat.rain.agg$Rain_mm_Tot #add rain data
dat.meteo.agg<-cbind(dat.meteo.agg, dat.soil.agg[,-c(1,2)]) #add soil data
#merge beton data
beton$TIMESTAMP<-beton$datetime
#convert to POSIXct
dat.meteo.agg$TIMESTAMP<-as.POSIXct(dat.meteo.agg$TIMESTAMP)
beton$TIMESTAMP<-as.POSIXct(beton$TIMESTAMP)
#merge
dat.beton.flux.meteo<-merge(dat.meteo.agg, beton, 
                                             by="TIMESTAMP", #merge by timestamp
                                             all = T) #keep all rows and add NA for missing data
dat.beton.flux.meteo$TIMESTAMP<-as.POSIXct(dat.beton.flux.meteo$TIMESTAMP)

#merge kiebitz data
kiebitz$TIMESTAMP<-kiebitz$datetime
#convert to POSIXct
dat.meteo.agg$TIMESTAMP<-as.POSIXct(dat.meteo.agg$TIMESTAMP)
kiebitz$TIMESTAMP<-as.POSIXct(kiebitz$TIMESTAMP)
#merge
dat.kiebitz.flux.meteo<-merge(dat.meteo.agg, kiebitz, 
                            by="TIMESTAMP", #merge by timestamp
                            all = T) #keep all rows and add NA for missing data
dat.kiebitz.flux.meteo$TIMESTAMP<-as.POSIXct(dat.kiebitz.flux.meteo$TIMESTAMP)
