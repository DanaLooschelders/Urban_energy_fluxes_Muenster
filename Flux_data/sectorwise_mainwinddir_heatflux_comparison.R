#source script to load flux and slow data and compare heatfluxes
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/sectorwise_frequencywise_heatflux_comparison.R")
library(circular)
#calculate the mean wind direction
mean(circular(dat.beton.flux.meteo$wind_dir, units = "degrees"), na.rm=T)
atan2(mean(dat.beton.flux.meteo$u_rot, na.rm=T),
      mean(dat.beton.flux.meteo$v_rot, na.rm=T))*(180/pi)+180
