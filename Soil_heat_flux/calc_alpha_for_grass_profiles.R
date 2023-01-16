library(gridExtra)
library(tidyverse)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
#source functions
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/functions_alpha.r")
#calculate for individual soil profile
FO_grass_depth<-read.csv("FO_grass.csv")
#transpose
FO_grass_time<-data.frame(t(FO_grass_depth))
#set time as column name
colnames(FO_grass_time)<-FO_grass_depth$grass_time
#delete row with time
FO_grass_time<-FO_grass_time[-dim(FO_grass_time)[1],]
#add depth to plot
FO_grass_plot<-FO_grass_time
FO_grass_plot$depth<-as.numeric(substr(rownames(FO_grass_plot),start = 2, stop=100))
#read in meteo data for sample day
meteo_all<-read.csv("meteo_sample_day.csv")
meteo_sub<-meteo_all[meteo_all$TIMESTAMP>="2021-08-04 07:00:00"&
                       meteo_all$TIMESTAMP<="2021-08-04 09:30:00",]
#reshape
meteo_all_long <- meteo_all %>%                             
  gather(variable, value, -c(TIMESTAMP))
#reshape
meteo_sub_long <- meteo_sub %>%                             
  gather(variable, value, -c(TIMESTAMP))

colnames(meteo_all)
str(meteo_all_long)
meteo_all_long$TIMESTAMP<-as.POSIXct(meteo_all_long$TIMESTAMP)
meteo_sub_long$TIMESTAMP<-as.POSIXct(meteo_sub_long$TIMESTAMP)

#plot ind var
plot_meteo(meteo_sub_long,"AirTC_Avg_beton")
plot_meteo(meteo_sub_long,"SUp_Avg_kiebitz")
plot_meteo(meteo_sub_long, "Rain_mm_Tot")

#soil profile to same same subset as meteo_data
FO_grass_sub<-FO_grass_time[,c(which(colnames(FO_grass_time)=="2021-08-04 07:00:00"):which(colnames(FO_grass_time)=="2021-08-04 09:30:00"))]
#indices: 701:716
#plot all values
for(i in 701:716 ){
  plot(FO_grass_plot$depth, FO_grass_plot[,i],  type="l", 
       main=paste("all values - " ,colnames(FO_grass_plot[i])), ylim=c(16,25))
  points(FO_grass_plot$depth, FO_grass_plot[,i])
  Sys.sleep(3)
}

#take every fifth point (variying starting point i)
for(i in 1:4){
  FO_grass_temp <- FO_grass_time[seq(i, nrow(FO_grass_time), 4), ] #select every 5th row
  FO_grass_temp[] <- lapply(FO_grass_temp, as.numeric) #coerce to numeric
  FO_grass_temp$depth<-as.numeric(substr(rownames(FO_grass_temp),start = 2, stop=100))
  assign(paste0("FO_grass_", i), FO_grass_temp) #assign name to object
  rm(FO_grass_temp)#remove object
}

alpha_1<-alpha(FO_data_x=FO_grass_1)
plot_temp_alpha(FO_data_x = FO_grass_1, alpha_x = alpha_1)
#"30.07.2021  08:08:00" to "30.07.2021 10:08:00"

#plot profiles
#first value
#plot profiles with every xth point

plot_5th_value(FO_grass_1, range=701:716)
alpha_4<-alpha(FO_grass_4, range=701:716)
boxplot(alpha_1[[1]])

median(unlist(alpha_1[[1]]))
