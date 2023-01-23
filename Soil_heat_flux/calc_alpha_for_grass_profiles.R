library(gridExtra)
library(tidyverse)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
#source functions
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/functions_alpha.r")
#calculate for individual soil profile
FO_grass_depth<-read.csv("FO_grass_20cm_aG_10cm.csv")
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
       main=paste("all values - " ,colnames(FO_grass_plot[i])), ylim=c(17,24))
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

#calculate for 1 value
color_5th_value(point=1)
plot_5th_value(FO_data_x = FO_grass_1, range=707:716)
alpha_1<-alpha(FO_data_x=FO_grass_1, 707:716)
median(unlist(alpha_1[[1]]))
boxplot(alpha_1[[1]])

#for second
plot_5th_value(FO_data_x = FO_grass_2,  707:716)
alpha_2<-alpha(FO_data_x=FO_grass_2,  707:716)
median(unlist(alpha_2[[1]]))
boxplot(alpha_2[[1]])

#for third
alpha_3<-alpha(FO_data_x=FO_grass_3,  707:716)
median(unlist(alpha_3[[1]]))
boxplot(alpha_3[[1]])

#for fourth
alpha_4<-alpha(FO_data_x=FO_grass_4,  707:716)
median(unlist(alpha_4[[1]]))
boxplot(alpha_4[[1]])
plot_temp_alpha(FO_data_x = FO_grass_1, alpha_x = alpha_1)

#specific heat capacity
median(unlist(alpha_1[[1]]))
median(unlist(alpha_2[[1]]))
median(unlist(alpha_3[[1]]))
median(unlist(alpha_4[[1]]))

specific_heat_lower<-1000
specific_heat_higher<-1200  #mean=1140, sd=25) 
density<-2.409*1000

#calculate k
k_lower<-alpha*specific_heat_lower*density
k_upper<-alpha*specific_heat_higher*density
i=1
#test
flux_lower<-shf(FO_data_x = FO_concrete_4, k=k_lower)
plot_shf(flux_dat=flux_lower)
