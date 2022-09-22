
library(tidyr)
library(ggplot2)
#check meteorological conditions for grass subset
#subset 1: "2021-08-02 05:07:12 CEST" to "2021-08-03 05:06:48 CEST"
starttime_1 <- as.POSIXct("2021-08-02 05:07:12 CEST")
endtime_1 <- as.POSIXct("2021-08-03 05:06:48 CEST")

#subset 2: "2021-08-10 13:05:12 CEST"  to "2021-08-11 13:05:36 CEST"
starttime_2 <- as.POSIXct("2021-08-10 13:05:12 CEST")
endtime_2 <- as.POSIXct("2021-08-11 13:05:36 CEST")

#load slow data
#source script to load slow data and QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R") 

#subset to 1
meteo_1<-dat.kiebitz.meteo[dat.kiebitz.meteo$TIMESTAMP>=starttime_1&dat.kiebitz.meteo$TIMESTAMP<=endtime_1,]
rain_1<-dat.kiebitz.rain[dat.kiebitz.rain$TIMESTAMP>=starttime_1&dat.kiebitz.rain$TIMESTAMP<=endtime_1,]
soil_1<-dat.kiebitz.soil[dat.kiebitz.soil$TIMESTAMP>=starttime_1&dat.kiebitz.soil$TIMESTAMP<=endtime_1,]

#subset to 2
meteo_2<-dat.kiebitz.meteo[dat.kiebitz.meteo$TIMESTAMP>=starttime_2&dat.kiebitz.meteo$TIMESTAMP<=endtime_2,]
rain_2<-dat.kiebitz.rain[dat.kiebitz.rain$TIMESTAMP>=starttime_2&dat.kiebitz.rain$TIMESTAMP<=endtime_2,]
soil_2<-dat.kiebitz.soil[dat.kiebitz.soil$TIMESTAMP>=starttime_2&dat.kiebitz.soil$TIMESTAMP<=endtime_2,]

####meteo###
#convert timestamp
meteo_1$TIMESTAMP<-as.POSIXct(meteo_1$TIMESTAMP)
#meteo_1$Record<-seq(1, nrow(meteo_1), by=1)
#reshape
meteo_1_long <- meteo_1 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(meteo_1_long, aes(TIMESTAMP, value)) + 
  geom_line()+
facet_wrap(variable ~ ., ncol=3, scales="free")+
  theme_bw()

#get stats
meteo_stats<-data.frame("meteo_1"=colMeans(meteo_1[,3:length(meteo_1)], na.rm=T))
#meteo
#convert timestamp
meteo_2$TIMESTAMP<-as.POSIXct(meteo_1$TIMESTAMP)
#reshape
meteo_2_long <- meteo_2 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(meteo_2_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=3, scales="free")+
  theme_bw()

#get stats
meteo_stats$meteo_2<-colMeans(meteo_2[,3:length(meteo_2)], na.rm=T)
#transpose
meteo_stats<-as.data.frame(t(meteo_stats))
####rain###
#convert timestamp
rain_1$TIMESTAMP<-as.POSIXct(rain_1$TIMESTAMP)
#reshape
rain_1_long <- rain_1 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(rain_1_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=2, scales="free")+
  theme_bw()
#add col for rain
meteo_stats$rain<-NA
#stats
meteo_stats$rain[1]<-mean(rain_1$Rain_mm_Tot)
#soil
#convert timestamp
soil_1$TIMESTAMP<-as.POSIXct(soil_1$TIMESTAMP)
#remove unneccessary columns
soil_1<-soil_1[,c(1, 3, 9, 15)]
#reshape
soil_1_long <- soil_1 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(soil_1_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., nrow=3, scales="free")+
  theme_bw()

soil_stats_1<-data.frame("VWC_1"=mean(soil_1$WC01_VWC_Avg),
                         "VWC_2"=mean(soil_1$WC02_VWC_Avg),
                         "VWC_3"=mean(soil_1$WC03_VWC_Avg))

#rain
#convert timestamp
rain_1$TIMESTAMP<-as.POSIXct(rain_1$TIMESTAMP)
#reshape
rain_1_long <- rain_1 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(rain_1_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=2, scales="free")+
  theme_bw()
#stats
meteo_stats$rain[2]<-mean(rain_2$Rain_mm_Tot, na.rm=T)
#soil
#convert timestamp
soil_2$TIMESTAMP<-as.POSIXct(soil_2$TIMESTAMP)
#remove unneccessary columns
soil_2<-soil_2[,c(1, 3, 9, 15)]
#reshape
soil_2_long <- soil_2 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(soil_2_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., nrow=3, scales="free")+
  theme_bw()
#stats
soil_stats_2<-data.frame("VWC_1"=mean(soil_2$WC01_VWC_Avg),
                         "VWC_2"=mean(soil_2$WC02_VWC_Avg),
                         "VWC_3"=mean(soil_2$WC03_VWC_Avg))
soil_stats<-rbind(soil_stats_1, soil_stats_2)

meteo_stats<-cbind(meteo_stats, soil_stats)

#####calculate overall stats to compare####
####meteo###
#convert timestamp
dat.kiebitz.meteo$TIMESTAMP<-as.POSIXct(dat.kiebitz.meteo$TIMESTAMP)
#meteo_1$Record<-seq(1, nrow(meteo_1), by=1)
#reshape
dat_kiebitz_meteo_long <- dat.kiebitz.meteo %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(dat_kiebitz_meteo_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=3, scales="free")+
  theme_bw()
##add green background for sample days!

#get stats
meteo_whole_stats<-data.frame("meteo_1"=colMeans(dat.kiebitz.meteo[,3:length(dat.kiebitz.meteo)], na.rm=T))

#convert timestamp
dat.kiebitz.rain$TIMESTAMP<-as.POSIXct(dat.kiebitz.rain$TIMESTAMP)
#reshape
dat_kiebitz_rain_long <- dat.kiebitz.rain %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(dat_kiebitz_rain_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=2, scales="free")+
  theme_bw()
#add col for rain
meteo_whole_stats$rain<-NA
#stats
meteo_whole_stats$rain[1]<-mean(dat.kiebitz.rain$Rain_mm_Tot)
#soil
#convert timestamp
dat.kiebitz.soil$TIMESTAMP<-as.POSIXct(dat.kiebitz.soil$TIMESTAMP)
#remove unneccessary columns
dat.kiebitz.soil<-dat.kiebitz.soil[,c(1, 3, 9, 15)]
#reshape
dat_kiebitz_soil_long <- dat.kiebitz.soil %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(dat_kiebitz_soil_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., nrow=3, scales="free")+
  theme_bw()
#stats
soil_stats_whole<-data.frame("VWC_1"=mean(dat.kiebitz.soil$WC01_VWC_Avg),
                         "VWC_2"=mean(dat.kiebitz.soil$WC02_VWC_Avg),
                         "VWC_3"=mean(dat.kiebitz.soil$WC03_VWC_Avg))

meteo_whole_stats<-cbind(meteo_whole_stats, soil_stats_whole)
