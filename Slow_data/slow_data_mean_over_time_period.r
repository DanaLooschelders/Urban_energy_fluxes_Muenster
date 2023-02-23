library(tidyverse)
library(lubridate)

source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R")
dat.meteo.merge$hour<-hour(dat.meteo.merge$TIMESTAMP)
dat.meteo.merge$hour_num<-hour(dat.meteo.merge$TIMESTAMP)
dat.meteo.merge$hour_num<-as.numeric(dat.meteo.merge$hour_num)
range(dat.meteo.merge$TIMESTAMP) #"2021-07-21 10:30:00 GMT" "2021-08-23 09:30:00 GMT"
####air temp####
#mean hourly 
ggplot(data=dat.meteo.merge)+
  geom_line(aes(x=hour, y=AirTC_Avg_beton, col="beton"), stat="summary", fun="mean")+
  geom_line(aes(x=hour, y=AirTC_Avg_kiebitz, col="kiebitz"), stat="summary", fun="mean")+
  theme_bw()
#mean over all - beton  -> 18.76968
mean(dat.meteo.merge$AirTC_Avg_beton, na.rm=T)
#mean overall - kiebitz -> 18.32164
mean(dat.meteo.merge$AirTC_Avg_kiebitz, na.rm=T)

mean()
#sd over all - beton  -> 3.452275
sd(dat.meteo.merge$AirTC_Avg_beton, na.rm=T)
#sd overall - kiebitz -> 3.270266
sd(dat.meteo.merge$AirTC_Avg_kiebitz, na.rm=T)

#range over all - beton  -> 11.1 28.8
range(dat.meteo.merge$AirTC_Avg_beton, na.rm=T)
#range overall - kiebitz -> 11.07 28.02
range(dat.meteo.merge$AirTC_Avg_kiebitz, na.rm=T)

#mean during the day - beton  -> 20.56021
mean(dat.meteo.merge$AirTC_Avg_beton[dat.meteo.merge$hour_num>6&dat.meteo.merge$hour_num<19], na.rm=T)

#sd during the day - beton -> 3.324524
sd(dat.meteo.merge$AirTC_Avg_beton[dat.meteo.merge$hour_num>6&dat.meteo.merge$hour_num<19], na.rm=T)

#mean during the day - kiebitz -> 20.10082
mean(dat.meteo.merge$AirTC_Avg_kiebitz[dat.meteo.merge$hour_num>6&dat.meteo.merge$hour_num<19], na.rm=T)

#sd during the day - kiebitz -> 3.155596
sd(dat.meteo.merge$AirTC_Avg_kiebitz[dat.meteo.merge$hour_num>6&dat.meteo.merge$hour_num<19], na.rm=T)

#mean during the night - beton -> 16.73155
mean(dat.meteo.merge$AirTC_Avg_beton[dat.meteo.merge$hour_num<6|dat.meteo.merge$hour_num>19], na.rm=T)

#sd during the night - beton -> 2.1934
sd(dat.meteo.merge$AirTC_Avg_beton[dat.meteo.merge$hour_num<6|dat.meteo.merge$hour_num>19], na.rm=T)

#mean during the night - kiebitz -> 16.39261
mean(dat.meteo.merge$AirTC_Avg_kiebitz[dat.meteo.merge$hour_num<6|dat.meteo.merge$hour_num>19], na.rm=T)

#sdn during the night - kiebitz ->2.035056
sd(dat.meteo.merge$AirTC_Avg_kiebitz[dat.meteo.merge$hour_num<6|dat.meteo.merge$hour_num>19], na.rm=T)

#### relative humidity####
#mean hourly 
ggplot(data=dat.meteo.merge)+
  geom_line(aes(x=hour, y=RH_Avg_beton, col="beton"), stat="summary", fun="mean")+
  geom_line(aes(x=hour, y=RH_Avg_kiebitz, col="kiebitz"), stat="summary", fun="mean")+
  theme_bw()

#mean overall  - beton  -> 71.76168
mean(dat.meteo.merge$RH_Avg_beton, na.rm=T)

#sd overall - beton -> 16.08806
sd(dat.meteo.merge$RH_Avg_beton, na.rm=T)

#mean overall  - kiebitz -> 74.81014
mean(dat.meteo.merge$RH_Avg_kiebitz, na.rm=T)

#sd overall - kiebitz -> 15.24666
sd(dat.meteo.merge$RH_Avg_kiebitz, na.rm=T)

####air pressure####

#mean  during the day
ggplot(data=dat.meteo.merge)+
  geom_line(aes(x=hour, y=AirP_Avg), stat="summary", fun="mean")+
  theme_bw()

#mean overall  -  1006.51
mean(dat.meteo.merge$AirP_Avg, na.rm=T)

#sd overall - 4.941579
sd(dat.meteo.merge$AirP_Avg, na.rm=T)

####Precipitation ####
#sum overall -> 58.8
sum(dat.rain.merge$Rain_mm_Tot, na.rm=T)
plot(dat.rain.merge$TIMESTAMP, dat.rain.merge$Rain_mm_Tot)

dat.rain.merge$day<-date(dat.rain.merge$TIMESTAMP)
rain_daily<-aggregate(Rain_mm_Tot~day, FUN=sum, data=dat.rain.merge)

ggplot(data=rain_daily)+
  geom_bar(aes())

####incoming solar rad####
#mean hourly 
ggplot(data=dat.meteo.merge)+
  geom_line(aes(x=hour, y=SUp_Avg_beton, col="beton"), stat="summary", fun="mean")+
  geom_line(aes(x=hour, y=SUp_Avg_kiebitz, col="kiebitz"), stat="summary", fun="mean")+
  theme_bw()

#mean overall  - beton  -> 190.6499
mean(dat.meteo.merge$SUp_Avg_beton, na.rm=T)

#sd overall - beton -> 247.0782
sd(dat.meteo.merge$SUp_Avg_beton, na.rm=T)

#mean overall  - kiebitz ->184.4747
mean(dat.meteo.merge$SUp_Avg_kiebitz, na.rm=T)

#sd overall - kiebitz -> 243.1173
sd(dat.meteo.merge$SUp_Avg_kiebitz, na.rm=T)

#compare with "normal values" for Münster" from reference time period 1991 bis 2020 (WMO): 
#Air Temp 18.4 -> vlg 18.3 kiebitz and 18.8 beton

#Precip: 78.5