#Slow Meteodata Analysis EC02 Beton and EC04 Kiebitz
#global settings
options(digits.secs=3)
Sys.setenv(TZ='Etc/GMT') #ewige Winterzeit
#load libraries
library(tidyverse)
#source script to load slow data
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/prep_slow_data_data.R")

#calculate mean temperature difference
dat.meteo.merge$temp.diff<-dat.meteo.merge$AirTC_Avg_beton-dat.meteo.merge$AirTC_Avg_kiebitz
temp.diff<-dat.meteo.merge$AirTC_Avg_beton-dat.meteo.merge$AirTC_Avg_kiebitz
mean(abs(temp.diff), na.rm=T) #absolut values

mean(temp.diff, na.rm=T) #arithmetic mean

str(dat.meteo.merge)
#plot Temperature difference
ggplot(aes(x = TIMESTAMP, y = temp.diff), data=dat.meteo.merge)+
  geom_line()+
  theme_bw()+
  geom_abline(slope = 0, intercept=0, color="red")+
  ggtitle(label="Temperature difference (EC02-EC04)")+
  labs(x="Time", y="Temperature difference [°C]")

#plot both temperatures
ggplot(aes(x = TIMESTAMP), data=dat.meteo.merge)+
  geom_line(aes(y=AirTC_Avg_beton, color="EC02 Beton"))+
  geom_line(aes(y=AirTC_Avg_kiebitz, color="EC04 Kiebitz"))+
  theme_bw()+  
  ggtitle(label="Temperature of EC02 and EC04")+
  labs(x="Time", y="Temperature [°C]")+
  scale_color_manual(name="EC Towers", 
                     breaks=c("EC02 Beton", "EC04 Kiebitz"),
                     values=c("EC02 Beton"="red", "EC04 Kiebitz"="blue"))

#plot agreement of temperatures as scatter plot
ggplot(aes(x = AirTC_Avg_kiebitz, y=AirTC_Avg_beton), data=dat.meteo.merge)+
  geom_point()+
  theme_bw()+  
  ggtitle(label="Temperature agreement of EC02 and EC04")+
  labs(x="Kiebitz Temperature [°C]", y="Beton Temperature [°C]")+
  geom_abline(intercept = 0, slope = 1, color="red")

#plot Temperature difference as a function of Temperature
ggplot(aes(x = AirTC_Avg_kiebitz, y=temp.diff), data=dat.meteo.merge)+
  geom_point()+
  theme_bw()+  
  ggtitle(label="Temperature difference for Temperature")+
  labs(x="Kiebitz Temperature [°C]", y="Temperature Difference [°C]")

#plot Temperature difference as a function of Temperature
ggplot(aes(x = AirTC_Avg_beton, y=temp.diff), data=dat.meteo.merge)+
  geom_point()+
  theme_bw()+  
  ggtitle(label="Temperature difference for Temperature")+
  labs(x="Kiebitz Temperature [°C]", y="Temperature Difference [°C]")

#per day
dat.meteo.merge$day<-format(dat.meteo.merge$TIMESTAMP, format="%d")
#mean hour
dat.meteo.merge$hour<-format(dat.meteo.merge$TIMESTAMP, format="%H")

#plot temperature difference as mean day per hour
ggplot(data=dat.meteo.merge, aes(y=temp.diff, x=hour))+
  geom_boxplot()+
  geom_hline(yintercept = 0, col="red")+
  theme_classic()+
  xlab(label="Hour of day")+
  ylab(label="Temperature difference [°C]")
  
