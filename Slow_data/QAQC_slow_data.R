#Quality Control for slow data for EC02 Beton and EC04 Kiebitz
#global settings
options(digits.secs=3)
Sys.setenv(TZ='Etc/GMT') #ewige Winterzeit
#load libraries
library(tidyverse)
library(dplyr)
library(tidyr)
#source script to load slow data
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/prep_slow_data_data.R")

#Meteo
####AirTC - check if it is between 0 and 40 degrees C####
#Beton
plot(dat.meteo.merge$AirTC_Avg_beton, type="l") #plot
any(dat.meteo.merge$AirTC_Avg_beton<0|dat.meteo.merge$AirTC_Avg_beton>40, na.rm=T) 
#Kiebitz
plot(dat.meteo.merge$AirTC_Avg_kiebitz, type="l") #plot
any(dat.meteo.merge$AirTC_Avg_kiebitz<0|dat.meteo.merge$AirTC_Avg_kiebitz>40, na.rm=T)
####RH - check if it is between 0 and 100####
#Beton
plot(dat.meteo.merge$RH_Avg_beton, type="l") #plot
any(dat.meteo.merge$RH_Avg_beton<0|dat.meteo.merge$RH_Avg_beton>100, na.rm=T) 
#Kiebitz
plot(dat.meteo.merge$RH_Avg_kiebitz, type="l") #plot
any(dat.meteo.merge$RH_Avg_kiebitz<0|dat.meteo.merge$RH_Avg_kiebitz>100, na.rm=T) 
#####SUp - check if it is between 0 and 1200 and larger than SDn####
#Beton
plot(dat.meteo.merge$SUp_Avg_beton, type="l")
any(dat.meteo.merge$SUp_Avg_beton<0|dat.meteo.merge$SUp_Avg_beton>800, na.rm=T)
#set any radiation under 0 to zero
dat.meteo.merge$SUp_Avg_beton[!is.na(dat.meteo.merge$SUp_Avg_beton)&
                                dat.meteo.merge$SUp_Avg_beton<0]<-0
#check if SDn is always larger than SUp
any(dat.meteo.merge$SUpCo_Avg_beton>dat.meteo.merge$SDnCo_Avg_beton, na.rm=T)
#Kiebitz
plot(dat.meteo.merge$SUp_Avg_kiebitz, type="l")
any(dat.meteo.merge$SUp_Avg_kiebitz<0|dat.meteo.merge$SUp_Avg_kiebitz>1200, na.rm=T)
#set any radiation under 0 to zero
dat.meteo.merge$SUp_Avg_kiebitz[!is.na(dat.meteo.merge$SUp_Avg_kiebitz)&
                                dat.meteo.merge$SUp_Avg_kiebitz<0]<-0
#check if SDn is always larger than SUp
any(dat.meteo.merge$SUpCo_Avg_kiebitz>dat.meteo.merge$SDnCo_Avg_kiebitz, na.rm=T)
####SDn - check if it is between 0 and 500####
#Beton
plot(dat.meteo.merge$SDn_Avg_beton, type="l") #plot
any(dat.meteo.merge$SDn_Avg_beton<0|dat.meteo.merge$SDn_Avg_beton>500, na.rm=T)
#set any radiation under 0 to zero
dat.meteo.merge$SDn_Avg_beton[!is.na(dat.meteo.merge$SDn_Avg_beton)&
                                dat.meteo.merge$SDn_Avg_beton<0]<-0
#Kiebitz
plot(dat.meteo.merge$SDn_Avg_kiebitz, type="l") #plot
any(dat.meteo.merge$SDn_Avg_kiebitz<0|dat.meteo.merge$SDn_Avg_kiebitz>500, na.rm=T)
#set any radiation under 0 to zero
dat.meteo.merge$SDn_Avg_kiebitz[!is.na(dat.meteo.merge$SDn_Avg_kiebitz)&
                                dat.meteo.merge$SDn_Avg_kiebitz<0]<-0

####LUp - check if it is between 0 and 500####
#beton
plot(dat.meteo.merge$LUpCo_Avg_beton, type="l")
any(dat.meteo.merge$LUpCo_Avg_beton<0|dat.meteo.merge$LUpCo_Avg_beton>500, na.rm=T)
#Kiebitz
plot(dat.meteo.merge$LUpCo_Avg_kiebitz, type="l") #plot
any(dat.meteo.merge$LUpCo_Avg_kiebitz<0|dat.meteo.merge$LUpCo_Avg_kiebitz>500, na.rm=T)

####LDn - check if it is between 0 and 600 and larger than Lup####
plot(dat.meteo.merge$LDnCo_Avg_beton, type="l")
any(dat.meteo.merge$LDnCo_Avg_beton<0|dat.meteo.merge$LDnCo_Avg_beton>600, na.rm=T)
#check if LUp is always larger than LDn
any(dat.meteo.merge$LDnCo_Avg_beton>dat.meteo.merge$LDnCo_Avg_beton, na.rm=T)
which(dat.meteo.merge$LUpCo_Avg_beton>dat.meteo.merge$LDnCo_Avg_beton)
dat.meteo.merge[which(dat.meteo.merge$LUpCo_Avg_beton>dat.meteo.merge$LDnCo_Avg_beton),]

#Kiebitz
plot(dat.meteo.merge$LDnCo_Avg_kiebitz, type="l") #plot
any(dat.meteo.merge$LDnCo_Avg_kiebitz<0|dat.meteo.merge$LDnCo_Avg_kiebitz>600, na.rm=T)
#check if LUp is always larger than LDn
any(dat.meteo.merge$LUpCo_Avg_kiebitz>dat.meteo.merge$LDnCo_Avg_kiebitz, na.rm=T)
####Albedo####
plot(dat.meteo.merge$Albedo_Avg_beton, type="l")
#check values
#### AirP - check if it between 990 and 1020 and that is does not change more than 5 hP####
plot(dat.meteo.merge$AirP_Avg, type="l") #plot
any(dat.meteo.merge$AirP_Avg<985|dat.meteo.merge$LDnCo_Avg_kiebitz>1020, na.rm=T)
#new column for difference between values
dat.meteo.merge$diff_AirP<-NA
dat.meteo.merge$diff_AirP[1:length(dat.meteo.merge$AirP_Avg)-1]<-abs(diff(dat.meteo.merge$AirP_Avg))
#check spikes
dat.meteo.merge$AirP_Avg[!is.na(dat.meteo.merge$AirP_Avg)&dat.meteo.merge$diff_AirP>5]<-NA
#remove diff column
dat.meteo.merge$diff_AirP<-NULL

####SHF check if it is between X and X ####
plot(dat.meteo.merge$shf_Avg.1., type="l")
lines(dat.meteo.merge$shf_Avg.2., col="red")
lines(dat.meteo.merge$shf_Avg.3., col="blue")

#Foken (2016): Bodenwärmestrom ca. 50–100 Wm−2
#plot in three panels
dat.meteo.merge[,c(1,18:20)] %>% 
  as.data.frame()%>%
  gather(key, value, -TIMESTAMP) %>% 
  ggplot(aes(x=TIMESTAMP, y=value)) +
  geom_line()+
  theme_bw()+
  xlab(label="Time")+
  ylab(label="Soil Heat Flux [W m^2]")+
  facet_wrap(~ key, nrow = 3) 

dat.meteo.merge[,c(1,18:20)] %>% 
  as.data.frame()%>%
  gather(key, value, -TIMESTAMP) %>% 
  ggplot(aes(x=TIMESTAMP, y=value, color=key)) +
  geom_line()+
  xlab(label="Time")+
  theme_bw()+
  ylab(label="Soil Heat Flux [W m^2]")

plot(dat.rain.merge$TIMESTAMP, dat.rain.merge$Rain_mm_Tot)
plot(dat.meteo.merge$TIMESTAMP, dat.meteo.merge$SDn_Avg_beton, type="l")

#####Rain - check for negative values ####
plot(dat.rain.merge$Rain_mm_Tot, type="l")
any(dat.rain.merge$Rain_mm_Tot<0, na.rm=T)

####Soil temperature - check agreement ####
plot(dat.soil.merge$WC01_T_Avg, type="l",
     main="Soil Temperature",
     ylab="T [°C]")
lines(dat.soil.merge$WC02_T_Avg, col="red")
lines(dat.soil.merge$WC03_T_Avg, col="blue")

#### VWC - check agreement and spikes####
plot(dat.soil.merge$WC01_VWC_Avg[500:1200], type="l",
     main = "Volumetric Water Content",
     ylab="VWC [m3/m3]")
lines(dat.soil.merge$WC02_VWC_Avg[500:1200], col="red")
lines(dat.soil.merge$WC03_VWC_Avg[500:1200], col="blue")
points(dat.rain.merge$Rain_mm_Tot[500:1200]/20)#plot rain with it
#-> probably a real phenomen

#calculate difference
#sensor 1
dat.soil.merge$diff_VWC1<-NA #create new diff column
dat.soil.merge$diff_VWC1[1:length(dat.soil.merge$WC01_VWC_Avg)-1]<-abs(diff(dat.soil.merge$WC01_VWC_Avg))

#create second diff column to check for spikes (two large differences after one another)
dat.soil.merge$diff_VWC1_2<-NA
dat.soil.merge$diff_VWC1_2[2:length(dat.soil.merge$WC01_VWC_Avg)-1]<-abs(diff(dat.soil.merge$WC01_VWC_Avg))

plot(dat.soil.merge$diff_VWC1, type="l") #plot difference
dat.soil.merge$WC01_VWC_Avg[!is.na(dat.soil.merge$WC01_VWC_Avg)&
                              dat.soil.merge$diff_VWC1>0.3&
                              dat.soil.merge$diff_VWC1_2>0.3]<-NA
#sensor 2
dat.soil.merge$diff_VWC2<-NA #create new diff column
dat.soil.merge$diff_VWC2[1:length(dat.soil.merge$WC02_VWC_Avg)-1]<-abs(diff(dat.soil.merge$WC02_VWC_Avg))
#create second diff column to check for spikes (two large differences after one another)
dat.soil.merge$diff_VWC2_2<-NA #create new diff column
dat.soil.merge$diff_VWC2_2[2:length(dat.soil.merge$WC02_VWC_Avg)-1]<-abs(diff(dat.soil.merge$WC02_VWC_Avg))

plot(dat.soil.merge$diff_VWC1, type="l") #plot difference
dat.soil.merge$WC02_VWC_Avg[!is.na(dat.soil.merge$WC02_VWC_Avg)&
                              dat.soil.merge$diff_VWC2>0.3&dat.soil.merge$
                              diff_VWC2_2>0.3]<-NA
#sensor 3
dat.soil.merge$diff_VWC3<-NA #create new diff column
dat.soil.merge$diff_VWC3[1:length(dat.soil.merge$WC03_VWC_Avg)-1]<-abs(diff(dat.soil.merge$WC03_VWC_Avg))
#create second diff column to check for spikes (two large differences after one another)
dat.soil.merge$diff_VWC3_2<-NA #create new diff column
dat.soil.merge$diff_VWC3_2[2:length(dat.soil.merge$WC03_VWC_Avg)-1]<-abs(diff(dat.soil.merge$WC03_VWC_Avg))

plot(dat.soil.merge$diff_VWC1, type="l") #plot difference
dat.soil.merge$WC03_VWC_Avg[!is.na(dat.soil.merge$WC03_VWC_Avg)&
                              dat.soil.merge$diff_VWC3>0.3&
                              dat.soil.merge$diff_VWC3_2>0.3]<-NA
#remove diff columns
dat.soil.merge[,c("diff_VWC1","diff_VWC2", "diff_VWC3")]<-NULL
#plot again
plot(dat.soil.merge$WC01_VWC_Avg, type="l", 
     main = "Volumetric Water Content",
     ylab="VWC [m3/m3]")
lines(dat.soil.merge$WC02_VWC_Avg, col="red")
lines(dat.soil.merge$WC03_VWC_Avg, col="blue")
