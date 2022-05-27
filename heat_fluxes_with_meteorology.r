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


####Rain####
#BETON
#plot latent heat flux with rain
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_bar(aes(y=Rain_mm_Tot*10), stat="identity", color="blue")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  theme_bw()

#plot Sensible Heat Flux with rain
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_bar(aes(y=Rain_mm_Tot*10), stat="identity", color="blue")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()

####Shortwave radiation#####
#BETON
#plot latent heat flux with shortwave radiation
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_bar(aes(y=Rain_mm_Tot*10), stat="identity", color="blue")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  theme_bw()

#plot Sensible Heat Flux with reflected shortwave radiation
sh.beton<-ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()

swdn.beton<-ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=SDn_Avg_beton))+
  geom_line(color="orange")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
grid.arrange(sh.beton,swdn.beton, nrow=2)

#plot Sensible Heat Flux with incoming shortwave radiation
swup.beton<-ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg_beton))+
  geom_line(color="orange")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
grid.arrange(sh.beton,swup.beton, nrow=2)
