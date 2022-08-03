#load scripts
#source script to load flux data from EC02 and EC04 and slow data
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Meteorology/heat_fluxes_with_meteorology.r")
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Ratios")
library(tidyverse)
#calculate Bowen ratio
  #Bo=sensible/latent   
dat.beton.flux.meteo$BR_beton<-dat.beton.flux.meteo$H/dat.beton.flux.meteo$LE
dat.kiebitz.flux.meteo$BR_kiebitz<-dat.kiebitz.flux.meteo$H/dat.kiebitz.flux.meteo$LE

#test normal distribution beton
qqnorm(dat.beton.flux.meteo$BR_beton)
qqline(dat.beton.flux.meteo$BR_beton)
shapiro.test(dat.beton.flux.meteo$BR_beton) 
    #-->not normal

#test normal distribution kiebitz
qqnorm(dat.kiebitz.flux.meteo$BR_kiebitz)
qqline(dat.kiebitz.flux.meteo$BR_kiebitz)
shapiro.test(dat.kiebitz.flux.meteo$BR_kiebitz)
      
    #####timeseries
#timeseries Beton bowen ratio
ggplot(data=dat.beton.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=BR_beton))+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  ylab(label="Bowen ratio")+
  xlab(label="time")+
  ggtitle(label="Bowen ratio EC02 Beton", subtitle = "Bo = H/LE")

ggsave(filename="BowenRatio_ts_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggsave(filename="BowenRatio_ts_beton.png",
       device="png",width=297, height=210, units = "mm")

#timeseries Kiebitz bowen ratio
ggplot(data=dat.kiebitz.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=BR_kiebitz))+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  ylab(label="Bowen ratio")+
  xlab(label="time")+
  ggtitle(label="Bowen ratio EC04 Kiebitz", subtitle = "Bo = H/LE")

ggsave(filename="BowenRatio_ts_kiebitz.pdf",
       device="pdf",width=297, height=210, units = "mm")
ggsave(filename="BowenRatio_ts_kiebitz.png",
       device="png",width=297, height=210, units = "mm")

  #mean day bowen ratio beton
ggplot(dat=subset(dat.beton.flux.meteo, !is.na(hour)), 
              aes(x=as.factor(hour), y=BR_beton))+
  stat_summary_bin(stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=dat.beton.flux.meteo, fun.data = "median_mad", 
               geom = "errorbar", alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Bowen Ratio EC02", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  xlab("Hour of Day")+
  ylab("Bowen Ratio")

ggsave(filename="BowenRatio_diurnal_median_mad_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggsave(filename="BowenRatio_diurnal_median_mad_beton.png",
       device="png",width=297, height=210, units = "mm")

#Aggregated Bowen Ratio EC04
ggplot(dat=subset(dat.kiebitz.flux.meteo, !is.na(hour)), 
       aes(x=as.factor(hour), y=BR_kiebitz))+
  stat_summary_bin(stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=dat.kiebitz.flux.meteo, fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Bowen Ratio EC04", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  xlab("Hour of Day")+
  ylab("Bowen Ratio")

ggsave(filename="BowenRatio_diurnal_mean_sd_kiebitz.pdf",
       device="pdf",width=297, height=210, units = "mm")
  
ggsave(filename="BowenRatio_diurnal_mean_sd_kiebitz.png",
       device="png",width=297, height=210, units = "mm")
    
#overall mean
mean(dat.beton.flux.meteo$BR_beton, na.rm=T) #3.31
mean(dat.kiebitz.flux.meteo$BR_kiebitz, na.rm=T) #0.86
    #Bowen ratio for both
      #time series bowen ratio both
ggplot(data=dat.kiebitz.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=BR_kiebitz, col="EC02 Beton"))+
  geom_line(data=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=BR_beton, col="EC04 Kiebitz"))+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  ylab(label="Bowen ratio")+
  xlab(label="time")+
  ggtitle(label="Bowen ratio EC04 and EC02", subtitle = "Bo = H/LE")

ggsave(filename="BowenRatio_ts_beton_kiebitz.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggsave(filename="BowenRatio_ts_beton_kiebitz.png",
       device="png",width=297, height=210, units = "mm")

#aggregated mean day bowen ratio kiebitz beton
ggplot(dat=subset(dat.kiebitz.flux.meteo, !is.na(hour)), 
       aes(x=as.factor(hour), y=BR_kiebitz))+
  stat_summary_bin(aes(col="EC02 Kiebitz"), stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=dat.kiebitz.flux.meteo, aes(col="EC02 Kiebitz"), fun.data = "median_mad", 
               geom = "errorbar",  alpha=1, width=0.4,
               show.legend = F, na.rm=T)+
  stat_summary_bin(dat=subset(dat.beton.flux.meteo, !is.na(hour)), 
                   aes(x=as.factor(hour), y=BR_beton, col="EC02 Beton"), stroke=2.5,
                   fun = "median",geom="point", show.legend = F, 
                   na.rm = T)+
  stat_summary(dat=subset(dat.beton.flux.meteo, !is.na(hour)), 
               aes(x=as.factor(hour), y=BR_beton, col="EC02 Beton"),  fun.data = "median_mad", 
               geom = "errorbar",  alpha=1, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Bowen Ratio EC04 and EC02", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="black")+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  xlab("Hour of Day")+
  ylab("Bowen Ratio")

ggsave(filename="BowenRatio_diurnal_mean_sd_kiebitz_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggsave(filename="BowenRatio_diurnal_mean_sd_kiebitz_beton.png",
       device="png",width=297, height=210, units = "mm")

#calculate ratio between EC02 and EC04
  #sensible EC02/EC04
dat.beton.flux.meteo$H_ratio_EC02_to_EC04<-dat.beton.flux.meteo$H/dat.kiebitz.flux.meteo$H
#test normal distribution
qqnorm(dat.beton.flux.meteo$H_ratio_EC02_to_EC04)
qqline(dat.beton.flux.meteo$H_ratio_EC02_to_EC04)
shapiro.test(dat.beton.flux.meteo$H_ratio_EC02_to_EC04)
  #not normal

  #timeseries ratio sensible EC02/EC04
ggplot(data=dat.beton.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=H_ratio_EC02_to_EC04))+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  ylab(bquote('Ratio of H Flux [W' ~m^-2* ']'))+
  xlab(label="time")+
  ggtitle(label="Ratio of sensible Heat", 
          subtitle = "EC02/ EC04" )

ggsave(filename="H_Ratio_ts_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggsave(filename="H_Ratio_ts_beton.png",
       device="png",width=297, height=210, units = "mm")

#QAQC
which.max(dat.beton.flux.meteo$H_ratio_EC02_to_EC04)
dat.beton.flux.meteo$TIMESTAMP[1309]
dat.beton.flux.meteo$H[1309]
dat.kiebitz.flux.meteo$H[1309]
which.min(dat.beton.flux.meteo$H_ratio_EC02_to_EC04)
dat.beton.flux.meteo$TIMESTAMP[857]
dat.beton.flux.meteo$H[857]
dat.kiebitz.flux.meteo$H[857]

#mean day ratio sensible EC02/EC04
ggplot(dat=subset(dat.beton.flux.meteo, !is.na(hour)), 
       aes(x=as.factor(hour), y=H_ratio_EC02_to_EC04))+
  stat_summary_bin(stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=dat.beton.flux.meteo, fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Ratio of sensible Heat", 
          subtitle = "EC02/ EC04 \nMedian with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  xlab("Hour of Day")+
  ylab(bquote('Ratio of H Flux [W' ~m^-2* ']'))

ggsave(filename="H_Ratio_diurnal_mean_sd_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggsave(filename="H_Ratio_diurnal_mean_sd_beton.png",
       device="png",width=297, height=210, units = "mm")
  #mean
mean(dat.beton.flux.meteo$H, na.rm=T)/mean(dat.kiebitz.flux.meteo$H, na.rm=T) #1.93
 
  #latent EC02/EC04
dat.beton.flux.meteo$LE_ratio_EC02_to_EC04<-dat.beton.flux.meteo$LE/dat.kiebitz.flux.meteo$LE
#test normal distribution
qqnorm(dat.beton.flux.meteo$LE_ratio_EC02_to_EC04)
qqline(dat.beton.flux.meteo$LE_ratio_EC02_to_EC04)
shapiro.test(dat.beton.flux.meteo$LE_ratio_EC02_to_EC04)
#not normal

#timeseries ratio sensible EC02/EC04
ggplot(data=dat.beton.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=LE_ratio_EC02_to_EC04))+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  ylab(bquote('Ratio of LE Flux [W' ~m^-2* ']'))+
  xlab(label="time")+
  ggtitle(label="Ratio of latent Heat", 
          subtitle = "EC02/ EC04" )

ggsave(filename="LE_Ratio_ts_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggsave(filename="LE_Ratio_ts_beton.png",
       device="png",width=297, height=210, units = "mm")

#mean day ratio sensible EC02/EC04
ggplot(dat=subset(dat.beton.flux.meteo, !is.na(hour)), 
       aes(x=as.factor(hour), y=LE_ratio_EC02_to_EC04))+
  stat_summary_bin(stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=dat.beton.flux.meteo, fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Ratio of latent Heat", 
          subtitle = "EC02/ EC04 \nMedian with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  xlab("Hour of Day")+
  ylab(bquote('Ratio of LE Flux [W' ~m^-2* ']'))

ggsave(filename="LE_Ratio_diurnal_mean_sd_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggsave(filename="LE_Ratio_diurnal_mean_sd_beton.png",
       device="png",width=297, height=210, units = "mm")
#mean
mean(dat.beton.flux.meteo$LE, na.rm=T)/mean(dat.kiebitz.flux.meteo$LE, na.rm=T) #0.52
