#load libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(Hmisc)

#source script to load flux data from EC02 and EC04 and slow data
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/heat_fluxes_with_meteorology.r")

#flux differences
#####sensible heat - time series####

#plot EC02 and EC04 together
ggplot()+
  geom_line(dat=beton,aes(x=datetime, y=H, color="EC02 Beton"))+
  geom_line(dat=kiebitz,aes(x=datetime,y=H, color="EC04 Kiebitz"))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Sensible Heat flux EC02 and EC04")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_both_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#plot difference between EC02 and EC04
dif<-data.frame(datetime=beton$datetime, H_beton=beton$H)
dif$H_kiebitz<-kiebitz$H
dif$flux_dif_H<-beton$H-kiebitz$H

##Sensible Heat Flux difference
ggplot(dat=dif, aes(x=datetime, y=flux_dif_H))+
  geom_line()+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label="Sensible Heat Flux Difference EC02-EC04")+
  ylab(bquote('H flux difference [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_dif_both.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

##integrate area under curve (for negative values)
library(flux)
#create dataset whithout NA values
AUC_data<-data.frame("TIMESTAMP"=dat.beton.flux.meteo$TIMESTAMP,
                     "H_Beton"=dat.beton.flux.meteo$H,
                     "H_Kiebitz"=dat.kiebitz.flux.meteo$H)
#remove all rows that contain NA
AUC_data<-AUC_data[complete.cases(AUC_data),]
#Beton
auc.withoutneg<-auc(x=AUC_data$TIMESTAMP, 
                    y=AUC_data$H_Beton, thresh=NULL) #negative are subtracted
auc.withneg<-auc(x=AUC_data$TIMESTAMP, 
                 y=AUC_data$H_Beton, thresh=0) #negative are set to 0
#thus negative area is:
neg_auc_H_beton<-auc.withneg-auc.withoutneg
#Kiebitz
auc.withoutneg<-auc(x=AUC_data$TIMESTAMP, 
                    y=AUC_data$H_Kiebitz, thresh=NULL) #negative are subtracted
auc.withneg<-auc(x=AUC_data$TIMESTAMP, 
                 y=AUC_data$H_Kiebitz, thresh=0) #negative are set to 0
#thus negative area is:
neg_auc_H_kiebitz<-auc.withneg-auc.withoutneg

#calculate Ratio of Kiebitz to Beton
neg_auc_H_kiebitz/neg_auc_H_beton
#negative heat flux of Kiebitz is 23 times larger than heat flux of Beton
##to quatify how much heat is going in which tower

#####sensible heat - diurnal####
#as aggregated mean line for each hour with errorbars
ggplot(beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  stat_summary(dat=kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\nMean with errorbars displaying 1 SD" )+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_mean_both_1sd_errorbars_hour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for each half hour
ggplot(beton, aes(x=as.factor(time), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  stat_summary(dat=kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\nMean with errorbars displaying 1 SD")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_mean_both_1sd_errorbars_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for hour
ggplot(beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=5)+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point", size=5)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated median line for hour
ggplot(beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point", size=5)+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="median", geom="point", size=5)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = "Median of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_median_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for halfhour
ggplot(beton, aes(x=as.factor(time), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux EC02 and EC04")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_mean_both_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")


#####latent heat - time series####
#plot EC02 and EC04 together
#remove outlier 
beton$LE[beton$LE<=-100]<-NA

ggplot()+
  geom_line(dat=beton,aes(x=datetime, y=LE, color="EC02 Beton"))+
  geom_line(dat=kiebitz,aes(x=datetime,y=LE, color="EC04 Kiebitz"))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Latent Heat flux EC02 and EC04")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "LE_Flux_both.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#plot difference between EC02 and EC04
dif$LE_beton<-beton$LE
dif$LE_kiebitz<-kiebitz$LE
dif$flux_dif_LE<-beton$LE-kiebitz$LE

##Sensible Heat Flux difference
ggplot(dat=dif, aes(x=datetime, y=flux_dif_LE))+
  geom_line()+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label="Latent Heat Flux Difference EC02-EC04")+
  ylab(bquote('LE flux difference [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "LE_Flux_dif_both.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#####latent heat - diurnal####
#as aggregated mean line for each hour with errorbars
ggplot(beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(dat=beton, aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=LE, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar",fun.args = list(mult = 1))+
  stat_summary(dat=kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\nMean with errorbars displaying one SD")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = "LE_Flux_diurnal_both_mean_1sd_errorbars_hour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for each half hour
ggplot(beton, aes(x=as.factor(time), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=LE, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  stat_summary(dat=kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\nMean with errorbars displaying 1 SD")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = "LE_Flux_diurnal_mean_both_1sd_errorbars_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for hour
ggplot(beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=5)+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point", size=5)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = "LE_Flux_diurnal_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated median line for hour
ggplot(beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point", size=5)+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="median", geom="point", size=5)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = "Median of EC02 (Beton) and EC04 (Kiebitz)")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = "LE_Flux_diurnal_median_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for halfhour
ggplot(beton, aes(x=as.factor(time), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux EC02 and EC04")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "LE_Flux_diurnal_mean_both_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

