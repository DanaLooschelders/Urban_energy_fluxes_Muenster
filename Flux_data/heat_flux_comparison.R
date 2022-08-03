#load libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(Hmisc)
library(ggpubr)
#source script to load flux data from EC02 and EC04 and slow data
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Flux_data/heat_fluxes_with_meteorology.r")

#flux differences
#####sensible heat - time series####
beton[beton$datetime=="2021-08-13 00:00",]

#plot EC02 and EC04 together
ggplot()+
  geom_line(dat=beton,aes(x=datetime, y=H, color="EC02 Beton"))+
  geom_line(dat=kiebitz,aes(x=datetime,y=H, color="EC04 Kiebitz"))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Sensible Heat flux EC02 and EC04")+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_both_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

range(dat.beton.flux.meteo$datetime[1056:1122])

#782:882,
ggplot()+
  geom_line(dat=dat.beton.flux.meteo[1056:1122,],
            aes(x=datetime, y=H, color="EC02", linetype="Sensible heat flux"))+
  geom_line(dat=dat.kiebitz.flux.meteo[1056:1122,],
            aes(x=datetime,y=H, color="EC04",linetype="Sensible heat flux"))+
  #geom_line(dat=dat.beton.flux.meteo[1058:1122,], aes(x=datetime, y=SUp_Avg_beton/10))+
  geom_line(dat=dat.beton.flux.meteo[1056:1122,], 
            aes(x=datetime, y= TotRNet_Avg_beton/1.7, 
                color="EC02", linetype="Net Radiation"))+
  geom_line(dat=dat.kiebitz.flux.meteo[1056:1122,], 
            aes(x=datetime, y= TotRNet_Avg_kiebitz/1.7, 
                color="EC04", linetype="Net Radiation"))+
  #geom_line(dat=dat.beton.flux.meteo[1058:1122,], aes(x=datetime, y= AirTC_Avg_beton*10))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Sensible Heat flux EC02 and EC04\n 12.08 12:00 to 14.08 21:00")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x * 1.7,
                                         name = bquote('Net Radiation [W' ~m^-2* ']')))+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77","#1f78b4", "#1b9e77"))+
  scale_linetype_manual("Linetype", values=c("dashed","solid"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()+
  theme(text = element_text(size=30), legend.position="bottom")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
        linetype= guide_legend(nrow = 2, byrow = TRUE))

#save plot
ggsave(filename = "H_Flux_both_timeseries_exampleday.png",
       device="png",width=297, height=210, units = "mm",
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
#as aggregated  median_mad line for each hour with errorbars
ggplot(beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="EC02 (concrete)"),stroke=2.5,
                   fun = "median",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="EC04 (grass)"), 
                   fun="median", geom="point", stroke=2.5)+
  stat_summary(dat=beton, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.4, width=0.4)+
  stat_summary(dat=kiebitz, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.4, width=0.4)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = " Median with errorbars displaying median absolute deviation" )+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  theme_bw()+
  theme(text = element_text(size=30), legend.position="bottom")

#save plot
ggsave(filename = "H_Flux_diurnal_median_both_mad_errorbars_hour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

ggsave(filename = "H_Flux_diurnal_median_both_mad_errorbars_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated  median_mad line for each hour with errorbars
ggplot(beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="EC02 (concrete)"),stroke=2.5,
                   fun = "median",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="EC04 (grass)"), 
                   fun="median", geom="point", stroke=2.5)+
  stat_summary(dat=beton, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar" , alpha=0.4, width=0.4)+
  stat_summary(dat=kiebitz, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar" ,alpha=0.4, width=0.4)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  theme_bw()+
  theme(text = element_text(size=30), legend.position="bottom")

#save plot
ggsave(filename = "H_Flux_diurnal_median_both_mad_errorbars_hour.png",
       device="png",width=350, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken") #for presentation

#as aggregated  median_mad line for each half hour
ggplot(beton, aes(x=as.factor(time), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="median", geom="point")+
  stat_summary(dat=beton, aes(col="beton"), fun.data = "median_mad", 
               geom = "errorbar" )+
  stat_summary(dat=kiebitz, aes(col="kiebitz"),fun.data = "median_mad", 
               geom = "errorbar" )+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\n Median with errorbars displaying median absolute deviation")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_median_both_mad_errorbars_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated  median_mad line for hour
ggplot(beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point", size=5)+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="median", geom="point", size=5)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = " median_mad of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_median_both_hour.png",
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
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_median_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated  median_mad line for halfhour
ggplot(beton, aes(x=as.factor(time), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="median", geom="point")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux EC02 and EC04")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_diurnal_median_both_halfhour.pdf",
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
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
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
#as aggregated  median_mad line for each hour with errorbars
ggplot(beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="EC02 (concrete)"),stroke=2.5,
                   fun = "median",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=LE, col="EC04 (grass)"), 
                   fun="median", geom="point", stroke=2.5)+
  stat_summary(dat=beton, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar" , alpha=0.4, width=0.4)+
  stat_summary(dat=kiebitz, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar" ,alpha=0.4, width=0.4)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = " Median with errorbars displaying median absolute deviation" )+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  theme_bw()+
  theme(text = element_text(size=30), legend.position="bottom")

#save plot
ggsave(filename = "LE_Flux_diurnal_median_both_mad_errorbars_hour.png",
       device="png",width=350, height=210, units = "mm",
       path = "C:/00_Dana/Uni/Masterarbeit/Graduiertenkolloquium") #for presentation

#save plot
ggsave(filename = "LE_Flux_diurnal_both_median_mad_errorbars_hour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")
#as aggregated median line for each hour with errorbars
ggplot(beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="EC02 (concrete)"),stroke=2.5,
                   fun = "median",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=LE, col="EC04 (grass)"), 
                   fun="median", geom="point", stroke=2.5)+
  stat_summary(dat=beton, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar" , alpha=0.4, width=0.4)+
  stat_summary(dat=kiebitz, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar" ,alpha=0.4, width=0.4)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  theme_bw()+
  theme(text = element_text(size=30), legend.position="bottom")

#save plot
ggsave(filename = "LE_Flux_diurnal_median_both_mad_errorbars_hour.png",
       device="png",width=350, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated  median_mad line for each half hour
ggplot(beton, aes(x=as.factor(time), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=LE, col="kiebitz"), 
                   fun="median", geom="point")+
  stat_summary(dat=beton, aes(col="beton"), fun.data = "median_mad", 
               geom = "errorbar" )+
  stat_summary(dat=kiebitz, aes(col="kiebitz"),fun.data = "median_mad", 
               geom = "errorbar" )+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\n median with errorbars displaying median absolute deviation")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = "LE_Flux_diurnal_median_both_mad_errorbars_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#save plot
ggsave(filename = "LE_Flux_diurnal_median_both_mad_errorbars_halfhour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated  median line for hour
ggplot(beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point", size=5)+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="median", geom="point", size=5)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = " median_mad of EC02 (Beton) and EC04 (Kiebitz)")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = "LE_Flux_diurnal_median_both_hour.png",
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
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = "LE_Flux_diurnal_median_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated  median_mad line for halfhour
ggplot(beton, aes(x=as.factor(time), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="median", geom="point")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux EC02 and EC04")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  scale_color_manual("Color", values=c("#1f78b4", "#1b9e77"))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "LE_Flux_diurnal_median_both_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")
