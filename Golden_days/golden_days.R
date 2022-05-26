#Golden Days - Felix
#1.	06.08 12 Uhr – 11.08 6.00 Uhr
#2.	16.08 00 Uhr – 20.08 20 Uhr

#Golden Days - CT
#05-08-21 10:00 until 06-08-21 14:00
#07-08-21 10:00 until 08-08-21 14:00
#12-08-21 17:50 until 13-08-21 10:40

#Subset Data
GDBeton_1<-dat.beton.flux.meteo[dat.beton.flux.meteo$TIMESTAMP>="2021-08-05 10:00:00"&dat.beton.flux.meteo$TIMESTAMP<="2021-08-06 14:00:00",]
GDKiebitz_1<-dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$TIMESTAMP>="2021-08-05 10:00:00"&dat.kiebitz.flux.meteo$TIMESTAMP<="2021-08-06 14:00:00",]

GDBeton_2<-dat.beton.flux.meteo[dat.beton.flux.meteo$TIMESTAMP>="2021-08-07 10:00:00"&dat.beton.flux.meteo$TIMESTAMP<="2021-08-08 14:00:00",]
GDKiebitz_2<-dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$TIMESTAMP>="2021-08-07 10:00:00"&dat.kiebitz.flux.meteo$TIMESTAMP<="2021-08-08 14:00:00",]

GDBeton_3<-dat.beton.flux.meteo[dat.beton.flux.meteo$TIMESTAMP>="2021-08-12 18:00:00"&dat.beton.flux.meteo$TIMESTAMP<="2021-08-13 10:30:00",]
GDKiebitz_3<-dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$TIMESTAMP>="2021-08-12 18:00:00"&dat.kiebitz.flux.meteo$TIMESTAMP<="2021-08-13 10:30:00",]

#Script works for both "Golden Days" - just outcomment what is needed
GD_Beton<-GDBeton_1
GD_Kiebitz<-GDKiebitz_1
label="GD_1_"

GD_Beton<-GDBeton_2
GD_Kiebitz<-GDKiebitz_2
label="GD_2_"

GD_Beton<-GDBeton_3
GD_Kiebitz<-GDKiebitz_3
label="GD_3_"
#flux differences
#####sensible heat - time series####

#plot EC02 and EC04 together
ggplot()+
  geom_line(dat=GD_Beton,aes(x=datetime, y=H, color="EC02 Beton"))+
  geom_line(dat=GD_Kiebitz,aes(x=datetime,y=H, color="EC04 Kiebitz"))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Golden Days - Sensible Heat flux EC02 and EC04")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = paste(label, "H_Flux_both_timeseries.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#plot difference between EC02 and EC04
GD_dif<-data.frame(datetime=GD_Beton$datetime, H_beton=GD_Beton$H)
GD_dif$H_kiebitz<-GD_Kiebitz$H
GD_dif$flux_dif_H<-GD_Beton$H-GD_Kiebitz$H

##Sensible Heat Flux difference
ggplot(dat=GD_dif, aes(x=datetime, y=flux_dif_H))+
  geom_line()+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label="Golden Days - Sensible Heat Flux Difference EC02-EC04")+
  ylab(bquote('H flux difference [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = paste(label, "H_Flux_dif_both.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

##integrate area under curve (for negative values)
##to quatify how much heat is going in which tower

#####sensible heat - diurnal####
#as aggregated mean line for each hour with errorbars
ggplot(GD_Beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=GD_Beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  stat_summary(dat=GD_Kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\nMean with errorbars displaying 1 SD" )+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = paste(label, "H_Flux_diurnal_mean_both_1sd_errorbars_hour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#as aggregated mean line for each half hour
ggplot(GD_Beton, aes(x=as.factor(time), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=GD_Beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  stat_summary(dat=GD_Kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\nMean with errorbars displaying 1 SD")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = paste(label, "H_Flux_diurnal_mean_both_1sd_errorbars_halfhour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#as aggregated mean line for hour
ggplot(GD_Beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=5)+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(hour), 
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
ggsave(filename = paste(label, "H_Flux_diurnal_mean_both_hour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#as aggregated median line for hour
ggplot(GD_Beton, aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point", size=5)+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(hour), 
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
ggsave(filename = paste(label, "H_Flux_diurnal_median_both_hour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#as aggregated mean line for halfhour
ggplot(GD_Beton, aes(x=as.factor(time), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux EC02 and EC04")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = paste(label, "H_Flux_diurnal_mean_both_halfhour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")


#####latent heat - time series####
#plot EC02 and EC04 together
#remove outlier 
GD_Beton$LE[GD_Beton$LE<=-100]<-NA

ggplot()+
  geom_line(dat=GD_Beton,aes(x=datetime, y=LE, color="EC02 Beton"))+
  geom_line(dat=GD_Kiebitz,aes(x=datetime,y=LE, color="EC04 Kiebitz"))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Latent Heat flux EC02 and EC04")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = paste(label, "LE_Flux_both.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#plot difference between EC02 and EC04
GD_dif$LE_beton<-GD_Beton$LE
GD_dif$LE_kiebitz<-GD_Kiebitz$LE
GD_dif$flux_dif_LE<-GD_Beton$LE-GD_Kiebitz$LE

##Sensible Heat Flux difference
ggplot(dat=GD_dif, aes(x=datetime, y=flux_dif_LE))+
  geom_line()+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label="Latent Heat Flux Difference EC02-EC04")+
  ylab(bquote('LE flux difference [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = paste(label, "LE_Flux_dif_both.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#####latent heat - diurnal####
#as aggregated mean line for each hour with errorbars
ggplot(GD_Beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(dat=GD_Beton, aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(hour), 
                                    y=LE, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=GD_Beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar",fun.args = list(mult = 1))+
  stat_summary(dat=GD_Kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\nMean with errorbars displaying one SD")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = paste(label, "LE_Flux_diurnal_both_mean_1sd_errorbars_hour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#as aggregated mean line for each half hour
ggplot(GD_Beton, aes(x=as.factor(time), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(time), 
                                    y=LE, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=GD_Beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  stat_summary(dat=GD_Kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux", 
          subtitle = "EC02 (Beton) and EC04 (Kiebitz)\nMean with errorbars displaying 1 SD")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()

#save plot
ggsave(filename = paste(label, "LE_Flux_diurnal_mean_both_1sd_errorbars_halfhour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#as aggregated mean line for hour
ggplot(GD_Beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=5)+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(hour), 
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
ggsave(filename = paste(label, "LE_Flux_diurnal_mean_both_hour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#as aggregated median line for hour
ggplot(GD_Beton, aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "median",geom="point", size=5)+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(hour), 
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
ggsave(filename = paste(label, "LE_Flux_diurnal_median_both_hour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#as aggregated mean line for halfhour
ggplot(GD_Beton, aes(x=as.factor(time), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=GD_Kiebitz, aes(x=as.factor(time), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux EC02 and EC04")+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = paste(label, "LE_Flux_diurnal_mean_both_halfhour.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

