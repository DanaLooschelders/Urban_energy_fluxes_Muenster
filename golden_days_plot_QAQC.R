library(ggpubr)

#Golden Days - Felix
#1.	06.08 12 Uhr – 11.08 6.00 Uhr
#2.	16.08 00 Uhr – 20.08 20 Uhr

#Golden Days - CT
#05-08-21 10:00 until 06-08-21 14:00
#07-08-21 10:00 until 08-08-21 14:00
#12-08-21 17:50 until 13-08-21 10:40

#need to manually source scripts 
#slow_data_to_30min_avg.r
#prep_flux_data_no_QAQC
#heat_fluxes_with_meteorology

#Subset Data
GDBeton_1<-dat.beton.flux.meteo[dat.beton.flux.meteo$TIMESTAMP>="2021-08-05 10:00:00"&dat.beton.flux.meteo$TIMESTAMP<="2021-08-06 14:00:00",]
GDKiebitz_1<-dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$TIMESTAMP>="2021-08-05 10:00:00"&dat.kiebitz.flux.meteo$TIMESTAMP<="2021-08-06 14:00:00",]

GDBeton_2<-dat.beton.flux.meteo[dat.beton.flux.meteo$TIMESTAMP>="2021-08-07 10:00:00"&dat.beton.flux.meteo$TIMESTAMP<="2021-08-08 14:00:00",]
GDKiebitz_2<-dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$TIMESTAMP>="2021-08-07 10:00:00"&dat.kiebitz.flux.meteo$TIMESTAMP<="2021-08-08 14:00:00",]

GDBeton_3<-dat.beton.flux.meteo[dat.beton.flux.meteo$TIMESTAMP>="2021-08-12 18:00:00"&dat.beton.flux.meteo$TIMESTAMP<="2021-08-13 10:30:00",]
GDKiebitz_3<-dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$TIMESTAMP>="2021-08-12 18:00:00"&dat.kiebitz.flux.meteo$TIMESTAMP<="2021-08-13 10:30:00",]

GD_QAQC<-function(Beton=GDBeton_1, Kiebitz=GDKiebitz_1, label_name="GD_1_"){
#define variables for each day
  GD_Beton<-Beton
  GD_Kiebitz<-Kiebitz
  label=label_name
#group QC in three groups
#Beton
#Sensible Heat
GD_Beton$qc_H[GD_Beton$qc_H>6]<-"poor"
GD_Beton$qc_H[GD_Beton$qc_H<=6&GD_Beton$qc_H>=4]<-"acceptable"
GD_Beton$qc_H[GD_Beton$qc_H<4]<-"very good"
#Latent Heat
GD_Beton$qc_LE[GD_Beton$qc_LE>6]<-"poor"
GD_Beton$qc_LE[GD_Beton$qc_LE<=6&GD_Beton$qc_LE>=4]<-"acceptable"
GD_Beton$qc_LE[GD_Beton$qc_LE<4]<-"very good"

#Kiebitz
#Sensible Heat
GD_Kiebitz$qc_H[GD_Kiebitz$qc_H>6]<-"poor"
GD_Kiebitz$qc_H[GD_Kiebitz$qc_H<=6&GD_Kiebitz$qc_H>=4]<-"acceptable"
GD_Kiebitz$qc_H[GD_Kiebitz$qc_H<4]<-"very good"
#Latent Heat
GD_Kiebitz$qc_LE[GD_Kiebitz$qc_LE>6]<-"poor"
GD_Kiebitz$qc_LE[GD_Kiebitz$qc_LE<=6&GD_Kiebitz$qc_LE>=4]<-"acceptable"
GD_Kiebitz$qc_LE[GD_Kiebitz$qc_LE<4]<-"very good"

#####sensible heat####
#H Kiebitz with QC as color
ggplot()+
  geom_line(dat=GD_Kiebitz,aes(x=datetime,y=H, color=qc_H, 
                             group=1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Golden Days - Sensible Heat flux EC04 with Flux Quality", 
          subtitle = "Quality: 1-3: very good  4-6: acceptable  7-9: poor")+
  scale_color_manual("Flux Quality", values = c("#1b9e77", "#7570b3","#d95f02"), 
                     breaks=c("very good", "acceptable", "poor"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()

#save plot
ggsave(filename = paste(label, "H_Flux_QC_EC04.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#H Beton with QC as color
ggplot()+
  geom_line(dat=GD_Beton,aes(x=datetime,y=H, color=qc_H, 
                             group=1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Golden Days - Sensible Heat flux EC02 with Flux Quality", 
          subtitle = "Quality: 1-3: very good  4-6: acceptable  7-9: poor")+
  scale_color_manual("Flux Quality", values = c("#1b9e77", "#7570b3","#d95f02"), 
                     breaks=c("very good", "acceptable", "poor"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()

#save plot
ggsave(filename = paste(label, "H_Flux_QC_EC02.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

####latent heat####
#LE Kiebitz with QC as color
ggplot()+
  geom_line(dat=GD_Kiebitz,aes(x=datetime,y=LE, color=qc_LE, 
                               group=1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Golden Days - Latent Heat flux EC04 with Flux Quality", 
          subtitle = )+
  scale_color_manual("Flux Quality", values = c("#1b9e77", "#7570b3","#d95f02"), 
                     breaks=c("very good", "acceptable", "poor"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()

#save plot
ggsave(filename = paste(label, "LE_Flux_QC_EC04.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

#LE Beton with QC as color
ggplot()+
  geom_line(dat=GD_Beton,aes(x=datetime,y=LE, color=qc_LE, 
                             group=1))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Golden Days - Latent Heat flux EC02 with Flux Quality", 
          subtitle = "Quality: 1-3: very good  4-6: acceptable  7-9: poor")+
  scale_color_manual("Flux Quality", values = c("#1b9e77", "#7570b3","#d95f02"), 
                     breaks=c("very good", "acceptable", "poor"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()

#save plot
ggsave(filename = paste(label, "LE_Flux_QC_EC02.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

####with meteorology####
dif<-GD_Beton$H-GD_Kiebitz$H
H_flux<-ggplot()+
  geom_line(aes(x=GD_Kiebitz$TIMESTAMP, y=dif))+
  ggtitle("Difference H Flux EC02-EC04")+
  theme_bw(base_size=8)+
  geom_hline(yintercept = 0, color="red")+
  ylab("Heat Flux [W/m2]")+
  xlab("time")

#wind speed
windspeed_beton<-ggplot(data=GD_Beton)+
  geom_line(aes(x=TIMESTAMP, y=wind_speed))+
  theme_bw(base_size=8)+
  ggtitle("Wind speed EC02")+
  ylab("Wind speed [m/s]")+
  xlab("time")

windspeed_kiebitz<-ggplot(data=GD_Kiebitz)+
  geom_line(aes(x=TIMESTAMP, y=wind_speed))+
  theme_bw(base_size=8)+
  ggtitle("Wind speed EC04")+
  ylab("Wind speed [m/s]")+
  xlab("time")

#wind direction
winddir_beton<-ggplot(data=GD_Beton)+
  geom_point(aes(x=TIMESTAMP, y=wind_dir))+
  theme_bw(base_size=8)+
  ggtitle("Wind direction EC02")+
  ylab("Wind dir [°]")+
  xlab("time")

winddir_kiebitz<-ggplot(data=GD_Kiebitz)+
  geom_point(aes(x=TIMESTAMP, y=wind_dir))+
  theme_bw(base_size=8)+
  ggtitle("Wind direction EC04")+
  ylab("Wind dir [°]")+
  xlab("time")

#incoming shortwave radiation
rad_beton<-ggplot(data=GD_Beton)+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg_beton))+
  theme_bw(base_size=8)+
  ggtitle("Incoming short wave radiation EC02")+
  ylab("Radiation [W/m2]")+
  xlab("time")

rad_kiebitz<-ggplot(data=GD_Kiebitz)+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg_kiebitz))+
  theme_bw(base_size=8)+
  ggtitle("Incoming short wave radiation EC04")+
  ylab("Radiation [W/m2]")+
  xlab("time")

multi_beton <- ggarrange(H_flux, winddir_beton, windspeed_beton,rad_beton,
                    nrow=4)
multi_beton

ggsave(filename = paste(label, "multiplot_flux_dif_meteo.png"),
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Golden_Days")

}

#call function once for every golden day

#GD 1
GD_QAQC(Beton=GDBeton_1, Kiebitz=GDKiebitz_1, label_name="GD_1_")

#GD 2
GD_QAQC(Beton=GDBeton_2, Kiebitz=GDKiebitz_2, label_name="GD_2_")

#GD 3
GD_QAQC(Beton=GDBeton_3, Kiebitz=GDKiebitz_3, label_name="GD_3_")
