#compare fluxes for energy balance rations
library(bigleaf)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(grid)
library(tidyverse)
library(dplyr)
library(lubridate)
library(reshape2)
library(colorspace)
library(scico)
library(patchwork)
library(scales)
library(tidyverse)
library(plotly)
library(Hmisc)
library(ggpubr)
library(gridExtra)

#if grass
grass.flux.meteo<-cbind(dat.kiebitz.flux.meteo, meteo_kiebitz)

####QAQC####
#QAQC
##exclude fluxes with qc values >6
grass.flux.meteo$co2_flux[grass.flux.meteo$qc_co2_flux>6]<-NA
grass.flux.meteo$h2o_flux[grass.flux.meteo$qc_h2o_flux>6]<-NA
grass.flux.meteo$LE[grass.flux.meteo$qc_LE>6]<-NA
grass.flux.meteo$Tau[grass.flux.meteo$qc_Tau>6]<-NA
grass.flux.meteo$H[grass.flux.meteo$qc_H>6]<-NA
#exclude unreasonable radiation values
#latent heat
plot(grass.flux.meteo$TIMESTAMP, grass.flux.meteo$LE, type="b") #plot


#sensible heat
plot(grass.flux.meteo$TIMESTAMP, grass.flux.meteo$H, type="b")

#soil heat flux
plot(grass.flux.meteo$TIMESTAMP, grass.flux.meteo$shf, type="b")

#get the first row with no NA values for the column needed
index_start=which(!is.na(rowSums(grass.flux.meteo[,c("LE", 
                                                   "H", 
                                                   "TotRNet_Avg", 
                                                   "shf")])))[1]
index_end=length(grass.flux.meteo$TIMESTAMP)
#change timespan to time where all parameters occured
grass.flux.meteo<-grass.flux.meteo[index_start:index_end,]
grass.flux.meteo$AirTC_Avg
grass.flux.meteo <- grass.flux.meteo [,c("TIMESTAMP",'LE','H', "TotRNet_Avg", "shf", "SDn_Avg", "SUp_Avg",
                              "LDnCo_Avg", "LUpCo_Avg", "AirTC_Avg")]

grass.flux.meteo$TotRNet_Avg_2<-grass.flux.meteo$SDn_Avg-grass.flux.meteo$SUp_Avg+
  grass.flux.meteo$LDnCo_Avg-grass.flux.meteo$LUpCo_Avg

##### Beton ####
concrete.flux.meteo<-cbind(dat.beton.flux.meteo, meteo_beton)

#QAQC
##exclude fluxes with qc values >6
concrete.flux.meteo$co2_flux[concrete.flux.meteo$qc_co2_flux>6]<-NA
concrete.flux.meteo$h2o_flux[concrete.flux.meteo$qc_h2o_flux>6]<-NA
concrete.flux.meteo$LE[concrete.flux.meteo$qc_LE>6]<-NA
concrete.flux.meteo$Tau[concrete.flux.meteo$qc_Tau>6]<-NA
concrete.flux.meteo$H[concrete.flux.meteo$qc_H>6]<-NA
#exclude unreasonable radiation values
#latent heat
plot(concrete.flux.meteo$TIMESTAMP, concrete.flux.meteo$LE, type="b") #plot

#sensible heat
plot(concrete.flux.meteo$TIMESTAMP, concrete.flux.meteo$H, type="b")

plot(concrete.flux.meteo$TIMESTAMP, concrete.flux.meteo$H, type="b")

#soil heat flux
plot(concrete.flux.meteo$TIMESTAMP, concrete.flux.meteo$shf, type="b")

#get the first row with no NA values for the column needed
index_start=which(!is.na(rowSums(concrete.flux.meteo[,c("LE", 
                                                   "H", 
                                                   "TotRNet_Avg", 
                                                   "shf")])))[1]

index_end=length(concrete.flux.meteo$TIMESTAMP)
#change timespan to time where all parameters occured
concrete.flux.meteo.cut<-concrete.flux.meteo[index_start:index_end,]

concrete.flux.meteo <- concrete.flux.meteo [,c("TIMESTAMP", "LE","H", "TotRNet_Avg", "shf", "SDn_Avg", "SUp_Avg",
                                         "LDnCo_Avg", "LUpCo_Avg",  "AirTC_Avg")]

concrete.flux.meteo$TotRNet_Avg_2<-concrete.flux.meteo$SDn_Avg-concrete.flux.meteo$SUp_Avg+
  concrete.flux.meteo$LDnCo_Avg-concrete.flux.meteo$LUpCo_Avg
#get to same length
concrete.flux.meteo<-concrete.flux.meteo[concrete.flux.meteo$TIMESTAMP>=range(grass.flux.meteo$TIMESTAMP)[1]&
                                           concrete.flux.meteo$TIMESTAMP<=range(grass.flux.meteo$TIMESTAMP)[2],]

#calculate net radiation manually 
#to make sure it is only calculated for time periods where all rad components are available
####concrete
range(concrete.flux.meteo$TIMESTAMP)
any(is.na(concrete.flux.meteo$SUp_Avg))
any(is.na(concrete.flux.meteo$LUpCo_Avg))
any(is.na(concrete.flux.meteo$SDn_Avg))
any(is.na(concrete.flux.meteo$LDnCo_Avg))

concrete.flux.meteo$TotRNet_calc<-concrete.flux.meteo$SUp_Avg+concrete.flux.meteo$LUpCo_Avg-
  concrete.flux.meteo$SDn_Avg-concrete.flux.meteo$LDnCo_Avg

plot(concrete.flux.meteo$TotRNet_Avg-concrete.flux.meteo$TotRNet_calc, type="l")
####grass
#EB_data_grass_complete$SDn_Avg[1:100]==grass.flux.meteo$SDn_Avg[1:100]

length(grass.flux.meteo$TIMESTAMP)
any(is.na(grass.flux.meteo$SUp_Avg))
length(grass.flux.meteo$SUp_Avg[is.na(grass.flux.meteo$SUp_Avg)])
any(is.na(grass.flux.meteo$LUpCo_Avg))
length(grass.flux.meteo$LUpCo_Avg[is.na(grass.flux.meteo$LUpCo_Avg)])
any(is.na(grass.flux.meteo$SDn_Avg))
length(grass.flux.meteo$SDn_Avg[is.na(grass.flux.meteo$SDn_Avg)])
any(is.na(grass.flux.meteo$LDnCo_Avg))
length(grass.flux.meteo$LDnCo_Avg[is.na(grass.flux.meteo$LDnCo_Avg)])

grass.flux.meteo$TotRNet_calc<-grass.flux.meteo$SUp_Avg+grass.flux.meteo$LUpCo_Avg-
  grass.flux.meteo$SDn_Avg-grass.flux.meteo$LDnCo_Avg

plot(grass.flux.meteo$TotRNet_Avg-grass.flux.meteo$TotRNet_calc, type="l")

grass.flux.meteo$hour<-hour(grass.flux.meteo$TIMESTAMP)
concrete.flux.meteo$hour<-hour(concrete.flux.meteo$TIMESTAMP)
#save both as csv for easy loading
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
write.csv(concrete.flux.meteo, "concrete_flux_meteo_20230217.csv", row.names=F)
write.csv(grass.flux.meteo, "grass_flux_meteo_20230217.csv", row.names=F)

#load
grass.flux.meteo<-read.csv("grass_flux_meteo_20230217.csv")
grass.flux.meteo$TIMESTAMP<-as.POSIXct(grass.flux.meteo$TIMESTAMP)
concrete.flux.meteo<-read.csv("concrete_flux_meteo_20230217.csv")
concrete.flux.meteo$TIMESTAMP<-as.POSIXct(concrete.flux.meteo$TIMESTAMP)
#for 4th August
#check time
#grass.flux.meteo$TIMESTAMP[604:651]
#concrete.flux.meteo$TIMESTAMP[604:651]
#subset
#grass.flux.meteo<-grass.flux.meteo[604:651,]
#concrete.flux.meteo<-concrete.flux.meteo[604:651,]

#plot all fluxes of grass
ggplot(data=concrete.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg, col="SUp_Avg"), stat="summary", fun="median")+
  geom_line(aes(x=TIMESTAMP, y=SDn_Avg, col="SDn_Avg"), stat="summary", fun="median")+
  geom_line(aes(x=TIMESTAMP, y=LUpCo_Avg, col="LUpCo_Avg"), stat="summary", fun="median")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line(aes(x=TIMESTAMP, y=LDnCo_Avg, col="LDnCo_Avg"), stat="summary", fun="median")+
  theme_bw()


ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("all_fluxes_grass.png"),
       width=297, height=210, units = "mm")

#stat summary with points for median values
ggplot(dat=grass.flux.meteo)+
  ylab(label="Flux [W m^-2]")+
  ggtitle("Mean day of gras fluxes")+
  stat_summary(aes(x=hour, y=LE, col="LE", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=H, col="H", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=TotRNet_Avg_2, col="TotRad", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=shf, col="shf", group=hour), fun.y=median)+
  theme_bw()

#regression of components
ggplot(dat=concrete.flux.meteo)+
  geom_point(aes(x=TotRNet_Avg_2*-1-shf, y=LE+H))+
  theme_bw()+
  ylab(label="LE+H [W m^-2]")+
  xlab(label="-NetR-SHF [W m^-2]")

#meteo_all$TIMESTAMP>="2021-08-12 09:30:00"&
#meteo_all$TIMESTAMP<="2021-08-12 10:50:00",
ggplot(data=concrete.flux.meteo)+
  geom_line(aes(x=TIMESTAMP))
#plot all fluxes of concrete
ggplot(data=concrete.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg_2, col="TotRNet_Avg"))+
  geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
  geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
  geom_line(aes(x=TIMESTAMP, y=shf*-1, col="shf"))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("all_fluxes_concrete.png"),
       width=297, height=210, units = "mm")

ggplot(dat=concrete.flux.meteo)+
  ylab(label="Flux [W m^-2]")+
  ggtitle("Mean day of concrete fluxes")+
  stat_summary(aes(x=hour, y=LE, col="LE", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=H, col="H", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=TotRNet_Avg_2, col="TotRad", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=shf, col="shf", group=hour), fun.y=median)+
  theme_bw()


##Function calculates energy balance ratio EBR = sum(LE + H)/sum(Rn − G − S)
#plot EBR components for grass
ggplot(dat=grass.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=LE+H, col="LE+H"))+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg_2-shf, col="TotRad-shf"))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LEH_totradshf_grass.png"),
       width=297, height=210, units = "mm")

#regression of components
ggplot(dat=grass.flux.meteo)+
  geom_point(aes(x=(TotRNet_Avg_2*-1)-shf, y=LE+H))+
  theme_bw()+
  ylab(label="LE+H [W m^-2]")+
  xlab(label="NetR-SHF [W m^-29")

#hourly duiurnal 
ggplot(dat=grass.flux.meteo)+
  geom_boxplot(aes(x=hour, y=LE+H, col="LE+H", group=hour))+
  geom_boxplot(aes(x=hour, y=(TotRNet_Avg_2)-shf, col="TotRad-shf", group=hour))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("hourly_LEH_vs_totradshf_grass.png"),
       width=297, height=210, units = "mm")


ggplot(dat=grass.flux.meteo)+
  ylab("Mean Flux [W m^-2]")+
  stat_summary(aes(x=hour, y=LE+H, col="LE+H", group=hour), fun.y=median)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), 
                     sec.axis=sec_axis(trans=~./100, name="EBR"))+
  stat_summary(aes(x=hour, y=(TotRNet_Avg_2)-shf, col="TotRad-shf", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=(LE+H)/(TotRNet_Avg-shf)*100, col="EBR", group=hour), fun.y=median)+
  theme_bw()

#plot EBR components for concrete
ggplot(dat=concrete.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=LE+H, col="LE+H"))+
  geom_line(aes(x=TIMESTAMP, y=(TotRNet_Avg_2)-shf, col="TotRad-shf"))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LE_H_concrete.png"),
       width=297, height=210, units = "mm")

ggplot(dat=concrete.flux.meteo)+
  geom_boxplot(aes(x=hour, y=LE+H, col="LE+H", group=hour))+
  geom_boxplot(aes(x=hour, y=(TotRNet_Avg_2)-shf, col="TotRad-shf", group=hour))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("hourly_LEH_vs_totradshf_concrete.png"),
       width=297, height=210, units = "mm")

ggplot(dat=concrete.flux.meteo)+
  ylab(label="Flux [W m^-2]")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), 
                     sec.axis=sec_axis(trans=~./100, name="EBR"))+
  stat_summary(aes(x=hour, y=LE+H, col="LE+H", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=(TotRNet_Avg_2)-shf, col="TotRad-shf", group=hour), fun.y=median)+
  stat_summary(aes(x=hour, y=(LE+H)/(TotRNet_Avg-shf)*100, col="EBR", group=hour), fun.y=median)+
  theme_bw()

#####compare fluxes between grass and concrete####


#Total Net Radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=TotRNet_Avg_2, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=TotRNet_Avg_2, col="concrete"))+
  theme_bw()+
  ggtitle(label="Total Net Radiation")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("TotENet_concrete_grass.png"),
       width=297, height=210, units = "mm")
#diurnal net rad
ggplot()+
  geom_boxplot(data=grass.flux.meteo, aes(x=hour, y=TotRNet_Avg_2, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ylab(label="Tot Net Radiation [W m^-2]")+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle(label="TotRNet - grass")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("TotENet_hourly_grass.png"),
       width=297, height=210, units = "mm")
ggplot()+
  geom_boxplot(data=concrete.flux.meteo, aes(x=hour, y=TotRNet_Avg_2, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ylab(label="Tot Net Radiation [W m^-2]")+
  ggtitle(label="TotRNet - concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("TotENet_hourly_concrete.png"),
       width=297, height=210, units = "mm")
#check difference 
diff_TotRNet<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff"=grass.flux.meteo$TotRNet_Avg-concrete.flux.meteo$TotRNet_Avg)
any(grass.flux.meteo$TIMESTAMP!=concrete.flux.meteo$TIMESTAMP) #make sure timestamps fit
#plot difference
ggplot(data=diff_TotRNet)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("TotENet_Diff_concrete_grass.png"),
       width=297, height=210, units = "mm")
diff_TotRNet$hour<-hour(diff_TotRNet$TIMESTAMP)
diff_TotRNet_melted<-melt(diff_TotRNet, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_TotRNet)+
  geom_boxplot(aes(x=hour, y=diff, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle(label="difference TotRNet grass - concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("TotENet_diff_hourly_concrete_grass.png"),
       width=297, height=210, units = "mm")
#grass has more energy available during the night
#concrete has more available energy during the day

#####plot incoming shortwave radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="Incoming SW")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("SWUp_concrete_grass.png"),
       width=297, height=210, units = "mm")
#check difference 
diff_SUp<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff"=grass.flux.meteo$SUp_Avg-concrete.flux.meteo$SUp_Avg)
#lag +2 -> + 1h: lag(grass.flux.meteo$SUp_Avg,n=2)-concrete.flux.meteo$SUp_Avg)
#lag -2 -> -1h: grass.flux.meteo$SUp_Avg-lag(concrete.flux.meteo$SUp_Avg, n=2))
any(grass.flux.meteo$TIMESTAMP!=concrete.flux.meteo$TIMESTAMP) #make sure timestamps fit

#test with one hour lag
#plot difference
ggplot(data=diff_SUp)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()+
  ylab(label="Difference SUp [W m^-2]")+
  ggtitle(label="Difference grass - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("SWUp_diff_concrete_grass.png"),
       width=297, height=210, units = "mm")
diff_SUp$hour<-hour(diff_SUp$TIMESTAMP)
diff_SUp_melted<-melt(diff_SUp, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_SUp)+
  geom_boxplot(aes(x=hour, y=diff, group=hour))+
  theme_bw()+
  geom_hline(aes(yintercept=0), col="red")+
  ylab(label="Diff in SWUp [W m^-2]")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle(label="difference SUp grass - concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("SWUp_diff_hourly_concrete_grass.png"),
       width=297, height=210, units = "mm")
#that is weird, because there seems to be a systematic difference between grass and concrete
#scattering? 
#timestamp kiebitz? -> no
#angle difference? -> impossible

#####plot reflected shortwave radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=SDn_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=SDn_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="reflected SW")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("SDn_concrete_grass.png"),
       width=297, height=210, units = "mm")

#check difference 
diff_SDn<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff"=grass.flux.meteo$SDn_Avg-concrete.flux.meteo$SDn_Avg)
#plot difference
ggplot(data=diff_SDn)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()+
  ylab(label="Difference SDn [W m^-2]")+
  ggtitle(label="Difference grass - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("SWDn_diff_concrete_grass.png"),
       width=297, height=210, units = "mm")
diff_SDn$hour<-hour(diff_SDn$TIMESTAMP)
diff_SDn_melted<-melt(diff_SDn, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_SDn)+
  geom_boxplot(aes(x=hour, y=diff, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle(label="difference SDn grass - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("SWDn_diff_hourly_concrete_grass.png"),
       width=297, height=210, units = "mm")
#grass reflects more SW radiation
#grass should reflect more than wet concrete but less than dry concrete
#our grass surface has a higher albedo than our concrete surface

####plot outgoing longwave radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=LDnCo_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=LDnCo_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="outgoing LW")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LDn_concrete_grass.png"),
       width=297, height=210, units = "mm")

#check difference 
diff_LDn<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff"=grass.flux.meteo$LDnCo_Avg-concrete.flux.meteo$LDnCo_Avg)
#plot difference
ggplot(data=diff_LDn)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()+
ylab(label="Difference LDn [W m^-2]")+
  ggtitle(label="Difference grass - concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LDn_diff_concrete_grass.png"),
       width=297, height=210, units = "mm")

diff_LDn$hour<-hour(diff_LDn$TIMESTAMP)
diff_LDn_melted<-melt(diff_LDn, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_LDn)+
  geom_boxplot(aes(x=hour, y=diff, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ylab(label="Difference LDn [W m^-2]")+
  ggtitle(label="difference LDn grass - concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LDn_diff_hourly_concrete_grass.png"),
       width=297, height=210, units = "mm")
#concrete has more upwelling LW rad -> makes sense
#least difference in the early morning after sunrise

#####plot reflected longwave radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=LUpCo_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=LUpCo_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="reflected LW")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LUp_concrete_grass.png"),
       width=297, height=210, units = "mm")
#check difference 
diff_LUp<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff"=grass.flux.meteo$LUpCo_Avg-concrete.flux.meteo$LUpCo_Avg)
#plot difference
ggplot(data=diff_LUp)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()+
  ylab(label="Difference LDn [W m^-2]")+
  ggtitle(label="Difference grass - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LUp_diff_concrete_grass.png"),
       width=297, height=210, units = "mm")
diff_LUp$hour<-hour(diff_LUp$TIMESTAMP)
diff_LUp_melted<-melt(diff_LUp, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_LUp)+
  geom_boxplot(aes(x=hour, y=diff, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ylab(label="Difference LDn [W m^-2]")+
  ggtitle(label="difference LDn grass - concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LUp_diff_hourly_concrete_grass.png"),
       width=297, height=210, units = "mm")

#Latent Heat
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=LE, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=LE, col="concrete"))+
  theme_bw()+
  ylab(label="LE [W m^-2]")+
  ggtitle(label="Latent Heat")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LE_concrete_grass.png"),
       width=297, height=210, units = "mm")
#check difference 
diff_LE<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff"=grass.flux.meteo$LE-concrete.flux.meteo$LE)
#plot difference
ggplot(data=diff_LE)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()+
  ylab(label="Difference LE [W m^-2]")+
  ggtitle(label="Difference grass - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LE_diff_concrete_grass.png"),
       width=297, height=210, units = "mm")
diff_LE$hour<-hour(diff_LE$TIMESTAMP)
diff_LE_melted<-melt(diff_LE, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_LE)+
  geom_boxplot(aes(x=hour, y=diff, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_hline(aes(yintercept=0), col="red")+
  ylab(label="Difference LE [W m^-2]")+
  ggtitle(label="difference LE grass - concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LE_diff_hourly_concrete_grass.png"),
       width=297, height=210, units = "mm")
#Grass has more latent heat flux --> makes sense

#Sensible Heat
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=H, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=H, col="concrete"))+
  theme_bw()+
  ylab(label="H [W m^-2]")+
  ggtitle(label="LDnCo_Avg")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("H_concrete_grass.png"),
       width=297, height=210, units = "mm")

#check difference 
diff_H<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                    "diff"=grass.flux.meteo$H-concrete.flux.meteo$H)
#plot difference
ggplot(data=diff_H)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()+
  ylab(label="Difference H [W m^-2]")+
  ggtitle(label="Difference grass - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("H_diff_concrete_grass.png"),
       width=297, height=210, units = "mm")
diff_H$hour<-hour(diff_H$TIMESTAMP)
diff_H_melted<-melt(diff_H, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_H)+
  geom_boxplot(aes(x=hour, y=diff, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_hline(aes(yintercept=0), col="red")+
  ylab(label="Difference H [W m^-2]")+
  ggtitle(label="difference H grass - concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("H_diff_hourly_concrete_grass.png"),
       width=297, height=210, units = "mm")

#grass has more latent heat in the morning

#Soil Heat flux
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=shf, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=shf, col="concrete"))+
  theme_bw()+
  ylab(label="SHF [W m^-2]")+
  ggtitle(label="Soil heat flux")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("SHF_concrete_grass.png"),
       width=297, height=210, units = "mm")

ggplot(data=grass.flux.meteo)+
  geom_boxplot(aes(x=hour, y=shf, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle(label="hourly shf - grass")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("shf_hourly_grass.png"),
       width=297, height=210, units = "mm")

ggplot(data=concrete.flux.meteo)+
  geom_boxplot(aes(x=hour, y=shf, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle(label="hourly shf - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("shf_hourly_concrete.png"),
       width=297, height=210, units = "mm")

#check difference 
diff_shf<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff"=grass.flux.meteo$shf-concrete.flux.meteo$shf)
#plot difference
ggplot(data=diff_shf)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()+
  ylab(label="Difference SDn [W m^-2]")+
  ggtitle(label="Difference grass - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("shf_diff_concrete_grass.png"),
       width=297, height=210, units = "mm")
diff_shf$hour<-hour(diff_shf$TIMESTAMP)
diff_shf_melted<-melt(diff_shf, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_shf)+
  geom_boxplot(aes(x=hour, y=diff, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle(label="difference SDn grass - concrete")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("shf_diff_hourly_concrete_grass.png"),
       width=297, height=210, units = "mm")


#plot stepwise closure as mean hour of the day

#test energy balance during the day and during the night

#calculate what percentage fluxes have of net rad
concrete.flux.meteo$TotRNet_Avg_2<-concrete.flux.meteo$SDn_Avg-concrete.flux.meteo$SUp_Avg+
  concrete.flux.meteo$LDnCo_Avg-concrete.flux.meteo$LUpCo_Avg
