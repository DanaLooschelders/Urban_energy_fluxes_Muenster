#compare fluxes for energy balance rations
library(bigleaf)
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
#remove values below zero
any(grass.flux.meteo$LE<0)
grass.flux.meteo$LE[grass.flux.meteo$LE<0]<-0

#sensible heat
plot(grass.flux.meteo$TIMESTAMP, grass.flux.meteo$H, type="b")
#remove values below zero
any(grass.flux.meteo$H<0) 
grass.flux.meteo$H[grass.flux.meteo$H<0]<-0
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

grass.flux.meteo <- grass.flux.meteo [,c("TIMESTAMP",'LE','H', "TotRNet_Avg", "shf", "SDn_Avg", "SUp_Avg",
                              "LDnCo_Avg", "LUpCo_Avg")]
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
#remove values below zero
any(concrete.flux.meteo$LE<0)
concrete.flux.meteo$LE[concrete.flux.meteo$LE<0]<-0

#sensible heat
plot(concrete.flux.meteo$TIMESTAMP, concrete.flux.meteo$H, type="b")
#remove values below zero
any(concrete.flux.meteo$H<0) 
concrete.flux.meteo$H[concrete.flux.meteo$H<0]<-0
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
                                         "LDnCo_Avg", "LUpCo_Avg")]
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

#save both as csv for easy loading
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
write.csv(concrete.flux.meteo, "concrete_flux_meteo.csv", row.names=F)
write.csv(grass.flux.meteo, "grass_flux_meteo.csv", row.names=F)

#load
grass.flux.meteo<-read.csv("grass_flux_meteo.csv")
grass.flux.meteo$TIMESTAMP<-as.POSIXct(grass.flux.meteo$TIMESTAMP)
concrete.flux.meteo<-read.csv("concrete_flux_meteo.csv")
concrete.flux.meteo$TIMESTAMP<-as.POSIXct(concrete.flux.meteo$TIMESTAMP)
#for 4th August
#check time
#grass.flux.meteo$TIMESTAMP[604:651]
#concrete.flux.meteo$TIMESTAMP[604:651]
#subset
#grass.flux.meteo<-grass.flux.meteo[604:651,]
#concrete.flux.meteo<-concrete.flux.meteo[604:651,]

#plot all fluxes of grass
ggplot(data=grass.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg, col="TotRNet_Avg"))+
  geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
  geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
  geom_line(aes(x=TIMESTAMP, y=shf*-1, col="shf"))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("all_fluxes_grass.png"),
       width=297, height=210, units = "mm")


#plot all fluxes of concrete
ggplot(data=concrete.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg, col="TotRNet_Avg"))+
  geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
  geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
  geom_line(aes(x=TIMESTAMP, y=shf*-1, col="shf"))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("all_fluxes_concrete.png"),
       width=297, height=210, units = "mm")

##Function calculates energy balance ratio EBR = sum(LE + H)/sum(Rn − G − S)
#plot EBR components for grass
ggplot(dat=grass.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=LE+H, col="LE+H"))+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg-shf*-1, col="TotRad-shf"))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LE_H_grass.png"),
       width=297, height=210, units = "mm")
#plot EBR components for concrete
ggplot(dat=concrete.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=LE+H, col="LE+H"))+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg-shf*-1, col="TotRad-shf"))+
  theme_bw()
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("LE_H_concrete.png"),
       width=297, height=210, units = "mm")
#####compare fluxes between grass and concrete####
grass.flux.meteo$hour<-hour(grass.flux.meteo$TIMESTAMP)
concrete.flux.meteo$hour<-hour(concrete.flux.meteo$TIMESTAMP)

#Total Net Radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=TotRNet_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=TotRNet_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="Total Net Radiation")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("TotENet_concrete_grass.png"),
       width=297, height=210, units = "mm")
#diurnal net rad
ggplot()+
  geom_boxplot(data=grass.flux.meteo, aes(x=hour, y=TotRNet_Avg, group=hour))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ylab(label="Tot Net Radiation [W m^-2]")+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle(label="TotRNet - grass")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("TotENet_hourly_grass.png"),
       width=297, height=210, units = "mm")
ggplot()+
  geom_boxplot(data=concrete.flux.meteo, aes(x=hour, y=TotRNet_Avg, group=hour))+
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

####Energy Balance for concrete ####
#for time steps
EB_stepwise_concrete<-energy.closure(data=concrete.flux.meteo,instantaneous = TRUE, 
                                     G=concrete.flux.meteo$shf*-1, 
                                     Rn = concrete.flux.meteo$TotRNet_Avg,
                                    LE=concrete.flux.meteo$LE, 
                                     H=concrete.flux.meteo$H)
EB_step_concrete<-data.frame("EB"=EB_stepwise_concrete, 
                             "datetime"=concrete.flux.meteo$TIMESTAMP)
#take only complete observations
EB_data_concrete_complete<-concrete.flux.meteo[complete.cases(concrete.flux.meteo),]
EB_data_concrete_complete$day<-date(EB_data_concrete_complete$TIMESTAMP)
#calculate energy balance for every individual day
concrete.flux.meteo$day<-date(concrete.flux.meteo$TIMESTAMP) #create column with day
EB_day_concrete<-data.frame("day"=unique(concrete.flux.meteo$day), "EBR"=NA, "Res"=NA)#create output dataframe

for( i in unique(concrete.flux.meteo$day)){
  EB_temp<-energy.closure(data=concrete.flux.meteo[concrete.flux.meteo$day==i,],
                          instantaneous = FALSE, 
                          G=concrete.flux.meteo$shf[concrete.flux.meteo$day==i]*-1, 
                          Rn = concrete.flux.meteo$TotRNet_Avg[concrete.flux.meteo$day==i])
  EB_day_concrete$EBR[EB_day_concrete$day==i]<-EB_temp[5]
  
  #Foken
  #sum of Foken Res for one day
  EB_day_concrete$Res[EB_day_concrete$day==i]<-sum(EB_data_concrete_complete$TotRNet_Avg[EB_data_concrete_complete$day==i])-
    sum(EB_data_concrete_complete$H[EB_data_concrete_complete$day==i])-
    sum(EB_data_concrete_complete$LE[EB_data_concrete_complete$day==i])-
    sum(EB_data_concrete_complete$shf[EB_data_concrete_complete$day==i]*-1)
}

ggplot(data=EB_day_concrete)+
  geom_line(aes(x=day,y=EBR))+
  geom_point(aes(x=day,y=EBR))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  theme_bw()+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("daily_energy_balance_bigleaf_concrete.png"),
       width=297, height=210, units = "mm")
#residual
ggplot(data=EB_day_concrete)+
  geom_line(aes(x=day,y=Res))+
  geom_point(aes(x=day,y=Res))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  theme_bw()+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("daily_energy_balance_residual_concrete.png"),
       width=297, height=210, units = "mm")

#plot both in one graph
ggplot(data=EB_day_concrete)+
  geom_line(aes(x=day,y=Res, col="Residual"), size=2)+
  geom_line(aes(x=day,y=EBR*1000, col="EBR"), size=2)+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20), 
                     sec.axis=sec_axis(trans=~./1000, name="EBR"))+
  geom_hline(aes(yintercept=1000, col="EBR"), linetype=2, size=2)+
  geom_hline(aes(yintercept=0, col="Residual"), linetype=2, size=2)+
  ggtitle("Daily Energy Balance - Concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("daily_energy_balance_residual_EBR_concrete.png"),
       width=297, height=210, units = "mm")

#plot
ggplot(data=EB_step_concrete)+
  geom_line(aes(datetime, EB))+
  theme_bw()+
  geom_hline(aes(yintercept=1), col="red")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  ggtitle("Energy Balance - Concrete")+
  ylab(label="energy balance non-closure")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("halfhour_energy_balance_bigleaf_concrete.png"),
       width=297, height=210, units = "mm")

EB_step_concrete$hour<-hour(EB_step_concrete$datetime)
ggplot(data=EB_step_concrete[EB_step_concrete$EB>=10*-1&EB_step_concrete$EB<=10,])+
  geom_boxplot(aes(x=as.factor(hour), y=EB, group=as.factor(hour)))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  ggtitle(label="Hourly Concrtete EB")+
  geom_hline(aes(yintercept=1, col="red"))+
  ylab("EBR")

#test to remove high-non-closures
EB_step_concrete_nospikes<-EB_step_concrete
which(EB_step_concrete_nospikes$EB<=-50)
EB_step_concrete_nospikes$EB[EB_step_concrete_nospikes$EB<=-50]<-NA
which(EB_step_concrete_nospikes$EB>=50)
EB_step_concrete_nospikes$EB[EB_step_concrete_nospikes$EB>=50]<-NA
#plot again
ggplot(data=EB_step_concrete_nospikes)+
  geom_line(aes(datetime, EB))+
  theme_bw()+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Energy Balance - Concrete")+
  ylab(label="energy balance non-closure")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("energy_balance_bigleaf_nospikes_concrete.png"),
       width=297, height=210, units = "mm")

EB_step_concrete_nospikes$hour<-hour(EB_step_concrete_nospikes$datetime)

ggplot(data=EB_step_concrete_nospikes)+
  geom_boxplot(aes(x=as.factor(hour), y=EB, group=as.factor(hour)))+
  theme_bw()

#for whole time span with ground heat flux
EB_whole_concrete<-energy.closure(data=concrete.flux.meteo,instantaneous = FALSE, 
                                               G=concrete.flux.meteo$shf*-1, 
                                               Rn = concrete.flux.meteo$TotRNet_Avg,
                                               LE=concrete.flux.meteo$LE, 
                                               H=concrete.flux.meteo$H)
EB_whole_concrete   #concrete:  0.934 

#Get percentage of energy gap
(1-EB_whole_concrete[5])*100 #concrete: 6.6

#for whole time span without ground heat flux
EB_noG<-energy.closure(data=concrete.flux.meteo,instantaneous = FALSE, 
                       Rn = concrete.flux.meteo$TotRNet_Avg,
                       LE=concrete.flux.meteo$LE, 
                       H=concrete.flux.meteo$H)
EB_noG  #concrete:  0.521 
#Get percentage of energy gap
(1-EB_noG[5])*100 #concrete: 47.9 

####other method####

#cumulatively sum Rn − G − S and LE +H over specified time periods

sum(EB_data_concrete_complete$LE+EB_data_concrete_complete$H)/
  sum(EB_data_concrete_complete$TotRNet_Avg-EB_data_concrete_complete$shf*-1)

#EBR grass: 0.6002816
#EBR concrete: 0.9336564

EB_day_concrete$EBR_2<-NA
#calculate that for every day
for( i in unique(concrete.flux.meteo$day)){
  EB_dat_temp<- EB_data_concrete_complete[EB_data_concrete_complete$day==i,]
  EB_day_concrete$EBR_2[EB_day_concrete$day==i]<-sum(EB_dat_temp$LE+EB_dat_temp$H)/
    sum(EB_dat_temp$TotRNet_Avg-EB_dat_temp$shf*-1)
}

####Energy balance Foken####
#Res = Rn-H-LE-G
EBR_Foken_concrete<-data.frame("TIMESTAMP"=EB_data_concrete_complete$TIMESTAMP, "EBR"=NA)
EBR_Foken_concrete$EBR<-EB_data_concrete_complete$TotRNet_Avg-
  EB_data_concrete_complete$H-
  EB_data_concrete_complete$LE-
  EB_data_concrete_complete$shf*-1

ggplot(data=EBR_Foken_concrete)+
  geom_line(aes(x=TIMESTAMP, y=EBR))+
  theme_bw()+
  ggtitle("EB Residual")+
  geom_hline(aes(yintercept=0), col="red")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("energy_balance_Foken_concrete.png"),
       width=297, height=210, units = "mm")

mean(EBR_Foken_concrete$EBR) 

#concrete: mean residual of 3.91977 W/m^-2
#grass: mean residual 43.3 W/m^-2

#plot for mean day
EBR_Foken_concrete$hour<-hour(EBR_Foken_concrete$TIMESTAMP)
EBR_Foken_concrete_melted<-melt(EBR_Foken_concrete, id.vars=c("TIMESTAMP", "hour"))

ggplot(data=EBR_Foken_concrete_melted)+
  geom_boxplot(aes(x=hour, y=value, group=hour))+
  theme_bw()+
  ylab(label="EB Residual [W m^-2]")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle("Residual per hour")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("mean_day_energy_balance_Foken_concrete.png"),
       width=297, height=210, units = "mm")

####Energy Balance for Grass####
#for time steps
EB_stepwise_grass<-energy.closure(data=grass.flux.meteo,instantaneous = TRUE, 
                                     G=grass.flux.meteo$shf*-1, 
                                     Rn = grass.flux.meteo$TotRNet_Avg,
                                     LE=grass.flux.meteo$LE, 
                                     H=grass.flux.meteo$H)
EB_step_grass<-data.frame("EB"=EB_stepwise_grass, 
                             "datetime"=grass.flux.meteo$TIMESTAMP)


#get only complete observation
EB_data_grass_complete<-grass.flux.meteo[complete.cases(grass.flux.meteo),]
EB_data_grass_complete$day<-date(EB_data_grass_complete$TIMESTAMP)
#calculate energy balance for every individual day
grass.flux.meteo$day<-date(grass.flux.meteo$TIMESTAMP) #create column with day
EB_day_grass<-data.frame("day"=unique(grass.flux.meteo$day), "EBR"=NA, "Res"=NA)#create output dataframe

for( i in unique(grass.flux.meteo$day)){
  #whole EBR for one day
  EB_temp<-energy.closure(data=grass.flux.meteo[grass.flux.meteo$day==i,],
                          instantaneous = FALSE, 
                          G=grass.flux.meteo$shf[grass.flux.meteo$day==i]*-1, 
                          Rn = grass.flux.meteo$TotRNet_Avg[grass.flux.meteo$day==i])
  EB_day_grass$EBR[EB_day_grass$day==i]<-EB_temp[5]
  #sum of Foken Res for one day
  EB_day_grass$Res[EB_day_grass$day==i]<-sum(EB_data_grass_complete$TotRNet_Avg[EB_data_grass_complete$day==i])-
    sum(EB_data_grass_complete$H[EB_data_grass_complete$day==i])-
    sum(EB_data_grass_complete$LE[EB_data_grass_complete$day==i])-
    sum(EB_data_grass_complete$shf[EB_data_grass_complete$day==i]*-1)
}
#bigleaf daily closure
ggplot(data=EB_day_grass)+
  geom_line(aes(x=day,y=EBR))+
  geom_point(aes(x=day,y=EBR))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("daily_energy_balance_bigleaf_grass.png"),
       width=297, height=210, units = "mm")
#foken daily residual
ggplot(data=EB_day_grass)+
  geom_line(aes(x=day,y=Res))+
  geom_point(aes(x=day,y=Res))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("daily_energy_balance_residual_grass.png"),
       width=297, height=210, units = "mm")
#plot both in one graph
ggplot(data=EB_day_grass)+
  geom_line(aes(x=day,y=Res, col="Residual"), size=2)+
  geom_line(aes(x=day,y=EBR*1000, col="EBR"), size=2)+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20), 
                     sec.axis=sec_axis(trans=~./1000, name="EBR"))+
  geom_hline(aes(yintercept=1000, col="EBR"), linetype=2, size=2)+
  geom_hline(aes(yintercept=0, col="Residual"), linetype=2, size=2)+
  ggtitle("Daily Energy Balance - Grass")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("daily_energy_balance_residual_EBR_grass.png"),
       width=297, height=210, units = "mm")

#plot
ggplot(data=EB_step_grass)+
  geom_line(aes(datetime, EB))+
  theme_bw()+
  ggtitle("Energy Balance - grass")+
  ylab(label="energy balance non-closure")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("halfhour_energy_balance_bigleaf_grass.png"),
       width=297, height=210, units = "mm")

EB_step_grass$hour<-hour(EB_step_grass$datetime)
ggplot(data=EB_step_grass[EB_step_grass$EB>=10*-1&EB_step_grass$EB<=10,])+
  geom_boxplot(aes(x=as.factor(hour), y=EB, group=as.factor(hour)))+
  theme_bw()+
  ggtitle("Hourly Grass EB")+
  geom_hline(aes(yintercept=1, col="red"))+
  ylab("EBR")

#test to remove high-non-closures
EB_step_grass_nospikes<-EB_step_grass
which(EB_step_grass_nospikes$EB<=-50)
EB_step_grass_nospikes$EB[EB_step_grass_nospikes$EB<=-50]<-NA
which(EB_step_grass_nospikes$EB>=50)
EB_step_grass_nospikes$EB[EB_step_grass_nospikes$EB>=50]<-NA
#plot again
ggplot(data=EB_step_grass_nospikes)+
  geom_line(aes(datetime, EB))+
  theme_bw()+
  ggtitle("Energy Balance - grass")+
  ylab(label="energy balance non-closure")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("energy_balance_bigleaf_nospikes_grass.png"),
       width=297, height=210, units = "mm")

#for whole time span with ground heat flux
EB_whole_grass<-energy.closure(data=grass.flux.meteo,instantaneous = FALSE, 
                                  G=grass.flux.meteo$shf*-1, 
                                  Rn = grass.flux.meteo$TotRNet_Avg,
                                  LE=grass.flux.meteo$LE, 
                                  H=grass.flux.meteo$H)
EB_whole_grass   #grass:  0.602 

#Get percentage of energy gap
(1-EB_whole_grass[5])*100 #grass: 39.8

#for whole time span without ground heat flux
EB_noG<-energy.closure(data=grass.flux.meteo,instantaneous = FALSE, 
                       Rn = grass.flux.meteo$TotRNet_Avg,
                       LE=grass.flux.meteo$LE, 
                       H=grass.flux.meteo$H)
EB_noG  #grass:  0.521 
#Get percentage of energy gap
(1-EB_noG[5])*100 #grass: 47.9 

####other method####
#cumulatively sum Rn − G − S and LE +H over specified time periods
#sum(LE+H)/sum(Rn-G)
sum(EB_data_grass_complete$LE+EB_data_grass_complete$H)/
  sum(EB_data_grass_complete$TotRNet_Avg-EB_data_grass_complete$shf*-1)

#EBR grass: 0.6002816
#EBR concrete: 0.9336564
EB_day_grass$EBR_2<-NA
#calculate that for every day
for( i in unique(grass.flux.meteo$day)){
  EB_dat_temp<- EB_data_grass_complete[EB_data_grass_complete$day==i,]
  EB_day_grass$EBR_2[EB_day_grass$day==i]<-sum(EB_dat_temp$LE+EB_dat_temp$H)/
    sum(EB_dat_temp$TotRNet_Avg-EB_dat_temp$shf*-1)
}

####Energy balance Foken####
#Res = Rn-H-LE-G
EBR_Foken_grass<-data.frame("TIMESTAMP"=EB_data_grass_complete$TIMESTAMP, "EBR"=NA)
EBR_Foken_grass$EBR<-EB_data_grass_complete$TotRNet_Avg-
  EB_data_grass_complete$H-
  EB_data_grass_complete$LE-
  EB_data_grass_complete$shf*-1

ggplot(data=EBR_Foken_grass)+
  geom_line(aes(x=TIMESTAMP, y=EBR))+
  theme_bw()+
  ggtitle("EB Residual")+
  geom_hline(aes(yintercept=0), col="red")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("energy_balance_Foken_grass.png"),
       width=297, height=210, units = "mm")

mean(EBR_Foken_grass$EBR) 

#grass: mean residual of 3.91977 W/m^-2
#grass: mean residual 43.3 W/m^-2

#plot for mean day
EBR_Foken_grass$hour<-hour(EBR_Foken_grass$TIMESTAMP)
EBR_Foken_grass_melted<-melt(EBR_Foken_grass, id.vars=c("TIMESTAMP", "hour"))

ggplot(data=EBR_Foken_grass_melted)+
  geom_boxplot(aes(x=hour, y=value, group=hour))+
  theme_bw()+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle("Residual per hour")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("mean_day_energy_balance_Foken_grass.png"),
       width=297, height=210, units = "mm")

#add column with only date
meteo_beton$day<-date(meteo_beton$TIMESTAMP)
meteo_kiebitz$day<-date(meteo_kiebitz$TIMESTAMP)
#add precipitation
meteo_beton$rain<-dat.beton.flux.meteo$Rain_mm_Tot
meteo_kiebitz$rain<-dat.kiebitz.flux.meteo$Rain_mm_Tot

#####plot fluxes for every single day####
EB_data_complete<-EB_data_concrete_complete
EB_step<-EB_step_concrete
EB_day<-EB_day_concrete
meteo<-meteo_beton

EB_data_complete<-EB_data_grass_complete
EB_step<-EB_step_grass
EB_day<-EB_day_grass
meteo<-meteo_kiebitz

plot_EBR_fluxes<-function(day=1){
  i=unique(EB_data_complete$day)[day]
  EB_data_temp<-EB_data_complete[EB_data_complete$day==i,]
  #plot fluxes
  flux_plot<-ggplot(data=EB_data_temp)+
    geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg, col="Rn"))+
    geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
    geom_line(aes(x=TIMESTAMP, y=shf*-1, col="G"))+
    geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
    geom_hline(aes(yintercept=0), col="black")+
    ylab(label="Flux [W/m^-2]")+
    theme_bw()
  #calculate closure
  EBR_Foken<-data.frame("TIMESTAMP"=EB_data_temp$TIMESTAMP, "EBR"=NA)
  EBR_Foken$EBR<-EB_data_temp$TotRNet_Avg-EB_data_temp$H-EB_data_temp$LE-EB_data_temp$shf
  #sum(EBR_Foken$EBR)/sum(EB_data_temp$Rn,EB_data_temp$H,EB_data_temp$LE,EB_data_temp$shf*-1)
  #plot closure
  energy_plot<-ggplot(data=EBR_Foken)+
    geom_line(aes(x=TIMESTAMP, y=EBR))+
    geom_point(aes(x=TIMESTAMP, y=EBR))+
    theme_bw()+
    ylab(label="Residual [W/m^2]")+
    geom_hline(aes(yintercept=0), col="red")+
    annotate(geom="text", x=max(EBR_Foken$TIMESTAMP), y=100, 
             label=paste("Day Res:  \n",  round(EB_day$Res[EB_day$day==i], 2), "[W/m^-2]"))+
    annotate(geom="text", x=min(EBR_Foken$TIMESTAMP), y=100, label="Res = Rn - H - LE - G")+
    ggtitle("EBR Foken")
  
  #bigleaf EBR plot
  EB_step_temp<-EB_step
  EB_step_temp$day<-date(EB_step_temp$datetime)
  EB_step_temp<-EB_step_temp[EB_step_temp$day==i,]
  #plot
  bigleaf_plot <- ggplot(data=EB_step_temp)+
    geom_line(aes(x=datetime, y=EB))+
    geom_point(aes(x=datetime, y=EB))+
    theme_bw()+
    ylab(label="EBR")+
    geom_hline(aes(yintercept=1), col="red")+
    annotate(geom="text", x=max(EB_step_temp$datetime), y=3, 
             label=paste("Day EBR:  \n", round(EB_day$EBR[EB_day$day==i], 2)))+
    annotate(geom="text", x=min(EB_step_temp$datetime), y=3, label="EBR=sum(LE+H)/sum(Rn-G)")

  #meteo plots
  meteo_plot<-ggplot(data=meteo[meteo$day==i,])+
    #geom_line(aes(x=TIMESTAMP, y=AirTC_Avg/2, color="AirTemp"))+
    geom_line(aes(x=TIMESTAMP, y=SUp_Avg))+
    ylab(label="SWUp [W m^-2]")+
    scale_y_continuous(sec.axis=sec_axis(trans=~./100, name="Precipitation [mm]"))+
    geom_bar(aes(x=TIMESTAMP, y=rain*100), stat="identity", fill="blue")+
      theme_bw()+
    scale_fill_discrete("blue")
    
  #print plots
  flux_plot + energy_plot + bigleaf_plot + meteo_plot + plot_layout(nrow=4)
  
}

plot_EBR_fluxes(day=3)
#grass: one major EBR outlier at 6am, little rain and cloudy during afternoon
#concrete:many outlier during the day
plot_EBR_fluxes(day=4)
#grass: one major EBR outlier at 6am, little rain during afternoon
#concrete: one major EBR outlier during the day, otherwise good closure
plot_EBR_fluxes(day=5)
#grass: one major EBR outlier at 7am,no rain, partly cloudy
#concrete: bad overall closure
plot_EBR_fluxes(day=6)
#grass: no major EBR outlier, minor round 6 am, no rain, overcast
#concrete: bad overall closure
plot_EBR_fluxes(day=7)
#grass: minor EBR outlier before 6 am, no rain
#concrete:good overall clousure, minor outlier in the afternoon
plot_EBR_fluxes(day=8)
#grass: minor EBR outlier after 6 am, no rain
#concrete: good overall closure
plot_EBR_fluxes(day=9)
#grass: little rain morning and evening, no outlier for EBR
#concrete:bad overall closure
plot_EBR_fluxes(day=10)
#grass: some rain during the day, mostly overcast, 
#concrete: one major outlier at 6 am, bad overall closure
plot_EBR_fluxes(day=11)
#grass: a lot of rain in the evening, mostly overcast, major EBR outlier at 6 am
#concrete: one major outlier at 6 am otherwise good overall closure
plot_EBR_fluxes(day=11)
#grass: a lot of rain in the evening, mostly overcast, major EBR outlier at 6 am, okay overall closure
#concrete: outlier at 6 am, otherwise good closure
plot_EBR_fluxes(day=12)
#grass: some rain in the morning, no major outlier
#concrete: outlier at 8 am, otherwise good closure
plot_EBR_fluxes(day=13)
#grass: some rain in the evening, major outlier at 6 am
#concrete: outlier before 6pm after rain event
plot_EBR_fluxes(day=14)
#grass: no rain, no major outliers
#concrete: major outlier at 6 am, why bad overall closure??
plot_EBR_fluxes(day=15)
#grass: no rain, major outlier at 6 am
#concrete: one major outlier at 5pm, one minor at 6 am
plot_EBR_fluxes(day=16)
#grass: no rain, minor outlier at 6 am, overcast
#concrete: no major outlier, why overall good closure?
plot_EBR_fluxes(day=17)
#grass: no rain, major outlier at 6 am, no clouds
#concrete: no major outlier, why overall good closure?
plot_EBR_fluxes(day=18)
#grass: no rain, minor outlier at 6 am, no clouds
#concrete: minor outlier around 6 am
plot_EBR_fluxes(day=19)
#grass: rain during midday, major outlier at and after 6 am, cloudy
#concrete: bad overall closure, many outliers
plot_EBR_fluxes(day=20)
#grass: rain after midday, minor outlier at 6 am, cloudy
#concrete: one major outlier at afternoon after rain event
plot_EBR_fluxes(day=21)
#grass: rain during night, major outlier at 6 am, overcast
#concrete: one major outlier at 6 am, overall good closure
plot_EBR_fluxes(day=22)
#grass: rain during the day, overcast, no outliers but overall bad closure
#concrete: overall bad closure

EB_both<-data.frame("day"=EB_day_concrete$day, 
                    "Concrete_EBR"=EB_day_concrete$EBR, 
                    "Concrete_Res"=EB_day_concrete$Res,
                    "Grass_EBR"=EB_day_grass$EBR,
                    "Grass_Res"=EB_day_grass$Res)

#plot stepwise closure as mean hour of the day

#test energy balance during the day and during the night
