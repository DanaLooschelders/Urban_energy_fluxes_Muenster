library(lubridate)
library(bigleaf)
library(tidyverse)
####Energy Balance for concrete ####
#for time steps
EB_stepwise_concrete<-energy.closure(data=concrete.flux.meteo,instantaneous = TRUE, 
                                     G=concrete.flux.meteo$shf, 
                                     Rn = concrete.flux.meteo$TotRNet_Avg_2,
                                     LE=concrete.flux.meteo$LE, 
                                     H=concrete.flux.meteo$H)
EB_step_concrete<-data.frame("EB"=1-EB_stepwise_concrete*100, 
                             "datetime"=concrete.flux.meteo$TIMESTAMP)

EB_step_concrete$hour<-hour(EB_step_concrete$datetime)
ggplot(data = EB_step_concrete)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="black", linetype="dashed")+
  geom_line(aes(x=hour, y=EB), stat="summary", fun="median")+
  #ylab(bquote('Residual [W' ~m^-2* ']'))+
  theme_bw()
#take only complete observations
EB_data_concrete_complete<-concrete.flux.meteo[complete.cases(concrete.flux.meteo),]
EB_data_concrete_complete$day<-date(EB_data_concrete_complete$TIMESTAMP)
#####test with albedo of 0.3 ####
EB_data_concrete_complete$SDn_calc<-EB_data_concrete_complete$SUp_Avg*0.25
EB_data_concrete_complete$TotRNet_calc<-EB_data_concrete_complete$SDn_calc-EB_data_concrete_complete$SUp_Avg+
  EB_data_concrete_complete$LDnCo_Avg-EB_data_concrete_complete$LUpCo_Avg

#calculate EBR for whole time span with ground heat flux
EB_whole_madeup_concrete<-energy.closure(data=EB_data_concrete_complete, instantaneous = FALSE, 
                                  G = EB_data_concrete_complete$shf, 
                                  Rn =  EB_data_concrete_complete$TotRNet_calc*-1,
                                  LE =  EB_data_concrete_complete$LE, 
                                  H =  EB_data_concrete_complete$H)
EB_whole_madeup_concrete   #concrete:  0.42
#calculate energy balance for every individual day
concrete.flux.meteo$day<-date(concrete.flux.meteo$TIMESTAMP) #create column with day
EB_day_concrete<-data.frame("day"=unique(concrete.flux.meteo$day), "EBR"=NA, "Res"=NA)#create output dataframe

for(i in unique(concrete.flux.meteo$day)){
  EB_temp<-energy.closure(data=concrete.flux.meteo[concrete.flux.meteo$day==i,],
                          instantaneous = FALSE, 
                          G=concrete.flux.meteo$shf[concrete.flux.meteo$day==i], 
                          Rn = concrete.flux.meteo$TotRNet_Avg_2[concrete.flux.meteo$day==i]*-1)
  EB_day_concrete$EBR[EB_day_concrete$day==i]<-EB_temp[5]
  
  #Foken
  #sum of Foken Res for one day
  EB_day_concrete$Res[EB_day_concrete$day==i]<-(sum(EB_data_concrete_complete$TotRNet_Avg_2[EB_data_concrete_complete$day==i]*-1)-
                                                  sum(EB_data_concrete_complete$H[EB_data_concrete_complete$day==i])-
                                                  sum(EB_data_concrete_complete$LE[EB_data_concrete_complete$day==i])-
                                                  sum(EB_data_concrete_complete$shf[EB_data_concrete_complete$day==i]))/length(EB_data_concrete_complete$H[EB_data_concrete_complete$day==i])
}
#plot EBR for individual day
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
#plot residual for individual day
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
  geom_line(aes(x=day,y=EBR*100, col="EBR"), size=2)+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20), 
                     sec.axis=sec_axis(trans=~./100, name="EBR"))+
  geom_hline(aes(yintercept=100, col="EBR"), linetype=2, size=2)+
  geom_hline(aes(yintercept=0, col="Residual"), linetype=2, size=2)+
  ggtitle("Daily Energy Balance - Concrete")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("daily_energy_balance_residual_EBR_concrete.png"),
       width=297, height=210, units = "mm")

#plot stepwise closure over day
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

#plot horly closure as boxplot over day
EB_step_concrete$hour<-hour(EB_step_concrete$datetime)
ggplot(data=EB_step_concrete[EB_step_concrete$EB>=10*-1&EB_step_concrete$EB<=10,])+
  geom_boxplot(aes(x=as.factor(hour), y=EB, group=as.factor(hour)))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  ggtitle(label="Hourly Concrtete EB")+
  geom_hline(aes(yintercept=1, col="red"))+
  ylab("EBR")

#calculate EBR for whole time span with ground heat flux
EB_whole_concrete<-energy.closure(data=concrete.flux.meteo,instantaneous = FALSE, 
                                  G = concrete.flux.meteo$shf, 
                                  Rn = concrete.flux.meteo$TotRNet_Avg_*-1,
                                  LE = concrete.flux.meteo$LE, 
                                  H = concrete.flux.meteo$H)
EB_whole_concrete   #concrete:  0.352 



#Get percentage of energy gap
(1-EB_whole_concrete[5])*100 #concrete: 6.6

#for whole time span without ground heat flux
EB_noG<-energy.closure(data=concrete.flux.meteo,instantaneous = FALSE, 
                       Rn = concrete.flux.meteo$TotRNet_Avg_2*-1,
                       LE=concrete.flux.meteo$LE, 
                       H=concrete.flux.meteo$H)
EB_noG  #concrete:  0.515 
#Get percentage of energy gap
(1-EB_noG[5])*100 #concrete: 47.9 

####other method####

#cumulatively sum Rn − G − S and LE +H over specified time periods
#big leaf method by hand
sum(EB_data_concrete_complete$LE+EB_data_concrete_complete$H)/
  sum((EB_data_concrete_complete$TotRNet_Avg_2*-1)-EB_data_concrete_complete$shf)
#big leaf method by hand without soil heat flux
sum(EB_data_concrete_complete$LE+EB_data_concrete_complete$H)/
  sum((EB_data_concrete_complete$TotRNet_Avg_2*-1))
#residual for whole time period
sum(EB_data_concrete_complete$TotRNet_Avg_2*-1)-
  sum(EB_data_concrete_complete$LE)-
  sum(EB_data_concrete_complete$H)-
  sum(EB_data_concrete_complete$shf)

EB_day_concrete$EBR_2<-NA
#calculate that for every day
for( i in unique(concrete.flux.meteo$day)){
  EB_dat_temp<- EB_data_concrete_complete[EB_data_concrete_complete$day==i,]
  EB_day_concrete$EBR_2[EB_day_concrete$day==i]<-sum(EB_dat_temp$LE+EB_dat_temp$H)/
    sum((EB_dat_temp$TotRNet_Avg_2*-1)-EB_dat_temp$shf)
}

####Energy balance Foken####
#Res = -Rn-H-LE-G
EBR_Foken_concrete<-data.frame("TIMESTAMP"=EB_data_concrete_complete$TIMESTAMP, 
                               "EBR"=NA)
EBR_Foken_concrete$EBR<-energy.closure(data=EB_data_concrete_complete,
                                       instantaneous = FALSE, 
                                       G = EB_data_concrete_complete$shf, 
                                       Rn = EB_data_concrete_complete$TotRNet_Avg_2*-1,
                                       LE = EB_data_concrete_complete$LE, 
                                       H = EB_data_concrete_complete$H)

EBR_Foken_concrete$Res<-EB_data_concrete_complete$TotRNet_Avg_2*-1-
  EB_data_concrete_complete$H-
  EB_data_concrete_complete$LE-
  EB_data_concrete_complete$shf
#plot stepwuse residual
ggplot(data=EBR_Foken_concrete)+
  geom_line(aes(x=TIMESTAMP, y=Res))+
  theme_bw()+
  ggtitle("EB Residual")+
  geom_hline(aes(yintercept=0), col="red")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("energy_balance_Foken_concrete.png"),
       width=297, height=210, units = "mm")

ggplot(data=EBR_Foken_concrete)+
  geom_line(aes(x=TIMESTAMP, y=EBR))+
  theme_bw()+
  ggtitle("EBR")+
  geom_hline(aes(yintercept=0), col="red")
mean(EBR_Foken_concrete$Res) 
#concrete: mean residual of 101.2994 W/m^-2 

#plot median day
EBR_Foken_concrete$hour<-hour(EBR_Foken_concrete$TIMESTAMP)
ggplot(data = EBR_Foken_concrete)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="black", linetype="dashed")+
  geom_line(aes(x=hour, y=Res), stat="summary", fun="median")+
  ylab(bquote('Residual [W' ~m^-2* ']'))+
  theme_bw()

#plot median day
EBR_Foken_concrete$hour<-hour(EBR_Foken_concrete$TIMESTAMP)
ggplot(data = EBR_Foken_concrete)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="black", linetype="dashed")+
  geom_line(aes(x=hour, y=EBR), stat="summary", fun="mean")+
  ylab(bquote('Residual [W' ~m^-2* ']'))+
  theme_bw()

#plot for mean day as boxplot
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
                                  G=grass.flux.meteo$shf, 
                                  Rn = grass.flux.meteo$TotRNet_Avg_2,
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
                          G=grass.flux.meteo$shf[grass.flux.meteo$day==i], 
                          Rn = grass.flux.meteo$TotRNet_Avg[grass.flux.meteo$day==i]*-1)
  EB_day_grass$EBR[EB_day_grass$day==i]<-EB_temp[5]
  #sum of Foken Res for one day
  EB_day_grass$Res[EB_day_grass$day==i]<-(sum(EB_data_grass_complete$TotRNet_Avg_2[EB_data_grass_complete$day==i]*-1)-
                                            sum(EB_data_grass_complete$H[EB_data_grass_complete$day==i])-
                                            sum(EB_data_grass_complete$LE[EB_data_grass_complete$day==i])-
                                            sum(EB_data_grass_complete$shf[EB_data_grass_complete$day==i]))/length(EB_data_grass_complete$TotRNet_Avg_2[EB_data_grass_complete$day==i])
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
  geom_line(aes(x=day,y=EBR*100, col="EBR"), size=2)+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20), 
                     sec.axis=sec_axis(trans=~./100, name="EBR"))+
  geom_hline(aes(yintercept=100, col="EBR"), linetype=2, size=2)+
  geom_hline(aes(yintercept=0, col="Residual"), linetype=2, size=2)+
  ggtitle("Daily Energy Balance - Grass")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("daily_energy_balance_residual_EBR_grass.png"),
       width=297, height=210, units = "mm")

#plot stepwise closure
ggplot(data=EB_step_grass)+
  geom_line(aes(datetime, EB))+
  theme_bw()+
  ggtitle("Energy Balance - grass")+
  ylab(label="energy balance non-closure")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("halfhour_energy_balance_bigleaf_grass.png"),
       width=297, height=210, units = "mm")

#plot hourly EBR as boxplot
EB_step_grass$hour<-hour(EB_step_grass$datetime)
ggplot(data=EB_step_grass[EB_step_grass$EB>=10*-1&EB_step_grass$EB<=10,])+
  geom_boxplot(aes(x=as.factor(hour), y=EB, group=as.factor(hour)))+
  theme_bw()+
  ggtitle("Hourly Grass EB")+
  geom_hline(aes(yintercept=1, col="red"))+
  ylab("EBR")

#for whole time span with ground heat flux
EB_whole_grass<-energy.closure(data=grass.flux.meteo,instantaneous = FALSE, 
                               G=grass.flux.meteo$shf, 
                               Rn = grass.flux.meteo$TotRNet_Avg_2*-1,
                               LE=grass.flux.meteo$LE, 
                               H=grass.flux.meteo$H)
EB_whole_grass   #grass:  0.546 

#Get percentage of energy gap
(1-EB_whole_grass[5])*100 #grass: 45.4

#for whole time span without ground heat flux
EB_noG<-energy.closure(data=grass.flux.meteo,instantaneous = FALSE, 
                       Rn = grass.flux.meteo$TotRNet_Avg_2*-1,
                       LE=grass.flux.meteo$LE, 
                       H=grass.flux.meteo$H)
EB_noG  #grass: 0.543 
#Get percentage of energy gap

(1-EB_noG[5])*100 #grass: 45.7

####other method####
#cumulatively sum Rn − G − S and LE +H over specified time periods
#sum(LE+H)/sum(Rn-G)
sum(EB_data_grass_complete$LE+EB_data_grass_complete$H)/
  sum((EB_data_grass_complete$TotRNet_Avg_2*-1)-EB_data_grass_complete$shf)

#EBR grass:  0.541347

EB_day_grass$EBR_2<-NA
#calculate that for every day
for( i in unique(grass.flux.meteo$day)){
  EB_dat_temp<- EB_data_grass_complete[EB_data_grass_complete$day==i,]
  EB_day_grass$EBR_2[EB_day_grass$day==i]<-sum(EB_dat_temp$LE+EB_dat_temp$H)/
    sum((EB_dat_temp$TotRNet_Avg_2*-1)-EB_dat_temp$shf)
}

####Energy balance Foken####
#Res = Rn-H-LE-G
EBR_Foken_grass<-data.frame("TIMESTAMP"=EB_data_grass_complete$TIMESTAMP, "EBR"=NA)
EBR_Foken_grass$EBR<-EB_data_grass_complete$TotRNet_Avg_2-
  EB_data_grass_complete$H-
  EB_data_grass_complete$LE-
  EB_data_grass_complete$shf

ggplot(data=EBR_Foken_grass)+
  geom_line(aes(x=TIMESTAMP, y=EBR))+
  theme_bw()+
  ggtitle("EB Residual")+
  geom_hline(aes(yintercept=0), col="red")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("energy_balance_Foken_grass.png"),
       width=297, height=210, units = "mm")

mean(EBR_Foken_grass$EBR) 

#grass: mean residual -164.6736 W/m^-2 

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

EB_both<-data.frame("day"=EB_day_concrete$day, 
                    "Concrete_EBR"=EB_day_concrete$EBR, 
                    "Concrete_Res"=EB_day_concrete$Res,
                    "Grass_EBR"=EB_day_grass$EBR,
                    "Grass_Res"=EB_day_grass$Res)
