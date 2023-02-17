#Energy Balance Calculation
#install.packages("bigleaf")
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
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Meteorology/heat_fluxes_with_meteorology.r")
####PREP####
#Prep data to do it for both EC towers splitting meteo.agg into kiebitz and beton

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
####Beton####
shf_whole<-read.csv(file = "shf_concrete_20230213.csv")
shf_whole$DATETIME<-as.POSIXct(shf_whole$DATETIME) #concrete
#aggregate to half hour
shf_30min <- aggregate(shf_whole$shf, 
                   list(TIMESTAMP=cut(shf_whole$DATETIME, "30 mins")),
                   mean)

shf_30min$TIMESTAMP<-as.POSIXct(shf_30min$TIMESTAMP)

meteo_beton<-dat.meteo.agg[,1:13]

#assign new column names
colnames(meteo_beton)[2:13]<-substr(colnames(meteo_beton)[2:13],1, nchar(colnames(meteo_beton)[2:13])-6)
#add soil heat flux
meteo_beton<-left_join(meteo_beton, shf_30min, by="TIMESTAMP")

colnames(meteo_beton)[14]<-"shf"

####Kiebitz####
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
shf_g<-read.csv(file="shf_grass_20230213.csv")
shf_g$DATETIME<-as.POSIXct(shf_g$DATETIME) #grass
#aggregate to half hour
shf_30min <- aggregate(shf_g$shf_higher, 
                       list(TIMESTAMP=cut(shf_g$DATETIME, "30 mins")),
                       mean)

shf_30min$TIMESTAMP<-as.POSIXct(shf_30min$TIMESTAMP)
meteo_kiebitz<-dat.meteo.agg[,c(1,15:17,21:29)]
#assign new column names
colnames(meteo_kiebitz)[2:13]<-substr(colnames(meteo_kiebitz)[2:13],1, nchar(colnames(meteo_kiebitz)[2:13])-8)
#add soil heat flux
meteo_kiebitz<-left_join(meteo_kiebitz, shf_30min)

colnames(meteo_kiebitz)[14]<-"shf"

#####Station####
#If Kiebitz:
dat.flux.meteo<-cbind(dat.kiebitz.flux.meteo,meteo_kiebitz)
station="EC04 Kiebitz"

#If Beton:
dat.flux.meteo<-cbind(dat.beton.flux.meteo, meteo_beton)
station="EC02 Beton"

#####EBR####
#QAQC
##exclude fluxes with qc values >6
dat.flux.meteo$co2_flux[dat.flux.meteo$qc_co2_flux>6]<-NA
dat.flux.meteo$h2o_flux[dat.flux.meteo$qc_h2o_flux>6]<-NA
dat.flux.meteo$LE[dat.flux.meteo$qc_LE>6]<-NA
dat.flux.meteo$Tau[dat.flux.meteo$qc_Tau>6]<-NA
dat.flux.meteo$H[dat.flux.meteo$qc_H>6]<-NA
#exclude unreasonable radiation values
#latent heat
plot(dat.flux.meteo$TIMESTAMP, dat.flux.meteo$LE, type="b") #plot

#remove spikes
#spikes<-which(abs(diff(dat.flux.meteo$LE))>30)
#dat.flux.meteo$LE[c(spikes, spikes+1)]<-NA
#plot(dat.flux.meteo$TIMESTAMP, dat.flux.meteo$LE, type="b") #plot again
#spikes_lag<-which(abs(diff(dat.flux.meteo$LE, lag=2))>40)
#dat.flux.meteo$LE[c(spikes_lag, spikes_lag+1)]<-NA
#plot(dat.flux.meteo$TIMESTAMP, dat.flux.meteo$LE, type="b") #plot again

#sensible heat
plot(dat.flux.meteo$TIMESTAMP, dat.flux.meteo$H, type="b")
#remove values below zero
plot(dat.flux.meteo$TIMESTAMP, dat.flux.meteo$H, type="b")
#remove spikes
#spikes<-which(diff(dat.flux.meteo$H)>50)
#dat.flux.meteo$H[c(spikes, spikes+1)]<-NA
#plot(dat.flux.meteo$TIMESTAMP, dat.flux.meteo$H, type="b")

#soil heat flux
plot(dat.flux.meteo$TIMESTAMP, dat.flux.meteo$shf, type="b")
#spikes<-which(abs(diff(dat.flux.meteo$shf))>200)
#dat.flux.meteo$shf[c(spikes, spikes+1)]<-NA
#plot(dat.flux.meteo$TIMESTAMP, dat.flux.meteo$shf, type="b")

#get the first row with no NA values for the column needed
index_start=which(!is.na(rowSums(dat.flux.meteo[,c("LE", 
                                                   "H", 
                                                   "TotRNet_Avg", 
                                                   "shf")])))[1]
index_end=length(dat.flux.meteo$TIMESTAMP)
#change timespan to time where all parameters occured
dat.flux.meteo.cut<-dat.flux.meteo[index_start:index_end,]

#Formula
#Res = Rn - G - H - E
  #Rn: net radiation (slow data)
      #incoming - outgoing rad
      #Rn=(SW_up+LW_up)-(SW_dn+LW_dn)
      #Rn=dat.flux.meteo.cut$SUp_Avg_kiebitz+dat.flux.meteo.cut$LUpCo_Avg_kiebitz-
        #dat.flux.meteo.cut$SDn_Avg_kiebitz-dat.flux.meteo.cut$LDnCo_Avg_kiebitz
      Rn=dat.flux.meteo.cut$TotRNet_Avg
      #plot
      plot(dat.flux.meteo.cut$TIMESTAMP,Rn, type="l")
      abline(h = 0, col="red")
  #G: soil heat flux (slow data)
      #calculates from FO towers
      G<-dat.flux.meteo.cut$shf
      #plot
      plot(dat.flux.meteo.cut$TIMESTAMP,G*-1, type="l")
      abline(h = 0, col="red")
      #Φsurface = Φ0.05 m + S (flux=soil_flux + Storage)
      #storage S = (T(t1) - T(t2)).cvolumic·x/(t1 - t2)
            #T(t1) - T(t2) is the temperature difference in the measurement interval
            #x the depth of installation
            #cvolumic = ρsoil,dry·csoil,dry + Q·cvolumic,water
                  #bulk density of the dry soil ρ,
                  #specific heat capacity of dry soil
                  #the water content (on mass basis), Q,
                  #cwater, the volumic heat capacity of water
  #H: sensible heat flux (flux data)
      H<-dat.flux.meteo.cut$H
      #plot
      plot(dat.flux.meteo.cut$TIMESTAMP,H, type="l")
      abline(h = 0, col="red")
  #LE: latent heat flux (flux data)
      LE<-dat.flux.meteo.cut$LE
      #plot
      plot(dat.flux.meteo.cut$TIMESTAMP,LE, type="l")
      abline(h = 0, col="red")

#calculate energy balance      
#EB<-Rn+G-H-LE      
#plot(dat.flux.meteo.cut$TIMESTAMP, EB, type="l")
#abline(h = 0, col="red")

#calculate mean energy balance
#mean(EB, na.rm=T)

#With Bigleaf
#prep data frame
EB_data<-data.frame("TIMESTAMP"=dat.flux.meteo.cut$TIMESTAMP,
                    "Rn"=dat.flux.meteo.cut$TotRNet_Avg, "G"=dat.flux.meteo.cut$shf*-1, 
                    "LE"=dat.flux.meteo.cut$LE, "H"=dat.flux.meteo.cut$H)
#EB_data<-EB_data[complete.cases(EB_data),]


#Function calculates energy balance ratio EBR = sum(LE + H)/sum(Rn − G − S)
#ordinary least squares (OLSs) relationship between the half-hourly estimates 
#of the dependent flux variables (LE + H) against the half-hourly estimates
#independently derived available energy (Rn −G− S)
                                                                                                                                                   
#for time steps
EB_stepwise<-energy.closure(data=EB_data,instantaneous = TRUE, G=EB_data$G*-1)
EB_step<-data.frame("EB"=EB_stepwise, "datetime"=EB_data$TIMESTAMP)

#calculate energy balance for every individual day
EB_data$day<-date(EB_data$TIMESTAMP) #create column with day
EB_day<-data.frame("day"=unique(EB_data$day), "EBR"=NA)#create output dataframe

for( i in unique(EB_data$day)){
  EB_temp<-energy.closure(data=EB_data[EB_data$day==i,],instantaneous = FALSE, G=EB_data$G[EB_data$day==i])
  EB_day$EBR[EB_day$day==i]<-EB_temp[5]
}
ggplot(data=EB_day)+
  geom_line(aes(x=day,y=EBR))+
  geom_point(aes(x=day,y=EBR))+
  theme_bw()+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
         filename=paste("daily_energy_balance_bigleaf", station, ".png"),
                        width=297, height=210, units = "mm")

which.max(EB_day$EBR)

EB_day_concrete<-EB_day
EB_day_grass<-EB_day

EB_day_both<-cbind(EB_day_concrete, EB_day$EBR)
colnames(EB_day_both)[2:3]<-c("Concrete", "Grass")

#plot
ggplot(data=EB_step)+
  geom_line(aes(datetime, EB))+
  theme_bw()+
  ggtitle("Energy Balance - Concrete")+
  ylab(label="energy balance non-closure")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("halfhour_energy_balance_bigleaf", station, ".png"),
       width=297, height=210, units = "mm")

#test to remove high-non-closures
EB_step_nospikes<-EB_step
which(EB_step_nospikes$EB<=-50)
EB_step_nospikes$EB[EB_step_nospikes$EB<=-50]<-NA
which(EB_step_nospikes$EB>=50)
EB_step_nospikes$EB[EB_step_nospikes$EB>=50]<-NA
#plot again
ggplot(data=EB_step_nospikes)+
  geom_line(aes(datetime, EB))+
  theme_bw()+
  ggtitle("Energy Balance - Concrete")+
  ylab(label="energy balance non-closure")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("energy_balance_bigleaf_nospikes", station, ".png"),
       width=297, height=210, units = "mm")

#QAQC
summary(EB_stepwise)
#Find time and check data of min
dat.flux.meteo.cut$TIMESTAMP[which.min(EB_stepwise)]
EB_data[which.min(EB_stepwise),]
#find time and check data of max
dat.flux.meteo.cut$TIMESTAMP[which.max(EB_stepwise)]
EB_data[which.max(EB_stepwise),]
#for whole time span with ground heat flux
EB_whole<-energy.closure(data=EB_data, G=EB_data$G*-1, Rn=EB_data$Rn, LE=EB_data$LE, H=EB_data$H,
                         instantaneous = FALSE)
EB_whole   #concrete:  0.934 

#Get percentage of energy gap
(1-EB_whole[5])*100 #concrete: 6.6

#for whole time span without ground heat flux
EB_noG<-energy.closure(data=EB_data,Rn=EB_data$Rn, LE=EB_data$LE, H=EB_data$H,
                         instantaneous = FALSE)
EB_noG  #concrete:  0.521 
#Get percentage of energy gap
(1-EB_noG[5])*100 #concrete: 47.9 

####other method####

#cumulatively sum Rn − G − S and LE +H over specified time periods
EB_data_complete<-EB_data[complete.cases(EB_data),]
sum(EB_data_complete$LE+EB_data_complete$H)/sum(EB_data_complete$Rn-EB_data_complete$G)
#EBR grass: 0.6002816 #new 0.5955271
#EBR concrete: 0.9336564

####Energy balance Foken####
#Res = Rn-H-LE-G
EBR_Foken<-data.frame("TIMESTAMP"=EB_data_complete$TIMESTAMP, "EBR"=NA)
EBR_Foken$EBR<-EB_data_complete$Rn-EB_data_complete$H-EB_data_complete$LE-EB_data_complete$G

ggplot(data=EBR_Foken)+
  geom_line(aes(x=TIMESTAMP, y=EBR))+
  theme_bw()+
  ggtitle("EB Residual")+
  geom_hline(aes(yintercept=0), col="red")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("energy_balance_Foken", station, ".png"),
       width=297, height=210, units = "mm")

mean(EBR_Foken$EBR) 
#concrete: mean residual of 3.91977 W/m^-2
#grass: mean residual 43.3 W/m^-2

#plot for mean day
EBR_Foken$hour<-hour(EBR_Foken$TIMESTAMP)
EBR_Foken_melted<-melt(EBR_Foken, id.vars=c("TIMESTAMP", "hour"))

ggplot(data=EBR_Foken_melted)+
  geom_boxplot(aes(x=hour, y=value, group=hour))+
  theme_bw()+
  geom_hline(aes(yintercept=0), col="red")+
  ggtitle("Residual per hour")

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("mean_day_energy_balance_Foken", station, ".png"),
       width=297, height=210, units = "mm")

#look into conditions at that point
#add EB to dataframe
dat.flux.meteo.cut$EB<-EB_stepwise
dat.flux.meteo.cut$index<-rownames(dat.flux.meteo.cut)
#order by EB values (decreasing)
dat.flux.meteo.cut.ordered<- dat.flux.meteo.cut[order(dat.flux.meteo.cut$EB, decreasing = TRUE), ]  #Order data descending
#write 3 max values in dataframe
EB_max3 <- head(dat.flux.meteo.cut.ordered, n=4)
#keep only necessary columns
EB_max3 <- EB_max3[,c("TIMESTAMP", "LE", 
                      "H", 
                      "TotRNet_Avg", 
                      "shf", "EB")]
#write 3 min values in dataframe
EB_min3 <- tail(dat.flux.meteo.cut.ordered[!is.na(dat.flux.meteo.cut.ordered$EB),], n=4)
#keep only necessary columns
EB_min3 <- EB_min3[,c("TIMESTAMP","LE", 
                      "H", 
                      "TotRNet_Avg", 
                      "shf", "EB")]

#bind together
EB_minmax<-rbind(EB_max3,EB_min3)
####Export to csv####
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Tabellen")
write.csv2(EB_minmax, file = paste(station, "EB_minmax.csv", sep="_"))

#Plot to analyse minima and maxima of EB closure ratio
#create annotation dataframe (containing all the observations over the EB threshold)
highlight<-data.frame("x"=dat.flux.meteo.cut$TIMESTAMP)
highlight$y<-NA
highlight$y[highlight$x%in%EB_minmax$TIMESTAMP]<-500

####Plot####
#setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance")
#plot individual fluxes
ggplot(data=EB_data)+
  geom_line(aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_line(aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_line(aes(x=TIMESTAMP, y=H, color="H"))+
  geom_line(aes(x=TIMESTAMP, y=G*-1, color="G"))+
  ylab(label="Energy [W m^-2]")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Components \nof", station))+
  scale_color_manual(values=c(  "#33a02c", "#1f78b4","#000000","#a6cee3"), 
                     name="Comp.\nof EB")+
  theme_bw()
ggsave(path="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("EB_components", station,".png"), 
       width=30, height=20, units = "cm")
#plot EB
ggplot()+
  geom_line(aes(x=dat.flux.meteo.cut$TIMESTAMP, y=EB_stepwise))+
  ylab(label="EB Ratio")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Ratio of", 
                     station),
          sub="EBR = sum(LE + H)/sum(Rn − G − S)")+
  annotate("text", label=paste("Gap in %:\n",round((1-EB_whole[5])*100,2)), 
x=as.POSIXct(dat.flux.meteo.cut$TIMESTAMP[110]), 
           y=max(EB_stepwise,na.rm=T)-20, size=3)+
  geom_point(data=highlight, aes(x=x, y=y))+
  theme_bw()
ggsave(path="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("EB_ratio", station,".png"), 
       width=30, height=20, units = "cm")


ggplot(data=EB_data)+
  geom_line(aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_line(aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_line(aes(x=TIMESTAMP, y=H, color="H"))+
  geom_line(aes(x=TIMESTAMP, y=G, color="G"))+
  geom_point(data=highlight, aes(x=x, y=y))+
  ylab(label="Energy [W m^-2]")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Components \nof", station))+
  scale_color_manual(values=c(  "#33a02c", "#1f78b4","#000000","#a6cee3"), 
                     name="Comp.\nof EB")+
  theme_bw()
ggsave(filename=paste("EB_ratio_with_highlights", station,".pdf"), 
       width=30, height=20, units = "cm")

#Plot only the values of every non-closure
EB_data$EB<-EB_stepwise #add EB values
EB_data$EB<-round(EB_data$EB, 1)
ggplot(data=EB_data[c(as.integer(rownames(EB_minmax))),])+
  geom_jitter(aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_jitter(aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_jitter(aes(x=TIMESTAMP, y=H, color="H"))+
  geom_jitter(aes(x=TIMESTAMP, y=G, color="G"))+
  geom_text_repel(aes(x=TIMESTAMP, y=300, label=EB), 
                  size=3, angle=60)+
  ylab(label="Energy [W m^-2]")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Components \nof", station))+
  scale_color_manual(values=c(  "#33a02c", "#1f78b4","#000000","#a6cee3"), 
                     name="Comp.\nof EB")+
  theme_bw()
grid.text("Values are \nthe EBR in %", 
          x = unit(.99, "npc"), y = unit(.15, "npc"), 
          just = c("right", "bottom"), gp = gpar(fontsize = 8))
ggsave(path="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("EB_ratio_only_nonclosure", station,".png"), 
       width=30, height=20, units = "cm")
#plot lines with nonclosure
ggplot(data=EB_data[c(as.integer(rownames(EB_minmax))),])+
  geom_jitter(aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_jitter(aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_jitter(aes(x=TIMESTAMP, y=H, color="H"))+
  geom_jitter(aes(x=TIMESTAMP, y=G, color="G"))+
  geom_line(data=EB_data, aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_line(data=EB_data, aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_line(data=EB_data, aes(x=TIMESTAMP, y=H, color="H"))+
  geom_line(data=EB_data, aes(x=TIMESTAMP, y=G, color="G"))+
  geom_text_repel(aes(x=TIMESTAMP, y=600, label=EB), size=3, angle=60)+
  ylab(label="Energy [W m^-2]")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Components \nof", station))+
  scale_color_manual(values=c(  "#33a02c", "#1f78b4","#000000","#a6cee3"), 
                     name="Comp.\nof EB")+
  theme_bw()
grid.text("Values are gap \nof EBR in %", 
          x = unit(.99, "npc"), y = unit(.15, "npc"), 
          just = c("right", "bottom"), gp = gpar(fontsize = 8))
ggsave(path="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("EB_ratio_lines_nonclosure", station,".pdf"), 
       width=30, height=20, units = "cm")

