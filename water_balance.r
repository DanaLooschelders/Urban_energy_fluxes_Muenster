#Calculate the water balance
library(ggplot2)
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/heat_fluxes_with_meteorology.r")

#0 = N - QE - A +- ΔSW
#0 = Niederschlag - latenter Wärmestrom - Abfluss +- Wasserspeicherung

#Calculate evaporated water from LE
#Ratcliffe, Joss. (2021). Re: How to calculate evapotranspiration from latent heat flux?. Retrieved from: https://www.researchgate.net/post/How-to-calculate-evapotranspiration-from-latent-heat-flux/606c413649411b0b257e2d9e/citation/download. 
#energy required to evaporate 1g or 1ml of water) which is 2257 J/g
#watts equal to jouls per second
#1 kg H20 per m-2 = 1 mm

#convert soil water content from m3/m3 to mm
dat.kiebitz.flux.meteo$VWC3_mm<-dat.kiebitz.flux.meteo$mean_3VWC*1000
dat.kiebitz.flux.meteo$VWC2_mm<-dat.kiebitz.flux.meteo$mean_3VWC*1000
plot(dat.kiebitz.flux.meteo$VWC3_mm, type="l")
plot(dat.kiebitz.flux.meteo$VWC2_mm, type="l")
#calculate change in soil water content
dat.kiebitz.flux.meteo$dif_VWC3<-NA
dat.kiebitz.flux.meteo$dif_VWC3[2:length(dat.kiebitz.flux.meteo$TIMESTAMP)]<-diff(dat.kiebitz.flux.meteo$VWC3_mm)

dat.kiebitz.flux.meteo$dif_VWC2<-NA
dat.kiebitz.flux.meteo$dif_VWC2[2:length(dat.kiebitz.flux.meteo$TIMESTAMP)]<-diff(dat.kiebitz.flux.meteo$VWC2_mm)
####for Kiebitz####
#calculate water storage in soil from change in soil water content
plot(dat.kiebitz.flux.meteo$dif_VWC3, type="l")
plot(dat.kiebitz.flux.meteo$mean_3VWC, type="l")
plot(dat.kiebitz.flux.meteo$VWC3_mm, type="l")

plot(dat.kiebitz.flux.meteo$dif_VWC2, type="l")
plot(dat.kiebitz.flux.meteo$mean_2VWC, type="l")
plot(dat.kiebitz.flux.meteo$VWC2_mm, type="l")

#calculate evaporated water from LE flux
evap_w<-dat.kiebitz.flux.meteo$LE*60*30/2257/1000
dat.kiebitz.flux.meteo$ET<-evap_w
#plot evaporated Water
ggplot(data=dat.kiebitz.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=ET))+
  theme_bw()+
  ylab("Evapotranspiration [mm]")
ggsave(filename = "ET_kiebitz.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")
#calculate evaporated water from Eq from paper
#ET(daily)=1/pw sum(LE30min/2.501-0.00236*T)
#calculate with for loop only if all 48 values are available
for (i in unique(dat.kiebitz.flux.meteo$date)){
  if(length(dat.kiebitz.flux.meteo$date[dat.kiebitz.flux.meteo$date==i])==48){
    #calcualte ET
  }else{}
}
  
  #pw (kg m-3) is density of water
  #other term is heat of vaporization as a function of temperature
#calculate water balance
K_WB3<-dat.kiebitz.flux.meteo$Rain_mm_Tot-evap_w-dat.kiebitz.flux.meteo$dif_VWC3
dat.kiebitz.flux.meteo$waterbalance3<-K_WB3

K_WB2<-dat.kiebitz.flux.meteo$Rain_mm_Tot-evap_w-dat.kiebitz.flux.meteo$dif_VWC2
dat.kiebitz.flux.meteo$waterbalance3<-K_WB2

#calculate mean closure
mean(dat.kiebitz.flux.meteo$waterbalance3, na.rm=T) #0.06 mm (with 3 sensors 0.07 mm)
#calculate root mean square average
sqrt(mean(dat.kiebitz.flux.meteo$waterbalance3^2, na.rm=T)) #1 mm (with 3 sensors 0.88 mm)

#plot water balance
ggplot(data=dat.kiebitz.flux.meteo,
       aes(TIMESTAMP,waterbalance3))+
  geom_line()+
  ggtitle(label="Water Balance of Kiebitz EC04")+
  xlab(label = "Time")+
  ylab(label="Water Balance [mm]")+
  theme_bw()
ggsave(filename = "Waterbalance_Kiebitz.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")


#plot water balance components
ggplot(data=dat.kiebitz.flux.meteo)+
  geom_bar(aes(TIMESTAMP,ET, fill="ET [mm]"), 
           stat="identity")+
  geom_bar(aes(TIMESTAMP,Rain_mm_Tot, fill="Rain [mm]"), 
           stat="identity")+
  geom_bar(aes(TIMESTAMP, dif_VWC3, fill="SWC [mm]"), 
           stat="identity")+
  geom_line(aes(x=TIMESTAMP, y=waterbalance, color="Water Balance"), linetype="dotted")+
  theme_bw()+
  scale_fill_manual("Bar Plots", values=c("#7570b3","#d95f02", "#1b9e77"))+
  xlab("Time")+
  scale_color_manual("Line", values=c("black"))+
  ylab("Water [mm]")
ggsave(filename = "Waterbalance_components_Kiebitz.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

check<-dat.kiebitz.flux.meteo[!is.na(dat.kiebitz.flux.meteo$waterbalance)&dat.kiebitz.flux.meteo$waterbalance>1,]
check<-check[,c("ET", "Rain_mm_Tot", "waterbalance")]

plot(dat.kiebitz.flux.meteo$dif_VWC3, type="l")
lines(dat.kiebitz.flux.meteo$dif_VWC2, type="l", col="red")

#####for Beton####
#calculate evaporated water from LE flux
evap_w<-dat.beton.flux.meteo$LE*60*30/2257/1000
dat.beton.flux.meteo$ET<-evap_w
#plot evaporated Water
ggplot(data=dat.beton.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=ET))+
  theme_bw()+
  ylab("Evapotranspiration [mm]")
ggsave(filename = "ET_Beton.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#calculate water balance
B_WB<-dat.beton.flux.meteo$Rain_mm_Tot-evap_w
dat.beton.flux.meteo$waterbalance<-B_WB
#plot water balance
ggplot(data=dat.beton.flux.meteo,
       aes(TIMESTAMP,waterbalance))+
  geom_line()+
  ggtitle(label="Water Balance of Beton EC04")+
  xlab(label = "Time")+
  ylab(label="Water Balance [mm]")+
  theme_bw()
ggsave(filename = "Waterbalance_Beton.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")


#plot water balance components
ggplot(data=dat.beton.flux.meteo)+#[-c(1:290),])+
  geom_bar(aes(TIMESTAMP,ET, fill="ET [mm]"), 
           stat="identity")+
  geom_bar(aes(TIMESTAMP,Rain_mm_Tot, fill="Rain [mm]"), 
           stat="identity")+
  geom_line(aes(x=TIMESTAMP, y=waterbalance, color="Water Balance"), linetype="dotted")+
  theme_bw()+
  scale_fill_manual("Bar Plots", values=c("#7570b3","#d95f02"))+
  xlab("Time")+
  scale_color_manual("Line", values=c("black"))+
  ylab("Water [mm]")
ggsave(filename = "Waterbalance_components_Beton.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

####daily water balance####
WBdata<-data.frame("TIMESTAMP"=dat.kiebitz.flux.meteo$TIMESTAMP,
                   "ET"=dat.kiebitz.flux.meteo$ET, 
                   "rain"=dat.kiebitz.flux.meteo$Rain_mm_Tot,
                   "soilW"=dat.kiebitz.flux.meteo$VWC_mm)

#summarize rain to sum
WBdata_daily<-WBdata %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="day")) %>%
  summarize_all(~sum(., na.rm=T))

#calculate change in soil moisture
WBdata$changeSW<-NA
WBdata$changeSW[2:length(WBdata$soilW)]<-diff(WBdata$soilW)

#calculate daily change in soil moisture
WBdata_daily$changeSW<-NA
WBdata_daily$changeSW[2:length(WBdata_daily$soilW)]<-diff(WBdata_daily$soilW)
#calculate WB
WBdata$WB<-WBdata$ET-WBdata$rain-WBdata$changeSW
#calculate daily WB
WBdata_daily$WB<-WBdata_daily$ET-WBdata_daily$rain-WBdata_daily$changeSW
#change to POSIXct
WBdata_daily$TIMESTAMP<-as.POSIXct(WBdata_daily$TIMESTAMP)

#plot components
ggplot(data=WBdata, aes(x=TIMESTAMP))+
  geom_line(aes(y=ET, color="ET"))+
  geom_line(aes(y=rain, color="rain"))+
  geom_line(aes(y=soilW, color="SW"))+
  theme_bw()

#plot components after rain
ggplot(data=WBdata[1500:length(WBdata$TIMESTAMP),], aes(x=TIMESTAMP))+
  geom_line(aes(y=ET, color="ET"))+
  geom_line(aes(y=rain, color="rain"))+
  geom_line(aes(y=soilW, color="SW"))+
  theme_bw()

#plot WB
ggplot(data=WBdata,aes(x=TIMESTAMP) )+
  geom_line(aes(y=WB))+
  theme_bw()

#plot daily
ggplot(data=WBdata_daily, aes(x=TIMESTAMP))+
  geom_line(aes(y=ET, group=1, color="ET"))+
  geom_line(aes(y=rain, group=1, color="rain"))+
  geom_line(aes(y=soilW, group=1, color="SW"))+
  theme_bw()

#plot daily WB
ggplot(data=WBdata_daily,aes(x=TIMESTAMP) )+
  geom_line(aes(y=WB))+
  theme_bw()

#cross correlation to see when rain effects soil water
WBdata_complete<-WBdata[complete.cases(WBdata),]
ccfWB<-ccf(WBdata_complete$rain, WBdata_complete$soilW)
which.max(ccfWB$lag)
