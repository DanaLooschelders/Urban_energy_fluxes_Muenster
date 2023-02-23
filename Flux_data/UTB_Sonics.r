library(data.table)
#compare with EC1  and EC5
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/12_UTB_Sonics/CalmCity21_EC1/")
#EC01<-read.table(file="CalmCity21_EC5/CalmCity21_EC5_results_15min_frc_3Drot.csv", 
 #                sep=",", dec=".", skip=2, header=F)

EC01<-fread(file = "CalmCity21_EC1/CalmCity21_EC1_results_15min_frc_3Drot.csv", 
            sep=",", dec=".", fill=TRUE)
colnames(EC01)<-as.character(EC01[1,])
units<-EC01[2,] #save metadata with units
colnames(units)<-colnames(EC01)
EC01<-EC01[-c(1:2),] #remove header and unit row from df

#check data
str(EC01)
#check columns
str(EC01$Datetime_start)
str(EC01$tau)
units$`rho*cp`

#convert datetime
EC01$Datetime_start<-as.POSIXct(EC01$Datetime_start, format = "%d-%m-%Y %H:%M:%OS")
EC01$tau<-as.numeric(EC01$tau)
EC01$wTs<-as.numeric(EC01$wTs)
EC01$beta<-as.numeric(EC01$beta)
EC01$H<-as.numeric(EC01$H)
EC01$Tww<-as.numeric(EC01$Tww)
EC01$`rho*cp`<-as.numeric(EC01$`rho*cp`)

#####second sonic####
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/12_UTB_Sonics/CalmCity21_EC3/")
EC03<-fread(file = "CalmCity21_EC3/CalmCity21_EC3_results_15min_frc_3Drot.csv", 
            sep=",", dec=".", fill=TRUE)
colnames(EC03)<-as.character(EC03[1,])
units<-EC03[2,] #save metadata with units
colnames(units)<-colnames(EC03)
EC03<-EC03[-c(1:2),] #remove header and unit row from df

#convert datetime
EC03$Datetime_start<-as.POSIXct(EC03$Datetime_start, format = "%d-%m-%Y %H:%M:%OS")
EC03$tau<-as.numeric(EC03$tau)
EC03$wTs<-as.numeric(EC03$wTs)
EC03$beta<-as.numeric(EC03$beta)
EC03$H<-as.numeric(EC03$H)
EC03$Tww<-as.numeric(EC03$Tww)
EC03$`rho*cp`<-as.numeric(EC03$`rho*cp`)
####
#Tau EddyPro in kg m-1 s-2
#plot tau [m2 s-2]
ggplot(data=EC01)+
  geom_line(aes(x=Datetime_start, y=tau))+
  theme_bw()+
 ylab("Tau [m2 s-2]")

#plot beta [ms-1]
ggplot(data=EC01)+
  geom_line(aes(x=Datetime_start, y=beta))+
  theme_bw()+
  ylab("beta [ms-1]")        

#plot wTs [J m-3 K-1]
#BBMMFlux: [K m s-1] 
#Covariance w'Ts', kinematic buoyancy flux
ggplot(data=EC01)+
  geom_line(aes(x=Datetime_start, y=wTs))+
  theme_bw()+
  ylab("wTs [K m s-1]")        

#calculate H_turb
ggplot(data=EC01)+
  geom_line(aes(x=Datetime_start, y=`rho*cp`))+
  theme_bw()+
  ylab("rho cp [J m-3 K-1]") 

####first sonic#
EC01$H_turb<-EC01$wTs*EC01$`rho*cp`

ggplot(data=EC01)+
  geom_line(aes(x=Datetime_start, y=H_turb))+
  theme_bw()+
  ylab("H [W m^-2]") 

EC01$H_turb_corr<-EC01$H_turb
EC01$H_turb_corr[EC01$H_turb_corr>500|EC01$H_turb_corr<=-500]<-NA

###second sonic###

EC03$H_turb<-EC03$wTs*EC03$`rho*cp`

ggplot(data=EC03)+
  geom_line(aes(x=Datetime_start, y=H_turb))+
  theme_bw()+
  ylab("H [W m^-2]") 

EC03$H_turb_corr<-EC03$H_turb
EC03$H_turb_corr[EC03$H_turb_corr>500|EC03$H_turb_corr<=-500]<-NA
####plot together
ggplot(data=EC01)+
  geom_line(aes(x=Datetime_start, y=H_turb_corr, col="EC01"))+
  geom_line(data=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H, color="EC02"))+
  #geom_line(data=EC03, aes(x=Datetime_start, y=H_turb_corr, color="EC03"))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ylab("H [W m^-2]")

ggsave(filename = "EC1_EC2_HFlux.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")


EC01$hour<-hour(EC01$Datetime_start)

str(EC01$H_turb_corr)
str(dat.beton.flux.meteo$H)

ggplot()+
  geom_line(data=EC01, aes(x=hour, y=H_turb_corr, col="EC01"), stat="summary", fun="median", group=1)+
  geom_line(data=dat.beton.flux.meteo, aes(x=hour_num, y=H, col="EC02"),stat="summary", fun="median" )+
  theme_bw()

EC01_subset<-EC01[EC01$Datetime_start>=min(dat.beton.flux.meteo$TIMESTAMP)&EC01$Datetime_start<=max(dat.beton.flux.meteo$TIMESTAMP),]

ggplot()+
  geom_line(data=EC01_subset, aes(x=hour, y=H_turb_corr, col="EC01"), 
            stat="summary", fun="median")+
  geom_line(data=dat.beton.flux.meteo, aes(x=hour_num, y=H, col="EC02"),
            stat="summary", fun="median")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_bw()

#Buoyancy flux, energetic
#H
#[W m-2] 
ggplot(data=EC01)+
  geom_line(aes(x=Datetime_start, y=H))+
  theme_bw()+
  ylab("H [W m^-2]")    

EC01$hour<-hour(EC01$Datetime_start)

#plot hourly meadian
ggplot(EC01)+
  geom_line(aes(x=hour, y=H), stat="summary", fun="median")+
  theme_bw()


EC01$H_corr<-EC01$H
EC01$H_corr[EC01$H_corr>500|EC01$H_corr<=-500]<-NA

ggplot(data=EC01)+
  geom_line(aes(x=Datetime_start, y=H_corr))+
  theme_bw()+
  ylab("H [W m^-2]")    

#wTs [K m s-1] Covariance w'Ts', kinematic buoyancy flux
#H [W m-2] Buoyancy flux, energetic
#H_analyz1 [W m-2] Buoyancy flux, energetic, computed for current air density
#wTs_corr_irga1 [K m s-1] Sensible heat flux (Schotanus correction applied), kinematic
# H_corr_irga1 [W m-2] Sensible heat flux (Schotanus correction applied), energetic
#Tww [K m2 s-2] Triple-order moment: vertical transport of kinematic vertical heat flux