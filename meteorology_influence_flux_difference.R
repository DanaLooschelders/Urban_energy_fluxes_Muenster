library(ggplot2)
#compare difference in heat fluxes with meteorologial conditions


flux_meteo<-data.frame("mean_airT"=rowMeans(dat.beton.flux.meteo[c("AirTC_Avg_beton", #mean of air temperature between kiebitz and beton
                                                                   "AirTC_Avg_kiebitz")], 
                                            na.rm=T),
                       "mean_rH"=rowMeans(dat.beton.flux.meteo[c("RH_Avg_beton", #mean of air temperature between kiebitz and beton
                                                                 "RH_Avg_kiebitz")], 
                                          na.rm=T),
                       "airP"=dat.beton.flux.meteo$AirP_Avg,#air pressure
                       "Hflux_diff"=dat.beton.flux.meteo$H-dat.kiebitz.flux.meteo$H, #H flux difference
                       "LEflux_diff"=dat.beton.flux.meteo$LE-dat.kiebitz.flux.meteo$LE) #LE flux difference

#set wd to graphics 
setwd("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Meteorology")

#Air temperature with h flux
ggplot(data=flux_meteo,aes(x=mean_airT, y=Hflux_diff))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  xlab("Mean air temperature [°C]")+
  ylab("Difference in h flux [W m^2]")
ggsave(filename="airT_hflux.pdf",
       device="pdf",width=297, height=210, units = "mm")

#relative humidity with h flux
ggplot(data=flux_meteo,aes(x=mean_rH, y=Hflux_diff))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  xlab("Mean relative humidity [%]")+
  ylab("Difference in h flux [W m^2]")
ggsave(filename="rH_hflux.pdf",
       device="pdf",width=297, height=210, units = "mm")

#air pressure with h flux
ggplot(data=flux_meteo,aes(x=airP, y=Hflux_diff))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  xlab("Air pressure [hPa]")+
  ylab("Difference in h flux [W m^2]")
ggsave(filename="airP_hflux.pdf",
       device="pdf",width=297, height=210, units = "mm")

#incoming radiation with le flux
#net radiation with le flux


#air temperature with le flux
ggplot(data=flux_meteo,aes(x=mean_airT, y=LEflux_diff))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  xlab("Mean air temperature [°C]")+
  ylab("Dif in LE flux [W m^2]")
ggsave(filename="airT_leflux.pdf",
       device="pdf",width=297, height=210, units = "mm")

#relative humidity with le flux
ggplot(data=flux_meteo,aes(x=mean_rH, y=LEflux_diff))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  xlab("Mean relative humidity [%]")+
  ylab("Dif in LE flux [W m^2]")
ggsave(filename="rH_leflux.pdf",
       device="pdf",width=297, height=210, units = "mm")

#air pressure with le flux
ggplot(data=flux_meteo,aes(x=airP, y=LEflux_diff))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  xlab("Air pressure [hPa]")+
  ylab("Difference in h flux [W m^2]")
ggsave(filename="airP_leflux.pdf",
       device="pdf",width=297, height=210, units = "mm")

#incoming radiation with le flux
#net radiation with le flux