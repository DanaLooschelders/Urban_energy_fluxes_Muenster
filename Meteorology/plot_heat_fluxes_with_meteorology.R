source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Meteorology/heat_fluxes_with_meteorology.r")

####BETON####
#Rain
#plot latent heat flux with rain
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_bar(aes(y=Rain_mm_Tot*10), stat="identity", color="blue")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Beton Latent Heat Flux and Rain"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x / 10,
                                         name = "Rain [mm]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_rain_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot sensible heat flux with rain
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_bar(aes(y=Rain_mm_Tot*10), stat="identity", color="blue")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Beton Sensible Heat Flux and Rain"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x / 10,
                                         name = "Rain [mm]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "H_rain_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Latent Heat Flux with Wind speed
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=wind_speed*30), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux and Wind Speed"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/30 ,
                                         name = "Wind Speed [m/s]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_ws_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))
#Latent heat flux increases with increasing wind speed
#-> check wind direction again

#plot regression of latent heat flux and wind speed
ggplot(dat=dat.beton.flux.meteo, aes(x=LE, y=wind_speed))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  ylab("Wind Speed")+
  xlab("LE flux [W m^2]")

ggsave(filename = "LE_ws_beton_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Sensible Heat Flux with Wind speed
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=wind_speed*30), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux and Wind Speed"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/30 ,
                                         name = "Wind Speed [m/s]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "H_ws_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot regression of sensible heat flux and wind speed
ggplot(dat=dat.beton.flux.meteo, aes(x=H, y=wind_speed))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  ylab("Wind Speed")+
  xlab("H flux [W m^2]")

ggsave(filename = "H_ws_beton_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Latent Heat Flux with relative humidity
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=RH_Avg_beton), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux and Relative Humidity"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x ,
                                         name = "Relative Humidity [%]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_rH_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Sensible Heat Flux with relative humidity
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=RH_Avg_beton), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux and Relative Humidity"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x ,
                                         name = "Relative Humidity [%]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "H_rH_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Latent Heat Flux with Temperature
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=AirTC_Avg_beton*5), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/5 ,
                                         name = "Air Temperature [°C]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_Temperature_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Sensible Heat Flux with Temperature
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=AirTC_Avg_beton*5), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/5 ,
                                         name = "Air Temperature [°C]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "H_Temperature_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#Shortwave radiation
#BETON
#plot latent heat flux with incoming shortwave radiation
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=SUp_Avg_beton/10), color="red", linetype="dotted")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x * 10,
                                         name = bquote('Shortwave Incoming [W' ~m^-2* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "red"))

ggsave(filename = "LE_SWup_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))


#plot regression of latent heat flux and incoming shortwave radiation 
ggplot(dat=dat.beton.flux.meteo, aes(x=LE, y=SUp_Avg_beton))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  ylab("Incoming SW Rad [W m^2]")+
  xlab("LE flux [W m^2]")

ggsave(filename = "LE_SWUp_beton_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot sensible heat flux with incoming shortwave radiation
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=SUp_Avg_beton/10), color="red", linetype="dotted")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux with Incoming \nShortwave Radiation"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x * 10,
                                         name = bquote('Shortwave Incoming [W' ~m^-2* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "red"))

ggsave(filename = "H_SWup_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))


#plot regression of latent heat flux and incoming shortwave radiation 
ggplot(dat=dat.beton.flux.meteo, aes(x=H, y=SUp_Avg_beton))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  ylab("Incoming SW Rad [W m^2]")+
  xlab("H flux [W m^2]")

ggsave(filename = "H_SWUp_beton_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Latent Heat Flux with reflected shortwave radiation
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=SDn_Avg_beton), color="red", linetype="dotted")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x ,
                                         name = bquote('Shortwave reflected [W' ~m^-2* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "red"))

ggsave(filename = "LE_SWDn_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Sensible Heat Flux with reflected shortwave radiation
ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=SDn_Avg_beton), color="red", linetype="dotted")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux with reflected Shortwave Radiation"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x ,
                                         name = bquote('Shortwave reflected [W' ~m^-2* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "red"))

ggsave(filename = "H_SWDn_beton_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot in two seperate plots
sh.beton<-ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()

swdn.beton<-ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=SDn_Avg_beton))+
  geom_line(color="orange")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
grid.arrange(sh.beton,swdn.beton, nrow=2)

#plot Sensible Heat Flux with incoming shortwave radiation
swup.beton<-ggplot(dat=dat.beton.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg_beton))+
  geom_line(color="orange")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
grid.arrange(sh.beton,swup.beton, nrow=2)

####Kiebitz####
#Rain
#plot latent heat flux with rain
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_bar(aes(y=Rain_mm_Tot*10), stat="identity", color="blue")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux and Rain"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x / 10,
                                         name = "Rain [mm]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_rain_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot sensible heat flux with rain
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_bar(aes(y=Rain_mm_Tot*10), stat="identity", color="blue")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux and Rain"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x / 10,
                                         name = "Rain [mm]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "H_rain_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Latent Heat Flux with Wind speed
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=wind_speed*30), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux and Wind Speed"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/30 ,
                                         name = "Wind Speed [m/s]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_ws_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot regression of sensible heat flux and wind speed
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=LE, y=wind_speed))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  ylab("Wind Speed")+
  xlab("LE flux [W m^2]")

ggsave(filename = "LE_ws_kiebitz_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Sensible Heat Flux with Wind speed
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=wind_speed*30), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux and Wind Speed"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/30 ,
                                         name = "Wind Speed [m/s]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "H_ws_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot regression of sensible heat flux and wind speed
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=H, y=wind_speed))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  ylab("Wind Speed")+
  xlab("H flux [W m^2]")

ggsave(filename = "H_ws_kiebitz_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Latent Heat Flux with relative humidity
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=RH_Avg_beton), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux and Relative Humidity"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x ,
                                         name = "Relative Humidity [%]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_rH_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Sensible Heat Flux with relative humidity
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=RH_Avg_beton), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux and Relative Humidity"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x ,
                                         name = "Relative Humidity [%]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "H_rH_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Latent Heat Flux with Temperature
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=AirTC_Avg_beton*5), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/5 ,
                                         name = "Air Temperature [°C]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_Temperature_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Sensible Heat Flux with Temperature
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=AirTC_Avg_beton*5), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/5 ,
                                         name = "Air Temperature [°C]"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "H_Temperature_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#latent heat with soil moisture
dat.kiebitz.flux.meteo$VWC_Avg<-rowMeans(dat.kiebitz.flux.meteo[,c("WC01_VWC_Avg", "WC02_VWC_Avg","WC01_VWC_Avg")])
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=VWC_Avg*300), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/300 ,
                                         name = "Volumetric Water Content"))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_Temperature_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))
plot(dat.kiebitz.flux.meteo$VWC_Avg~dat.kiebitz.flux.meteo$LE)

#Shortwave radiation
#plot latent heat flux with incoming shortwave radiation
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=SUp_Avg_beton/10), color="red", linetype="dotted")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x * 10,
                                         name = bquote('Shortwave Incoming [W' ~m^-2* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "red"))

ggsave(filename = "LE_SWup_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot regression of latent heat flux and incoming shortwave radiation 
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=LE, y=SUp_Avg_beton))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  ylab("Incoming SW Rad [W m^2]")+
  xlab("LE flux [W m^2]")

ggsave(filename = "LE_SWUp_kiebitz_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot sensible heat flux with incoming shortwave radiation
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=SUp_Avg_beton/10), color="red", linetype="dotted")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux with Incoming \nShortwave Radiation"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x * 10,
                                         name = bquote('Shortwave Incoming [W' ~m^-2* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "red"))

ggsave(filename = "H_SWup_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot regression of latent heat flux and incoming shortwave radiation 
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=H, y=SUp_Avg_beton))+
  geom_point()+
  theme_bw()+
  geom_smooth(method='lm')+
  ylab("Incoming SW Rad [W m^2]")+
  xlab("H flux [W m^2]")

ggsave(filename = "H_SWUp_kiebitz_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Latent Heat Flux with reflected shortwave radiation
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=SDn_Avg_beton), color="red", linetype="dotted")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x ,
                                         name = bquote('Shortwave reflected [W' ~m^-2* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "red"))

ggsave(filename = "LE_SWDn_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot Sensible Heat Flux with reflected shortwave radiation
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_line(aes(y=SDn_Avg_beton), color="red", linetype="dotted")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux with reflected Shortwave Radiation"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x ,
                                         name = bquote('Shortwave reflected [W' ~m^-2* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "red"))

ggsave(filename = "H_SWDn_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

#plot in two seperate plots
sh.kiebitz<-ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=H))+
  geom_line()+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()

swdn.kiebitz<-ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=SDn_Avg_beton))+
  geom_line(color="orange")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
grid.arrange(sh.kiebitz,swdn.kiebitz, nrow=2)

#plot Sensible Heat Flux with incoming shortwave radiation
swup.kiebitz<-ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg_beton))+
  geom_line(color="orange")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
grid.arrange(sh.kiebitz,swup.kiebitz, nrow=2)
