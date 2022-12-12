library(tidyverse)
library(ggpubr)
library(lubridate)
#Flux=-k*(delta T/delta z)
FO_concrete_surface<-FO_concrete_df[,102:105]
FO_concrete_surface$time<-FO_concrete_temp_time_df_order$time
#average to 30 mins 
FO_concrete_surface_30min = as.data.frame(lapply(FO_concrete_surface[,1:3], 
                                      function(x) aggregate(list(temp=x), 
                                                            list(time=cut(FO_concrete_surface$time, "30 min")), mean)))
time<-FO_concrete_surface_30min$X0.513811518324607.time
FO_concrete_surface_30min <- select(FO_concrete_surface_30min, -contains("time"))
FO_concrete_surface_30min$time<-time
#for 0-5 cm
delta_T_0_5<-FO_concrete_surface_30min$X0.518900523560209.temp - FO_concrete_surface_30min$X0.513811518324607.temp #Temperatur gradient 
delta_z_0_5<-0.518900523560209-0.513811518324607
delta_z_0_5
#for 5-10 cm
delta_T_5_10<-FO_concrete_surface_30min$X0.518900523560209.temp - FO_concrete_surface_30min$X0.513811518324607.temp #Temperatur gradient 
delta_z_5_10<-0.518900523560209-0.513811518324607
delta_z_5_10
#for 0-10 cm
delta_T_0_10<-FO_concrete_surface_30min$X0.523989528795812.temp - FO_concrete_surface_30min$X0.513811518324607.temp #Temperatur gradient 
delta_z_0_10<-0.523989528795812-0.513811518324607
delta_z_0_10

plot(delta_T, type="l")
#delta_z<-0.005 #difference in depth [m]
k=0.173 #bootstrapped mean
#k=0.1757122 #calculated
#k=0.08 #Oke aerated concrete
#k=1.5 #Oke dense concrete
#boot confidence interval: 0.1874 to  0.2040
k_lower<-0.1874
k_upper<-0.2040
Flux_beton_0_5_lower<--k_lower*(delta_T_0_5/delta_z_0_5)
Flux_beton_0_5_upper<--k_upper*(delta_T_0_5/delta_z_0_5)
Flux_beton_5_10_lower<--k_lower*(delta_T_5_10/delta_z_5_10)
Flux_beton_5_10_upper<--k_upper*(delta_T_5_10/delta_z_5_10)
Flux_beton_0_10_lower<--k_lower*(delta_T_0_10/delta_z_0_10)
Flux_beton_0_10_upper<--k_upper*(delta_T_0_10/delta_z_0_10)

#plot(Flux_beton, type="l")
#abline(h=0, col="red")
#mean(abs(Flux_beton))
#range(Flux_beton)
#Foken: Sommertag 50-100 W/m2

#####clauculate heat flux for confidence intervall####

#Flux_beton_lower<--k_lower*(delta_T/delta_z)
#Flux_beton_upper<--k_upper*(delta_T/delta_z)
Flux_CI<-data.frame("time"=as.POSIXct(FO_concrete_surface_30min$time),
                    "lower_0_5"=Flux_beton_0_5_lower,
                    "upper_0_5"=Flux_beton_0_5_upper,
                    "lower_5_10"=Flux_beton_5_10_lower,
                    "upper_5_10"=Flux_beton_5_10_upper,
                    "lower_0_10"=Flux_beton_0_10_lower,
                    "upper_0_10"=Flux_beton_0_10_upper,
                    "hour"=hour(as.POSIXct(FO_concrete_surface_30min$time)))

#set wd to dir of plots
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
#plot ts for 0 to 10
ggplot(data = Flux_CI)+
  geom_line(aes(x=time, y=upper_0_10, col="limits"))+
  geom_line(aes(x=time, y=lower_0_10, col="limits"))+
  geom_ribbon( aes(x=time, ymin=lower_0_10, ymax=upper_0_10, fill="CI"))+
  theme_bw()+
  geom_hline(yintercept=0, col="red")+
  ylab(label=bquote('Soil Heat Flux [W' ~m^-2* ']'))+
  scale_fill_manual("",values = c("grey"))+
  scale_color_manual("",values=c("black", "black"))
ggsave(filename="Soil_heat_flux_concrete_bootstrapped_CI_0_10.png",
       width=297, height=210, units = "mm")
#plot ts for 0 to 5
ggplot(data = Flux_CI)+
  geom_line(aes(x=time, y=upper_0_5, col="limits"))+
  geom_line(aes(x=time, y=lower_0_5, col="limits"))+
  geom_ribbon( aes(x=time, ymin=lower_0_5, ymax=upper_0_5, fill="CI"))+
  theme_bw()+
  geom_hline(yintercept=0, col="red")+
  ylab(label=bquote('Soil Heat Flux [W' ~m^-2* ']'))+
  scale_fill_manual("",values = c("grey"))+
  scale_color_manual("",values=c("black", "black"))
ggsave(filename="Soil_heat_flux_concrete_bootstrapped_CI_0_5.png",
       width=297, height=210, units = "mm")
#duirnal for the first 5cm
ggplot(dat=Flux_CI)+
  stat_summary_bin( aes(x=as.factor(hour), y=lower_0_5, col="lower"), stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=Flux_CI, aes(x=as.factor(hour), y=lower_0_5), fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  stat_summary_bin(aes(x=as.factor(hour), y=upper_0_5, col="upper"), stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(aes(x=as.factor(hour), y=lower_0_5), dat=Flux_CI, fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Soil heat flux", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=1),color="red")+
  xlab("Hour of Day")+
  ylab(bquote('Soil heat flux [W' ~m^-2* ']'))

#duirnal for the first 10cm
ggplot(dat=Flux_CI, 
       aes(x=as.factor(hour), y=mean_0_10))+
  stat_summary_bin(stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=Flux_CI, fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Soil heat flux", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=1),color="red")+
  xlab("Hour of Day")+
  ylab(bquote('Soil heat flux [W' ~m^-2* ']'))
