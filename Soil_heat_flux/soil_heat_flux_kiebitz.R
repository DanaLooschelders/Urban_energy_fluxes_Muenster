source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/bootstrap_kiebitz.R")
library(tidyverse)
library(lubridate)
library(ggpubr)
#calculate soil heat flux
#Flux=-k*(delta T/delta z)  
FO_grass_surface<-FO_grass_df[,64:93]
FO_grass_surface$time<-FO_grass_temp_time_df_order$time
#average to 30 mins 
FO_grass_surface_30min = as.data.frame(lapply(FO_grass_surface[,1:30], 
                                                 function(x) aggregate(list(temp=x), 
                                                                       list(time=cut(FO_grass_surface$time, "30 min")), mean)))
time<-FO_grass_surface_30min$X0.456971943272261.time
FO_grass_surface_30min <- select(FO_grass_surface_30min, -contains("time"))
FO_grass_surface_30min$time<-as.POSIXct(time)
#add k values to dataframe
FO_grass_surface_30min$day<-date(FO_grass_surface_30min$time)
FO_grass_30_min_k<-left_join(x = FO_grass_surface_30min, y = daily_VWC)

#calculate temperature difference 
delta_T_0_5  <- FO_grass_surface_30min$X0.467132217830244.temp - FO_grass_surface_30min$X0.416330845040329.temp
plot(delta_T_0_5, type="l")
delta_T_5_10 <- FO_grass_surface_30min$X0.411250707761337.temp - FO_grass_surface_30min$X0.360449334971423.temp
plot(delta_T_5_10, type="l")
delta_T_0_10 <- FO_grass_surface_30min$X0.467132217830244.temp - FO_grass_surface_30min$X0.360449334971423.temp
plot(delta_T_0_10, type="l")
delta_T_5_15 <- FO_grass_surface_30min$X0.426491119598312.temp - FO_grass_surface_30min$X0.324888374018481.temp
plot(delta_T_5_15, type="l")

#difference in depth [m]
delta_z_0_5  <- 0.467132217830244 - 0.416330845040329 #for 0 to 5 cm below ground
delta_z_0_5
delta_z_5_10 <- 0.411250707761337 - 0.360449334971423  #for 5 to 10 cm below ground
delta_z_5_10
delta_z_0_10 <- 0.467132217830244 - 0.360449334971423  ##for 0 to 10 cm below ground
delta_z_0_10
delta_z_5_15 <- 0.426491119598312 - 0.324888374018481   #for 5 to 15 cm below ground
delta_z_5_15
#boot confidence interval: 0.0356 to  0.1000

Flux_grass_0_5_lower<--FO_grass_30_min_k$lower_k*(delta_T_0_5/delta_z_0_5)
Flux_grass_0_5_upper<--FO_grass_30_min_k$upper_k*(delta_T_0_5/delta_z_0_5)
Flux_grass_5_10_lower<--FO_grass_30_min_k$lower_k*(delta_T_5_10/delta_z_5_10)
Flux_grass_5_10_upper<--FO_grass_30_min_k$upper_k*(delta_T_5_10/delta_z_5_10)
Flux_grass_0_10_lower<--FO_grass_30_min_k$lower_k*(delta_T_0_10/delta_z_0_10)
Flux_grass_0_10_upper<--FO_grass_30_min_k$upper_k*(delta_T_0_10/delta_z_0_10)
Flux_grass_5_15_lower<--FO_grass_30_min_k$lower_k*(delta_T_5_15/delta_z_5_15)
Flux_grass_5_15_upper<--FO_grass_30_min_k$upper_k*(delta_T_5_15/delta_z_5_15)
#mean(abs(Flux_kiebitz_1))
#####clauculate heat flux for confidence intervall####

Flux_CI<-data.frame("time"=as.POSIXct(FO_grass_30_min_k$time),
                    "lower_0_5"=Flux_grass_0_5_lower,
                    "upper_0_5"=Flux_grass_0_5_upper,
                    "lower_5_10"=Flux_grass_5_10_lower,
                    "upper_5_10"=Flux_grass_5_10_upper,
                    "lower_0_10"=Flux_grass_0_10_lower,
                    "upper_0_10"=Flux_grass_0_10_upper,
                    "lower_5_15"=Flux_grass_5_15_lower,
                    "upper_5_15"=Flux_grass_5_15_upper,
                    "hour"=hour(as.POSIXct(FO_grass_30_min_k$time)))

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
ggsave(filename="Soil_heat_flux_grass_bootstrapped_CI_0_10.png",
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
ggsave(filename="Soil_heat_flux_grass_bootstrapped_CI_0_5.png",
       width=297, height=210, units = "mm")

##plot ts for 5 to 15
ggplot(data = Flux_CI)+
  geom_line(aes(x=time, y=upper_5_15, col="limits"))+
  geom_line(aes(x=time, y=lower_5_15, col="limits"))+
  geom_ribbon( aes(x=time, ymin=lower_5_15, ymax=upper_5_15, fill="CI"))+
  theme_bw()+
  geom_hline(yintercept=0, col="red")+
  ylab(label=bquote('Soil Heat Flux [W' ~m^-2* ']'))+
  scale_fill_manual("",values = c("grey"))+
  scale_color_manual("",values=c("black", "black"))
ggsave(filename="Soil_heat_flux_grass_bootstrapped_CI_5_15.png",
       width=297, height=210, units = "mm")

#duirnal for the first 5cm
ggplot(dat=Flux_CI)+
  stat_summary_bin( aes(x=as.factor(hour), y=lower_0_5, col="lower"), stroke=2.5,
                    fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=Flux_CI, aes(x=as.factor(hour), y=lower_0_5, col="lower"), fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  stat_summary_bin(aes(x=as.factor(hour), y=upper_0_5, col="upper"), stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(aes(x=as.factor(hour), y=upper_0_5, col="upper"), dat=Flux_CI, fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Soil heat flux", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=1),color="red")+
  xlab("Hour of Day")+
  ylab(bquote('Soil heat flux [W' ~m^-2* ']'))
ggsave(filename="Soil_heat_flux_grass_diurnal_bootstrapped_CI_0_5.png",
       width=297, height=210, units = "mm")

#duirnal for the first 10cm
ggplot(dat=Flux_CI)+
  stat_summary_bin( aes(x=as.factor(hour), y=lower_0_10, col="lower"), stroke=2.5,
                    fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=Flux_CI, aes(x=as.factor(hour), y=lower_0_10, col="lower"), fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  stat_summary_bin(aes(x=as.factor(hour), y=upper_0_10, col="upper"), stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(aes(x=as.factor(hour), y=upper_0_10, col="upper"), dat=Flux_CI, fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Soil heat flux", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=1),color="red")+
  xlab("Hour of Day")+
  ylab(bquote('Soil heat flux [W' ~m^-2* ']'))

ggsave(filename="Soil_heat_flux_grass_diurnal_bootstrapped_CI_0_10.png",
       width=297, height=210, units = "mm")
xdelta
#duirnal for 5 to 15cm
ggplot(dat=Flux_CI)+
  stat_summary_bin( aes(x=as.factor(hour), y=lower_5_15, col="lower"), stroke=2.5,
                    fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=Flux_CI, aes(x=as.factor(hour), y=lower_5_15, col="lower"), fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  stat_summary_bin(aes(x=as.factor(hour), y=upper_5_15, col="upper"), stroke=2.5,
                   fun = "median",geom="point", show.legend = F, na.rm = T)+
  stat_summary(aes(x=as.factor(hour), y=upper_5_15, col="upper"), dat=Flux_CI, fun.data = "median_mad", 
               geom = "errorbar",  alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Soil heat flux", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  geom_hline(aes(yintercept=1),color="red")+
  xlab("Hour of Day")+
  ylab(bquote('Soil heat flux [W' ~m^-2* ']'))
ggsave(filename="Soil_heat_flux_grass_diurnal_bootstrapped_CI_5_15.png",
       width=297, height=210, units = "mm")
