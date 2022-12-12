source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/bootstrap_kiebitz.R")

#calculate soil heat flux
#Flux=-k*(delta T/delta z)  
FO_grass_surface<-FO_grass_df[,1:3]
FO_grass_surface$time<-FO_grass_temp_time_df_order$time
#average to 30 mins 
FO_grass_surface_30min = as.data.frame(lapply(FO_grass_surface[,1:3], 
                                                 function(x) aggregate(list(temp=x), 
                                                                       list(time=cut(FO_grass_surface$time, "30 min")), mean)))
time<-FO_grass_surface_30min$X0.0125.time
FO_grass_surface_30min <- select(FO_grass_surface_30min, -contains("time"))
FO_grass_surface_30min$time<-as.POSIXct(time)
#calculate temperature difference 
delta_T_0_5 <- FO_grass_surface_30min$X0.00583333333333333.temp - FO_grass_surface_30min$X0.00166666666666667.temp 
delta_T_5_10 <- FO_grass_surface_30min$X0.00583333333333333.temp - FO_grass_surface_30min$X0.00583333333333333.temp
delta_T_0_10 <- FO_grass_surface_30min$X0.0125.temp - FO_grass_surface_30min$X0.00166666666666667.temp 

delta_z_0_5 <- 0.00583333333333333 - 0.00166666666666667 #difference in depth [m]
delta_z_5_10 <- 0.0125 - 0.00166666666666667 #difference in depth [m]
delta_z_5_10 <- 0.0125 - 0.00583333333333333 #difference in depth [m]

Flux_grass_0_5<--k_whole*(delta_T_0_5/delta_z_0_5)
Flux_grass_5_10<--k_whole*(delta_T_5_10/delta_z_5_10)
Flux_grass_0_10<--k_whole*(delta_T_0_10/delta_z_0_10)

#mean(abs(Flux_kiebitz_1))
#####clauculate heat flux for confidence intervall####
#boot confidence interval: 0.0237,  0.5788
k_lower<-0.0237
k_upper<-0.5788
Flux_grass_lower<--k_lower*(delta_T_0_10/delta_z_0_10)
Flux_grass_upper<--k_upper*(delta_T_0_10/delta_z_0_10)
Flux_CI<-data.frame("time"=as.POSIXct(FO_grass_surface_30min$time),
                    "lower_CI"=Flux_grass_lower,
                    "upper_CI"=Flux_grass_upper, 
                    "mean_0_5"=Flux_grass_0_5,
                    "mean_5_10"=Flux_grass_5_10,
                    "mean_0_10"=Flux_grass_0_10,
                    "hour"=hour(as.POSIXct(FO_grass_surface_30min$time)))

#set wd to dir of plots
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
#plot
ggplot(data = Flux_CI)+
  geom_line(aes(x=time, y=upper_CI, col="limits"))+
  geom_line(aes(x=time, y=lower_CI, col="limits"))+
  geom_ribbon( aes(x=time, ymin=lower_CI, ymax=upper_CI, fill="CI"))+
  theme_bw()+
  geom_hline(yintercept=0, col="red")+
  ylab(label=bquote('Soil Heat Flux [W' ~m^-2* ']'))+
  scale_fill_manual("",values = c("grey"))+
  scale_color_manual("",values=c("black", "black"))
ggsave(filename="Soil_heat_flux_grass_bootstrapped_CI.png",
       width=297, height=210, units = "mm")

#duirnal for the first 5cm
ggplot(dat=Flux_CI, 
       aes(x=as.factor(hour), y=mean_0_5))+
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
