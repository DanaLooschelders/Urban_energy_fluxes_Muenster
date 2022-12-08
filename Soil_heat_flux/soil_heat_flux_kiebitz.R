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

#plot(delta_T, type="l")
#for test days:
#delta_T_1<-FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_1&FO_grass_surface_30min$time<=endtime_1, 1] - FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_1&FO_grass_surface_30min$time<=endtime_1,3] #Temperatur gradient 
#plot(delta_T_1, type="l")
#day 2
#delta_T_2<-FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_2&FO_grass_surface_30min$time<=endtime_2, 1] - FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_2&FO_grass_surface_30min$time<=endtime_2,3] #Temperatur gradient 
#plot(delta_T_2, type="l")
#day 3
#delta_T_3<-FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_3&FO_grass_surface_30min$time<=endtime_3, 1] - FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_3&FO_grass_surface_30min$time<=endtime_3,3] #Temperatur gradient 
#plot(delta_T_3, type="l")

delta_z_0_5 <- 0.00583333333333333 - 0.00166666666666667 #difference in depth [m]
delta_z_5_10 <- 0.0125 - 0.00166666666666667 #difference in depth [m]
delta_z_5_10 <- 0.0125 - 0.00583333333333333 #difference in depth [m]

#k_1 #bootstrapped for test day 1 -> 0.5359
#k_2 #bootstrapped for test day 2 -> 0.50865
#k_3 #bootstrapped for test day 3 -> 0.4854

#k calculated -> time series
#kc_1
#kc_2
#kc_3

#plot day 1
#Flux_kiebitz_1<-k_1*(delta_T_1/delta_z)
#plot(Flux_kiebitz_1, type="l")
#abline(h=0, col="red")

#plot day 2
#Flux_kiebitz_2<-k_2*(delta_T_2/delta_z)
#plot(Flux_kiebitz_2, type="l")
#abline(h=0, col="red")

#plot day 3
#Flux_kiebitz_3<-k_3*(delta_T_3/delta_z)
#plot(Flux_kiebitz_3, type="l")
#abline(h=0, col="red")

#mean(abs(Flux_kiebitz_1))
#####clauculate heat flux for confidence intervall####
#boot confidence interval: 0.1585  to  0.1856 
k_lower<-0.1585
k_upper<-0.1856
Flux_beton_lower<--k_lower*(delta_T/delta_z)
Flux_beton_upper<--k_upper*(delta_T/delta_z)
Flux_CI<-data.frame("time"=as.POSIXct(FO_concrete_surface_30min$time),
                    "lower_CI"=Flux_beton_lower,
                    "upper_CI"=Flux_beton_upper, 
                    "mean_0_5"=Flux_beton_0_5,
                    "mean_5_10"=Flux_beton_5_10,
                    "mean_0_10"=Flux_beton_0_10,
                    "hour"=hour(as.POSIXct(FO_concrete_surface_30min$time)))

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
ggsave(filename="Soil_heat_flux_concrete_bootstrapped_CI.png",
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
