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
delta_T<-FO_grass_surface_30min$X0.00166666666666667.temp - FO_grass_surface_30min$X0.0125.temp

plot(delta_T, type="l")
#for test days:
delta_T_1<-FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_1&FO_grass_surface_30min$time<=endtime_1, 1] - FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_1&FO_grass_surface_30min$time<=endtime_1,3] #Temperatur gradient 
plot(delta_T_1, type="l")
#day 2
delta_T_2<-FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_2&FO_grass_surface_30min$time<=endtime_2, 1] - FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_2&FO_grass_surface_30min$time<=endtime_2,3] #Temperatur gradient 
plot(delta_T_2, type="l")
#day 3
delta_T_3<-FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_3&FO_grass_surface_30min$time<=endtime_3, 1] - FO_grass_surface_30min[FO_grass_surface_30min$time>=starttime_3&FO_grass_surface_30min$time<=endtime_3,3] #Temperatur gradient 
plot(delta_T_3, type="l")

delta_z<- 0.0125-0.00166666666666667#difference in depth [m]
k_1 #bootstrapped for test day 1 -> 0.5359
k_2 #bootstrapped for test day 2 -> 0.50865
k_3 #bootstrapped for test day 3 -> 0.4854

#k calculated -> time series
kc_1
kc_2
kc_3

#plot day 1
Flux_kiebitz_1<-k_1*(delta_T_1/delta_z)
plot(Flux_kiebitz_1, type="l")
abline(h=0, col="red")

#plot day 2
Flux_kiebitz_2<-k_2*(delta_T_2/delta_z)
plot(Flux_kiebitz_2, type="l")
abline(h=0, col="red")

#plot day 3
Flux_kiebitz_3<-k_3*(delta_T_3/delta_z)
plot(Flux_kiebitz_3, type="l")
abline(h=0, col="red")

mean(abs(Flux_kiebitz_1))
