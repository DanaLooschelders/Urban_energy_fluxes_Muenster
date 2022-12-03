#Flux=-k*(delta T/delta z)
FO_concrete_surface<-FO_concrete_df[,2:4]
FO_concrete_surface$time<-FO_concrete_temp_time_df_order$time
#average to 30 mins 
FO_concrete_surface_30min = as.data.frame(lapply(FO_concrete_surface[,1:3], 
                                      function(x) aggregate(list(temp=x), 
                                                            list(time=cut(FO_concrete_surface$time, "30 min")), mean)))
time<-FO_concrete_surface_30min$X0.01.time
FO_concrete_surface_30min <- select(FO_concrete_surface_30min, -contains("time"))
FO_concrete_surface_30min$time<-time

delta_T<-FO_concrete_surface_30min$X0.005.temp - FO_concrete_surface_30min$X0.0150890052356021.temp #Temperatur gradient 
plot(delta_T, type="l")
delta_z<-0.0150890052356021-0.005 #difference in depth [m]
k=0.173 #bootstrapped
k=0.1757122 #calculated
k=0.08 #Oke aerated concrete
k=1.5 #Oke dense concrete
Flux_beton<--k*(delta_T/delta_z)

plot(Flux_beton, type="l")
abline(h=0, col="red")
mean(abs(Flux_beton))

#Foken: Sommertag 50-100 W/m2