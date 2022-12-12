starttime_1 <- as.POSIXct("2021-07-30 08:00:00 CEST")
endtime_1 <- as.POSIXct("2021-07-31 08:00:00 CEST")
VWC_1<-mean(dat.soil.merge$mean_VWC[dat.soil.merge$TIMESTAMP>=starttime_1&dat.soil.merge$TIMESTAMP<=endtime_1], na.rm=T)

#subset 2: "2021-08-10 13:05:12 CEST"  to "2021-08-11 13:05:36 CEST"
starttime_2 <- as.POSIXct("2021-08-04 00:00:00 CEST")
endtime_2 <- as.POSIXct("2021-08-05 00:00:00 CEST")
VWC_2<-mean(dat.soil.merge$mean_VWC[dat.soil.merge$TIMESTAMP>=starttime_2&dat.soil.merge$TIMESTAMP<=endtime_2], na.rm=T)

#subset 3 "2021-08-10 19:46:48 CEST" "2021-08-11 19:47:12 CEST"
starttime_3<-as.POSIXct("2021-08-11 00:00:00 CEST")
endtime_3<-as.POSIXct("2021-08-12 00:00:00 CEST")
VWC_3<-mean(dat.soil.merge$mean_VWC[dat.soil.merge$TIMESTAMP>=starttime_3&dat.soil.merge$TIMESTAMP<=endtime_3], na.rm=T)

#create input dataframe to bootstrap from
dat_1<-data.frame("alpha"=alpha_range, "rho_water"=998, 
                  "cp_water"= 4182,  "theta"=VWC_1,
                  "rho_soilds"=rho_solids,
                  "cp_soilds"=cp_solids, 
                  "v_solids"= 1-VWC_1)
dat_2<-data.frame("alpha"=alpha_range, "rho_water"=998, 
                  "cp_water"= 4182,  "theta"=VWC_2,
                  "rho_soilds"=rho_solids,
                  "cp_soilds"=cp_solids, 
                  "v_solids"= 1-VWC_2)
dat_3<-data.frame("alpha"=alpha_range, "rho_water"=998, 
                  "cp_water"= 4182,  "theta"=VWC_3,
                  "rho_soilds"=rho_solids,
                  "cp_soilds"=cp_solids, 
                  "v_solids"= 1-VWC_3)
str(dat_1)

#bootstrap
#k_1<-boot::boot(data = dat_1, statistic=cond, R=1000)
#plot(k_1)
#calculate confidence intervals
#boot.ci(boot.out = k_1,  type = "basic", conf = 0.95)
#k_1<-mean( c(0.2198,  0.8520))

#k_2<-boot::boot(data = dat_2, statistic=cond, R=1000)
#plot(k_2)
#calculate confidence intervals
#boot.ci(boot.out = k_2,  type = "basic", conf = 0.95)
#k_2<-mean(c(0.2281,  0.7892))

#k_3<-boot::boot(data = dat_3, statistic=cond, R=1000)
#plot(k_3)
#calculate confidence intervals
#boot.ci(boot.out = k_3,  type = "basic", conf = 0.95)
#k_3<-mean(c(0.1989,  0.7719 ))

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