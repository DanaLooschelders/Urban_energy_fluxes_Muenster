library(boot)
#source script to load slow data and QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R") 
#select alpha values between 
alpha_range<-rnorm(n=2000, mean=1.803*10^-7, sd=9.681081*10^-08)
hist(alpha_range)
#select values for specific heat capacity
#cp_soilds*rho_solids*v_solids+cp_water*rho_water*theta
#the density of water is 998 kg/m3
rho_water<- 998 #kg/m3
rho_solids=rnorm(n=2000, mean=1500, sd=100)
#####cp = specific heat
#specific heat of soil solids for sandy loam soil
#from Ochsner, 2001 
cp_solids<- rnorm(n=2000, mean=801, sd=10) #J/(kg K) 
#specific heat of water: 4182 J/kg/K
cp_water<- 4182 # J/kg/K
#soil water content for test days
dat.soil.merge$TIMESTAMP<-as.POSIXct(dat.soil.merge$TIMESTAMP)
dat.soil.merge$mean_VWC<-rowMeans(dat.soil.merge[,c("WC01_VWC_Avg","WC02_VWC_Avg", "WC03_VWC_Avg")], na.rm=T )

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

#for whole time series???
dat_whole<-data.frame("alpha"=alpha_range, "rho_water"=998, 
                  "cp_water"= 4182,  "theta"=VWC_3,
                  "rho_soilds"=rho_solids,
                  "cp_soilds"=cp_solids, 
                  "v_solids"= 1-VWC_3)

#define function 
cond<-function(dat=dat, indices){
  dt<-dat[indices,]
  k<-c(dt[,1]*(dt[,2]*dt[,3]*dt[,4]+dt[,5]*dt[,6]*dt[,7]))
  return(k)
}

#bootstrap
k_1<-boot::boot(data = dat_1, statistic=cond, R=1000)
plot(k_1)
#calculate confidence intervals
boot.ci(boot.out = k_1,  type = "basic", conf = 0.95)
#calculate heat flux for this day


k_2<-boot::boot(data = dat_2, statistic=cond, R=1000)
plot(k_2)
#calculate confidence intervals
boot.ci(boot.out = k_2,  type = "basic", conf = 0.95)

k_3<-boot::boot(data = dat_3, statistic=cond, R=1000)
plot(k_3)
#calculate confidence intervals
boot.ci(boot.out = k_3,  type = "basic", conf = 0.95)


