library(boot)
#source script to load slow data and QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R") 
#select alpha values between 
alpha_range<-rnorm(n=1000, mean=1.803*10^-7, sd=9.681081*10^-08)
hist(alpha_range)
#select values for specific heat capacity
#cp_soilds*rho_solids*v_solids+cp_water*rho_water*theta
#the density of water is 998 kg/m3
rho_water<- 998 #kg/m3
rho_solids=rnorm(n=1000, mean=1500, sd=50)
#####cp = specific heat
#specific heat of soil solids for sandy loam soil
#from Ochsner, 2001 
cp_solids<- rnorm(n=1000, mean=801, sd=10) #J/(kg K) 
#specific heat of water: 4182 J/kg/K
cp_water<- 4182 # J/kg/K
#soil water content for test days
dat.soil.merge$TIMESTAMP<-as.POSIXct(dat.soil.merge$TIMESTAMP)
dat.soil.merge$mean_VWC<-rowMeans(dat.soil.merge[,c("WC01_VWC_Avg","WC02_VWC_Avg", "WC03_VWC_Avg")], na.rm=T )



#for whole time series???
dat_whole<-data.frame("alpha"=alpha_range, "rho_water"=998, 
                  "cp_water"= 4182,  "theta"=mean(dat.soil.merge$mean_VWC, na.rm=T),
                  "rho_soilds"=rho_solids,
                  "cp_soilds"=cp_solids, 
                  "v_solids"= 1-mean(dat.soil.merge$mean_VWC, na.rm=T))

#define function 
cond<-function(dat=dat, indices){
  dt<-dat[indices,]
  k<-c(dt[,1]*(dt[,2]*dt[,3]*dt[,4]+dt[,5]*dt[,6]*dt[,7]))
  return(k)
}

#whole dataframe
k_whole<-boot::boot(data = dat_whole, statistic=cond, R=1000)
plot(k_whole)
#calculate confidence intervals
ci_test<-boot.ci(boot.out = k_whole,  type = "perc", conf = 0.95)
ci_test$percent[4:5] #extract parameter

k_whole<-mean(ci_test$percent[4:5])

#bootstrap one value for every day