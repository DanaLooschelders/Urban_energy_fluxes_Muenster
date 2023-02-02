
#source script to load slow data and QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R") 

#select values for specific heat capacity
#cp_soilds*rho_solids*v_solids+cp_water*rho_water*theta
#the density of water is 998 kg/m3
rho_water<- 998 #kg/m3
#density of soil soilds
#2.58 Mg m3
#2.58 *1000 -> 2580 kg m3
rho_solids=rnorm(n=1000, mean=2580, sd=5)
hist(rho_solids)
#####cp = specific heat
#specific heat of soil solids for sandy loam soil
#from Ochsner, 2001 
cp_solids<- rnorm(n=1000, mean=801, sd=5) #J/(kg K) 
hist(cp_solids)
#specific heat of water: 4182 J/kg/K
cp_water<- 4182 # J/kg/K
#soil water content for test days
dat.soil.merge$TIMESTAMP<-as.POSIXct(dat.soil.merge$TIMESTAMP)
dat.soil.merge$mean_VWC<-rowMeans(dat.soil.merge[,c("WC01_VWC_Avg","WC02_VWC_Avg", "WC03_VWC_Avg")], na.rm=T )
#calculate soil water content for each day
dat.soil.merge$day<-date(dat.soil.merge$TIMESTAMP)
#write in soil.dat.merge
dat.soil.merge<- dat.soil.merge %>%
  group_by(day) %>%
  mutate(daily_VWC = mean(mean_VWC))
#write in new dataframe (short)
daily_VWC<-dat.soil.merge %>%
  group_by(day) %>%
  summarise_at(vars(mean_VWC), list(VWC = mean))
hist(daily_VWC$VWC)
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

####Bootstrap daily k####

#create output dataframe
daily_VWC$lower_k<-NA
daily_VWC$upper_k<-NA
#bootstrap one value for every day
for (i in 1:dim(daily_VWC)[1]){
  if(!is.nan(daily_VWC$VWC[i])){
    dat_temp<-data.frame("alpha"=alpha_range, "rho_water"=998, 
                         "cp_water"= 4182,  "theta"=daily_VWC$VWC[i],
                         "rho_soilds"=rho_solids,
                         "cp_soilds"=cp_solids, 
                         "v_solids"= 1-daily_VWC$VWC[i])
    boot_temp<-boot::boot(data = dat_temp, statistic=cond, R=1000) #bootstrap
    ci_temp<-boot.ci(boot.out = boot_temp,  type = "perc", conf = 0.95) #calculate CI
    ci_k<-ci_temp$percent[4:5] #extract parameter
    daily_VWC$lower_k[i]<-ci_k[1] #write lower end of ci for k
    daily_VWC$upper_k[i]<-ci_k[2] #write upper end of ci for k
  }else{}
}
