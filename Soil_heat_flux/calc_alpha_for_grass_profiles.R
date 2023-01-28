library(gridExtra)
library(tidyverse)
library(boot)
library(lubridate)
library(dplyr)

#source script to load slow data and QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R") 
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
#source functions
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/functions_alpha.r")
#calculate for individual soil profile
FO_grass_depth<-read.csv("FO_grass.csv")
FO_grass_aG<-read.csv("FO_grass_20cm_aG_10cm.csv")
FO_grass_depth<-FO_grass_aG[,c(1:50, 60)]
#transpose
FO_grass_time<-data.frame(t(FO_grass_depth))
#set time as column name
colnames(FO_grass_time)<-FO_grass_depth$grass_time
#delete row with time
FO_grass_time<-FO_grass_time[-dim(FO_grass_time)[1],]
#add depth to plot
FO_grass_plot<-FO_grass_time
FO_grass_plot$depth<-as.numeric(substr(rownames(FO_grass_plot),start = 2, stop=100))

#soil profile to same same subset as meteo_data
FO_grass_sub<-FO_grass_time[,c(which(colnames(FO_grass_time)=="2021-08-04 07:00:00"):which(colnames(FO_grass_time)=="2021-08-04 09:30:00"))]
#indices: 701:716
#plot all values
for(i in 701:716 ){
  plot(FO_grass_plot$depth, FO_grass_plot[,i],  type="l", 
       main=paste("all values - " ,colnames(FO_grass_plot[i])), ylim=c(17,24))
  abline(v=0.4722124, col="brown")
  abline(v=0.533174, col="green")
  points(FO_grass_plot$depth, FO_grass_plot[,i])
  Sys.sleep(3)
}

#take every fifth point (variying starting point i)
for(i in 1:4){
  FO_grass_temp <- FO_grass_time[seq(i, nrow(FO_grass_time), 4), ] #select every 5th row
  FO_grass_temp[] <- lapply(FO_grass_temp, as.numeric) #coerce to numeric
  FO_grass_temp$depth<-as.numeric(substr(rownames(FO_grass_temp),start = 2, stop=100))
  assign(paste0("FO_grass_", i), FO_grass_temp) #assign name to object
  rm(FO_grass_temp)#remove object
}

#calculate for 1 value
#color_5th_value_grass(range = 707:716, point=2, FO_data_x= FO_grass_time)
plot_5th_value_grass(FO_data_x = FO_grass_4, range=707:716)
color_5th_value_grass(FO_data_x=FO_grass_time, 
                      range=707:716, point=3)

alpha_1_g<-calc_alpha(FO_data_x=FO_grass_1, 707:716) #3.003784e-07
median(unlist(alpha_1_g[[1]]))
boxplot(alpha_1_g[[1]])

#for second
#plot_5th_value(FO_data_x = FO_grass_2,  707:716)
alpha_2_g<-calc_alpha(FO_data_x=FO_grass_2,  707:716)
median(unlist(alpha_2_g[[1]])) #2.166522e-07
boxplot(alpha_2_g[[1]])

#for third
alpha_3_g<-calc_alpha(FO_data_x=FO_grass_3,  707:716)
median(unlist(alpha_3_g[[1]])) #1.863725e-07
boxplot(alpha_3_g[[1]])

#for fourth
alpha_4_g<-calc_alpha(FO_data_x=FO_grass_4,  707:716)
median(unlist(alpha_4_g[[1]])) #1.863725e-07
boxplot(alpha_4_g[[1]])

#plot_temp_alpha(FO_data_x = FO_grass_1, alpha_x = alpha_1)

#specific heat capacity
alpha1_g<-median(unlist(alpha_1_g[[1]])) #6.046484e-07
alpha2_g<-median(unlist(alpha_2_g[[1]])) #4.430371e-07
alpha3_g<-median(unlist(alpha_3_g[[1]])) #6.02086e-07
alpha4_g<-median(unlist(alpha_4_g[[1]])) #4.747325e-07

#bootstrap values
daily_VWC<-bootstrap_k(alpha=alpha1_g)
range(daily_VWC$VWC, na.rm=T)
####save bootstrapped daily k####
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
write.csv(daily_VWC, file="daily_VWC.csv")
daily_VWC<-read.csv("daily_VWC.csv")

#get VWC and k for sample day
daily_VWC[daily_VWC$day=="2021-08-04",]
k_lower_g<-daily_VWC$lower_k[daily_VWC$day=="2021-08-04"]
flux_lower<-shf(FO_data_x = FO_grass_4, range =  701:716, k = k_lower_g)
plot_shf_grass(flux_dat=flux_lower)

#calculate fluxes to merge together
#one
daily_VWC_1<-bootstrap_k(alpha=alpha1_g)
k_1<-daily_VWC_1$lower_k[daily_VWC_1$day=="2021-08-04"]
flux_1<-shf(FO_data_x=FO_grass_1, range =  701:716, k = k_1)
#two
daily_VWC_2<-bootstrap_k(alpha=alpha2_g)
k_2<-daily_VWC_2$lower_k[daily_VWC_2$day=="2021-08-04"]
flux_2<-shf(FO_data_x=FO_grass_2, range =  701:716, k = k_2)
#three
daily_VWC_3<-bootstrap_k(alpha=alpha3_g)
k_3<-daily_VWC_3$lower_k[daily_VWC_3$day=="2021-08-04"]
flux_3<-shf(FO_data_x=FO_grass_3, range =  701:716, k = k_3)
#four
daily_VWC_4<-bootstrap_k(alpha=alpha4_g)
k_4<-daily_VWC_4$lower_k[daily_VWC_4$day=="2021-08-04"]
flux_4<-shf(FO_data_x=FO_grass_4, range =  701:716, k = k_4)
#plot together
dat_1<-flux_1[[2]][[4]]
dat_2<-flux_2[[2]][[4]]
dat_3<-flux_3[[2]][[4]]
dat_4<-flux_4[[2]][[4]]
ggplot()+
  geom_point(data=dat_1, aes( depth, shf*-1, col="point_1"))+
  geom_point(data=dat_2, aes( depth, shf*-1, col="point_2"))+
  geom_point(data=dat_3, aes( depth, shf*-1, col="point_3"))+
  geom_point(data=dat_4, aes( depth, shf*-1, col="point_4"))+
  xlab(label="height [m]")+
  ylab(label="shf [W/m^2]")+
  geom_line(data=dat_1, aes( depth, shf*-1, col="point_1"))+
  geom_line(data=dat_2, aes( depth, shf*-1, col="point_2"))+
  geom_line(data=dat_3, aes( depth, shf*-1, col="point_3"))+
  geom_line(data=dat_4, aes( depth, shf*-1, col="point_4"))+
  geom_vline(xintercept = 0.4722124, col="brown")+
  geom_vline(xintercept = 0.533174, col="green")+
  theme_bw()+
  ggtitle(label=paste("grass tower - soil heat flux ", names(flux_lower[[1]][4])))
#plot as one line
dat_ensemble<-rbind(dat_1, dat_2, dat_3, dat_4)
dat_ensemble<-dat_ensemble[order(dat_ensemble$depth),]
ggplot(data=dat_ensemble, aes( depth, shf*-1))+
  geom_point()+
  xlab(label="height [m]")+
  ylab(label="shf [W/m^2]")+
  geom_line()+
  geom_vline(xintercept = 0.4722124, col="brown")+
  geom_vline(xintercept = 0.533174, col="green")+
  theme_bw()+
  ggtitle(label=paste("grass tower - soil heat flux ", names(flux_lower[[1]][4])))

#calculate for whole time period for two centimeters
#get lower and upper k
daily_VWC_1$lower_k 
daily_VWC$upper_k
#calculate temp diff over depth
dT_dz<-(FO_grass_1[7,1:2914]-FO_grass_1[8,1:2914])/diff(FO_grass_1$depth[7:8])
shf_g<-data.frame("dT"=t(FO_grass_1[7,1:2914]-FO_grass_1[8,1:2914]), 
                "dz"=diff(FO_grass_1$depth[7:8]), 
                "DATETIME"=as.POSIXct(colnames(FO_grass_1)[1:2914]),
                "day"=date(as.POSIXct(colnames(FO_grass_1)[1:2914])),
                "shf_lower"=NA, "shf_higher"=NA)
colnames(shf_g)[1]<-"dT"
#calculate for every day
for(i in date(daily_VWC_1$day)){
sub=shf_g[shf_g$day==i,]
shf_g$shf_lower[shf_g$day==i]<--daily_VWC_1$lower_k[daily_VWC_1$day==i]*sub$dT/sub$dz
shf_g$shf_higher[shf_g$day==i]<--daily_VWC_1$lower_k[daily_VWC_1$day==i]*sub$dT/sub$dz
}

#plot ts for sub 2
ggplot(data=shf_g)+
  geom_line(aes(DATETIME, shf))+
  theme_bw()+
  ylab(label="shf [W m^-2]")+
  ggtitle("soil heat flux - grass")

#plot diurnal cycle
shf_g$hour<-hour(shf_g$DATETIME) #create column with hour
#plot diurnal cycle for concrete
ggplot(data=shf_g)+
  geom_boxplot(aes(y=shf, x= hour, group=hour))+
  theme_bw()+
  ylab(label="shf [W m^-2]")+
  ggtitle("soil heat flux diurnal - grass")

####plot pretty####
dat<-flux_lower[[2]][[4]]
ggplot(data=dat, aes( depth, shf*-1))+
  geom_point()+
  xlab(label="height [m]")+
  ylab(label="shf [W/m^2]")+
  geom_line()+
  geom_vline(xintercept = 0.4722124, col="brown")+
  geom_vline(xintercept = 0.533174, col="green")+
  theme_bw()+
  ggtitle(label=paste("grass tower - soil heat flux ", names(flux_lower[[1]][4])))

setwd("C:/Users/Dana/Desktop")
ggsave(filename="grass_sample_plot.jpg", width=297, height=210, units = "mm")
