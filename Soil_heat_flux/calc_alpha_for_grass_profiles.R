library(gridExtra)
library(tidyverse)
library(boot)
library(lubridate)
library(dplyr)
library(cmna)
library(plyr)
library(bigsnpr)
library(reshape2)
library(colorspace)
library(scico)
library(patchwork)
#source script to load slow data and QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R") 
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
#source functions
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/functions_alpha.r")
#calculate for individual soil profile
FO_grass_depth<-read.csv("FO_grass.csv")
#FO_grass_aG<-read.csv("FO_grass_20cm_aG_10cm.csv")

#FO_grass_depth<-FO_grass_aG[,c(1:50, 60)] #above threshold
#FO_grass_depth<-FO_grass_aG[,c(1:33, 60)] #new threshold 0.4366514: below original threshold

#transpose
FO_grass_time<-data.frame(t(FO_grass_depth))
#set time as column name
colnames(FO_grass_time)<-FO_grass_depth$grass_time
#delete row with time
FO_grass_time<-FO_grass_time[-dim(FO_grass_time)[1],]
#add depth to plot
FO_grass_plot<-FO_grass_time
FO_grass_plot$depth<-as.numeric(substr(rownames(FO_grass_plot),start = 2, stop=100))
#test subset between "2021-08-02 07:00:00" and "2021-08-02 10:50:00"
which(FO_grass_depth$grass_time== "2021-08-12 09:30:00") #1868
which(FO_grass_depth$grass_time=="2021-08-12 10:50:00") #1876
#soil profile to same same subset as meteo_data
FO_grass_sub<-FO_grass_time[,c(which(colnames(FO_grass_time)=="2021-08-04 07:00:00"):which(colnames(FO_grass_time)=="2021-08-04 09:30:00"))]

#indices: 701:716
#new indices: 413:436
#plot all values
for(i in 1868:1876 ){
  plot(FO_grass_plot$depth, FO_grass_plot[,i],  type="l", 
       main=paste("all values - " ,colnames(FO_grass_plot[i])), ylim=c(14,24))
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
#color_5th_value_grass(range = 413:436, point=1, FO_data_x= FO_grass_time)
#plot_5th_value_grass(FO_data_x = FO_grass_4, range=707:716)
#color_5th_value_grass(FO_data_x=FO_grass_time, 
#                      range=707:716, point=3)

alpha_1_g<-calc_alpha(FO_data_x=FO_grass_1, 1868:1876) #3.003784e-07  new threshold: 7.333339e-07
median(unlist(alpha_1_g[[1]])) #new time span:   6.508261e-07
boxplot(alpha_1_g[[1]])

#for second
#plot_5th_value(FO_data_x = FO_grass_2,  707:716)
alpha_2_g<-calc_alpha(FO_data_x=FO_grass_2,  1868:1876) #2.166522e-07  new threshold: 6.347845e-07
median(unlist(alpha_2_g[[1]])) #new time span:  7.425274e-07
boxplot(alpha_2_g[[1]]) 

#for third
alpha_3_g<-calc_alpha(FO_data_x=FO_grass_3,   1868:1876)  #1.863725e-07  new threshold: 7.344112e-07
median(unlist(alpha_3_g[[1]])) #new time span: 6.901217e-07
boxplot(alpha_3_g[[1]])

#for fourth
alpha_4_g<-calc_alpha(FO_data_x=FO_grass_4,   1868:1876) #1.863725e-07    new threshold: 7.384248e-07
median(unlist(alpha_4_g[[1]])) #new time span: 4.747325e-07
boxplot(alpha_4_g[[1]])

#plot_temp_alpha(FO_data_x = FO_grass_1, alpha_x = alpha_1)

#specific heat capacity
alpha1_g<-median(unlist(alpha_1_g[[1]])) #6.508261e-07
var(unlist(alpha_1_g[[1]])) #variance: 1.011784e-11
alpha2_g<-median(unlist(alpha_2_g[[1]])) #5.53854e-07
var(unlist(alpha_2_g[[1]])) #variance: 1.205055e-11
alpha3_g<-median(unlist(alpha_3_g[[1]])) #6.901217e-07
var(unlist(alpha_3_g[[1]])) #variance: 1.102182e-12
alpha4_g<-median(unlist(alpha_4_g[[1]])) #6.911421e-07
var(unlist(alpha_4_g[[1]])) #variance: 7.486039e-12

#bootstrap values
daily_VWC<-bootstrap_k(alpha=alpha1_g)
range(daily_VWC$VWC, na.rm=T)
####save bootstrapped daily k####
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
write.csv(daily_VWC, file="daily_VWC.csv")
daily_VWC<-read.csv("daily_VWC.csv")

#get VWC and k for sample day
daily_VWC[daily_VWC$day=="2021-08-12",]
k_lower_g<-daily_VWC$lower_k[daily_VWC$day=="2021-08-12"]
flux_lower<-shf(FO_data_x = FO_grass_4, range =  1868:1876, k = k_lower_g)
#plot_shf_grass(flux_dat=flux_lower)

#calculate fluxes to merge together
#one
daily_VWC_1<-bootstrap_k(alpha=alpha1_g)
k_1<-daily_VWC_1$lower_k[daily_VWC_1$day=="2021-08-12"]
#upper k: 1.371805 
#lower k: 1.335676  
flux_1<-shf(FO_data_x=FO_grass_1, range = 1868:1876, k = k_1)
#two
daily_VWC_2<-bootstrap_k(alpha=alpha2_g)
k_2<-daily_VWC_2$lower_k[daily_VWC_2$day=="2021-08-12"]
#upper k:  1.562725   
#lower k: 1.522284    
flux_2<-shf(FO_data_x=FO_grass_2, range = 1868:1876, k = k_2)
#three
daily_VWC_3<-bootstrap_k(alpha=alpha3_g)
k_3<-daily_VWC_3$lower_k[daily_VWC_3$day=="2021-08-12"]
#upper k: 1.453004   
#lower k: 1.413914  
#calculate cv
k_lower<-daily_VWC_3$lower_k[daily_VWC_3$day=="2021-08-12"]
k_upper<- daily_VWC_3$upper_k[daily_VWC_3$day=="2021-08-12"]
k_lower/alpha3_g
k_upper/alpha3_g
flux_3<-shf(FO_data_x=FO_grass_3, range = 1868:1876, k = k_3)
#four
daily_VWC_4<-bootstrap_k(alpha=alpha4_g)
k_4<-daily_VWC_4$lower_k[daily_VWC_4$day=="2021-08-12"]
#upper k: 1.456151  
#lower k: 1.416862 
flux_4<-shf(FO_data_x=FO_grass_4, range =  1868:1876, k = k_4)
#find max value of soil heat flux over time
max_shf<-data.frame("time"=names(flux_2[[1]]), "depth"=NA, "maxflux"=NA) #output dataframe

  
  #for every time step
  for(i in 1:length(flux_1[[2]])){
  #rowbind fluxes from one timestep together
   dat_temp<-rbind(flux_1[[2]][[i]], flux_2[[2]][[i]], flux_3[[2]][[i]], flux_4[[2]][[i]])
  #order for depth
   dat_temp<-dat_temp[order(dat_temp$depth),]
  #find maximum value
   max_shf$depth[i]<-dat_temp$depth[which.max(abs(dat_temp$shf*-1))]
   max_shf$maxflux[i]<-max(abs(dat_temp$shf*-1))
   rm(dat_temp)
  }
#plot depth over a day 
ggplot(data=max_shf)+
  geom_line(aes(x=as.POSIXct(time), y=depth))+
  geom_hline(yintercept = 0.4722124, col="brown")+
  theme_bw()+
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("depth of max shf over time")
ggsave("depth_max_flux_20210804_grass.jpg", width=297, height=210, units = "mm")

class(max_shf$depth)
max(max_shf$depth)
max_shf$time[max_shf$depth>=0.4671322]

#calculate depth of ma shf over time with mean thermal conductivity over time
#for 1
meank1<-mean(daily_VWC_1$lower_k, na.rm=T)
flux_whole1<-shf(FO_data_x=FO_grass_1,range=1:2914 , k = meank1)
#for 2
meank2<-mean(daily_VWC_1$lower_k, na.rm=T)
flux_whole2<-shf(FO_data_x=FO_grass_1,range=1:2914 , k = meank2)
#for 3
meank3<-mean(daily_VWC_3$lower_k, na.rm=T)
flux_whole3<-shf(FO_data_x=FO_grass_3,range=1:2914 , k = meank3)
#for 4
meank4<-mean(daily_VWC_4$lower_k, na.rm=T)
flux_whole4<-shf(FO_data_x=FO_grass_4,range=1:2914 , k = meank4)
#find max value of soil heat flux over time
max_whole_shf<-data.frame("time"=names(flux_whole2[[1]]), "depth"=NA, "maxflux"=NA) #output dataframe
#for every time step
for(i in 1:length(flux_whole1[[2]])){
  #rowbind fluxes from one timestep together
  dat_temp<-rbind(flux_whole1[[2]][[i]], flux_whole2[[2]][[i]], flux_whole3[[2]][[i]], flux_whole4[[2]][[i]])
  #order for depth
  dat_temp<-dat_temp[order(dat_temp$depth),]
  #find maximum value
  max_whole_shf$depth[i]<-dat_temp$depth[which.max(abs(dat_temp$shf*-1))]
  max_whole_shf$maxflux[i]<-max(abs(dat_temp$shf*-1))
  rm(dat_temp)
}

#plot depth over whole time 
ggplot(data=max_whole_shf)+
  geom_line(aes(x=as.POSIXct(time), y=depth))+
  geom_hline(yintercept = 0.4722124, col="brown")+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  #ylim(0.4, 0.48)+
  ylab("Depth [m]")+
  xlab("Time")+
  ggtitle("depth of max shf over time - grass")

ggsave("depth_max_shf_whole_grass.jpg", width=297, height=210, units = "mm")
#add hour as factor
max_whole_shf$hour<-hour(max_whole_shf$time)

class(max_whole_shf$depth)
max(max_whole_shf$depth)
max_whole_shf$time[max_whole_shf$depth>=0.472]
diff(as.POSIXct(max_whole_shf$time[max_whole_shf$depth<=0.462&max_whole_shf$depth<=0.472]))
max_whole_shf$time[max_whole_shf$depth>=0.472][14:23]
hist(max_whole_shf$depth)
max_whole_shf$time[max_whole_shf$depth>=0.462&
                     max_whole_shf$depth<=0.472&
                     max_whole_shf$hour>6&
                     max_whole_shf$hour<11][26:34]
#[48:70]
diff(as.POSIXct(max_whole_shf$time[max_whole_shf$depth>=0.462&
                                     max_whole_shf$depth<=0.472&
                                     max_whole_shf$hour>6&
                                     max_whole_shf$hour<11]))
#plot for mean day

ggplot(data=max_whole_shf)+
  geom_hline(yintercept = 0.4722124, col="brown")+
  geom_boxplot(aes(x=hour, y=depth, group=hour))+
  theme_bw()+
  ggtitle("depth of max shf for mean day - grass")
ggsave("depth_max_flux_mean_day_grass.jpg", width=297, height=210, units = "mm")

#get median for each day
aggregate(max_whole_shf$depth, by=list(max_whole_shf$hour), FUN=median)
#get mean for each day
aggregate(max_whole_shf$depth, by=list(max_whole_shf$hour), FUN=mean)

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
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_line(data=dat_1, aes( depth, shf*-1, col="point_1"))+
  geom_line(data=dat_2, aes( depth, shf*-1, col="point_2"))+
  geom_line(data=dat_3, aes( depth, shf*-1, col="point_3"))+
  geom_line(data=dat_4, aes( depth, shf*-1, col="point_4"))+
  geom_vline(xintercept = 0.4722124, col="brown")+
  geom_vline(xintercept = 0.533174, col="green")+
  theme_bw()+
  ggtitle(label=paste("grass tower - soil heat flux ", names(flux_1[[1]][4])))

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min/",
       filename=paste("four_fluxes_grass.png"),
       width=297, height=210, units = "mm")

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
  ggtitle(label=paste("grass tower - soil heat flux ", names(flux_1[[1]][8])))

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min/",
       filename=paste("ensemble_shf_grass.png"),
       width=297, height=210, units = "mm")

#find max shf for new threshold
dat_ensemble$depth[which.max(dat_ensemble$shf*-1)]
#new threshold: 0.4366514
color_5th_value_grass(range=1868:1876, point=3)
#calculate for whole time period for two centimeters
#get lower and upper k
daily_VWC_1$lower_k 
daily_VWC$upper_k
#calculate temp diff over depth
dT_dz<-(FO_grass_3[9,1:2914]-FO_grass_3[10,1:2914])/diff(FO_grass_3$depth[9:10])
shf_g<-data.frame("dT"=t(FO_grass_3[9,1:2914]-FO_grass_3[10,1:2914]), 
                "dz"=diff(FO_grass_3$depth[9:10]), 
                "DATETIME"=as.POSIXct(colnames(FO_grass_1)[1:2914]),
                "day"=date(as.POSIXct(colnames(FO_grass_1)[1:2914])),
                "shf_lower"=NA, "shf_higher"=NA)
colnames(shf_g)[1]<-"dT"
#calculate for every day
for(i in date(daily_VWC_3$day)){
sub=shf_g[shf_g$day==i,]
shf_g$shf_lower[shf_g$day==i]<--daily_VWC_3$lower_k[daily_VWC_3$day==i]*sub$dT/sub$dz
shf_g$shf_higher[shf_g$day==i]<--daily_VWC_3$upper_k[daily_VWC_3$day==i]*sub$dT/sub$dz
}
#save in csv
write.csv(shf_g, file="shf_grass_20230213.csv", row.names = F)
#plot ts for sub 2 - lower and higher
ggplot(data=shf_g)+
  geom_line(aes(DATETIME, shf_lower, col="lower"))+
  geom_line(aes(DATETIME, shf_higher, col="higher"))+
  theme_bw()+
  ylab(label="shf [W m^-2]")+
  ggtitle("soil heat flux - grass")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min/",
       filename=paste("timeseries_shf_grass.png"),
       width=297, height=210, units = "mm")

#plot difference between higher and lower
#plot ts for sub 2 - lower and higher
ggplot(data=shf_g)+
  geom_line(aes(DATETIME, shf_higher-shf_lower))+
  theme_bw()+
  ylab(label="shf [W m^-2]")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("soil heat flux - grass")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min/",
       filename=paste("timeseries_difference_shf_grass.png"),
       width=297, height=210, units = "mm")
#plot diurnal cycle
shf_g$hour<-hour(shf_g$DATETIME) #create column with hour
#plot diurnal cycle for concrete - lower
ggplot(data=shf_g)+
  geom_boxplot(aes(y=shf_lower, x= hour, group=hour))+
  theme_bw()+
  ylab(label="shf [W m^-2]")+
  ggtitle("soil heat flux diurnal - grass")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min/",
       filename=paste("hourly_lower_shf_grass.png"),
       width=297, height=210, units = "mm")
#plot diurnal cycle for concrete - higher
ggplot(data=shf_g)+
  geom_boxplot(aes(y=shf_higher, x= hour, group=hour))+
  theme_bw()+
  ylab(label="shf [W m^-2]")+
  ggtitle("soil heat flux diurnal - grass")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min/",
       filename=paste("hourly_higher_shf_grass.png"),
       width=297, height=210, units = "mm")

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

####plot test subset for 10 min data for grass####
meteo_grass_sub<-dat.kiebitz.meteo[dat.kiebitz.meteo$TIMESTAMP>=colnames(FO_grass_time)[750]&dat.kiebitz.meteo$TIMESTAMP<=colnames(FO_grass_time)[770],]
  #prepare data
  FO_grass_to_melt<-FO_grass_2[,750:770]
  FO_grass_to_melt$ID<-as.factor(round(as.numeric(substr(rownames(FO_grass_2), start=2, stop=30)), 3))
  FO_grass_melted = reshape2::melt(FO_grass_to_melt, id.vars = "ID")
  #plot soil temperature
    soiltemp<-ggplot(data=FO_grass_melted)+
    geom_line(aes(x=as.POSIXct(variable), y=as.numeric(value), col=ID))+
    theme_bw()+
    xlab(label="Time")+
    ylab(label="Temperature [°C]")+
    scale_color_discrete_sequential(palette="Blues 3")
  #plot meteo vars
    meteo<-ggplot(data=meteo_grass_sub)+
    geom_line(aes(x=as.POSIXct(TIMESTAMP), y=SUp_Avg/20, col="SUp"))+
    geom_line(aes(x=as.POSIXct(TIMESTAMP), y=AirTC_Avg, col="AirT"))+
    theme_bw()
  #plot soil heat flux  
    flux_plot1<-shf(FO_data_x=FO_grass_1, range =  750:760, k = k_1)
    flux_plot2<-shf(FO_data_x=FO_grass_2, range =  750:760, k = k_2)
    flux_plot3<-shf(FO_data_x=FO_grass_3, range =  750:760, k = k_3)
    flux_plot4<-shf(FO_data_x=FO_grass_4, range =  750:760, k = k_4)
  
    shfplot<-ggplot(flux_plot2[[2]][[3]])+
      geom_line(aes(x=depth, y=shf*-1))+
      theme_bw()+
      ggtitle(label=paste("Soil Heat Flux - ", names(flux_plot1[[1]][3])))
    
    soiltemp + shfplot + plot_layout(nrow=2)
 
    soiltemp +  shfplot + meteo  + plot_layout(nrow=3)
  
#save plot
ggsave(filename=paste("Temp_grass_testsubset_1", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
