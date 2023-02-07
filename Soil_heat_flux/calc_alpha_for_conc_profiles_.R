setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
library(gridExtra)
library(tidyverse)
library(lubridate)
#source function
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/functions_alpha.r")
#calculate for individual soil profile
#FO_concrete_aG<-read.csv("FO_concrete_20cm_aG_10cm.csv")
FO_concrete_depth<-read.csv("FO_concrete_20cm.csv")
#FO_concrete_depth<-FO_concrete_aG[,c(1:53, 63)]
#transpose
FO_concrete_time<-data.frame(t(FO_concrete_depth))
#set time as column name
colnames(FO_concrete_time)<-FO_concrete_depth$concrete_time
#delete row with time
FO_concrete_time<-FO_concrete_time[-dim(FO_concrete_time)[1],]
#add depth to plot
FO_concrete_plot<-FO_concrete_time
FO_concrete_plot$depth<-as.numeric(substr(rownames(FO_concrete_plot),start = 2, stop=100))
#30.07.2021  08:00:00 - 09:50:00
#all values
for(i in 107:119 ){
  plot(FO_concrete_plot$depth, FO_concrete_plot[,i],  
       type="l", main=paste("all values - " ,colnames(FO_concrete_plot[i])), 
       ylim=c(20,28))
  abline(v=0.529, col="red")
  points(FO_concrete_plot$depth, FO_concrete_plot[,i])
  Sys.sleep(2)
}

#take every fifth point (variying starting point i)
for(i in 1:4){
  FO_concrete_temp <- FO_concrete_time[seq(i, nrow(FO_concrete_time), 4), ] #select every 5th row
  FO_concrete_temp[] <- lapply(FO_concrete_temp, as.numeric) #coerce to numeric
  FO_concrete_temp$depth<-as.numeric(substr(rownames(FO_concrete_temp),start = 2, stop=100))
  assign(paste0("FO_concrete_", i), FO_concrete_temp) #assign name to object
  rm(FO_concrete_temp)#remove object
}

#"30.07.2021  08:08:00" to "30.07.2021 10:08:00"
#plot all points but color only every fifth
#color_5th_value()

#####calculate for 1 value
#color_5th_value(point=1)
#plot_5th_value(FO_data_x = FO_concrete_1)
alpha_1_c<-calc_alpha(FO_data_x=FO_concrete_1)
median(unlist(alpha_1_c[[1]]))
boxplot(alpha_1_c[[1]])

#####for second
#color_5th_value(point=2)
#plot_5th_value(FO_data_x = FO_concrete_2)
alpha_2_c<-calc_alpha(FO_data_x=FO_concrete_2)
median(unlist(alpha_2_c[[1]]))
#boxplot(alpha_2[[1]])
#####for third
#color_5th_value(point=3)
alpha_3_c<-calc_alpha(FO_data_x=FO_concrete_3)
median(unlist(alpha_3_c[[1]]))
#boxplot(alpha_3[[1]])

#####for fourth
#color_5th_value(point=4)
alpha_4_c<-calc_alpha(FO_data_x=FO_concrete_4)
median(unlist(alpha_4_c[[1]]))
#boxplot(alpha_4[[1]])

#plot values
#plot_5th_value(FO_concrete_2)
#plot_5th_value(FO_concrete_3, x_value="3rd value")
#plot_5th_value(FO_concrete_4, x_value="4th value")
#plot as boxplot


#range_alpha<-as.data.frame(lapply(alpha_x, range))
#mean_alpha<-as.data.frame(lapply(alpha_x, mean))

#plot_temp_alpha(FO_data_x=FO_concrete_2, alpha_x=alpha_2)
#specific heat capacity

alpha1_c<-median(unlist(alpha_1_c[[1]])) # 1.679633e-06
#with 2cm aboveground: 1.531781e-06

alpha2_c<-median(unlist(alpha_2_c[[1]])) # 1.429565e-06
#with 2cm aboveground: 1.21812e-06

alpha3_c<-median(unlist(alpha_3_c[[1]])) # 1.232641e-06
#with 2cm aboveground: aboveground: 

alpha4_c<-median(unlist(alpha_4_c[[1]])) # 1.220921e-06
##with 2cm aboveground: 1.046379e-06

#bootstrap for specific heat or find reliable value
#specific_heat_lower<-1000
#specific_heat_higher<-1200  #mean=1140, sd=25) 
density<-2.409*1000 #measured

#calculate k
#k_lower<-alpha*specific_heat_lower*density
#k_median<-alpha*1100*density
#k_upper<-alpha*specific_heat_higher*density

#from Howlander et al 2012
#-> concrete with similar density and diffusivity
k_est<-alpha2_c*1020*density 
#test
flux_lower<-shf(FO_data_x = FO_concrete_2, k=k_est,range=821:850)
#plot_shf_concrete(flux_dat=flux_lower)

#flux_lower[[2]][[7]]
#calculate fluxes to merge together
#one
k_1<-alpha1_c*1020*density #4.12716
flux_1<-shf(FO_data_x = FO_concrete_1, k=k_1,range=821:850 )
#two
k_2<-alpha2_c*1020*density  #3.512698
flux_2<-shf(FO_data_x = FO_concrete_2, k=k_2,range=821:850 )
#three
k_3<-alpha3_c*1020*density #3.02882
flux_3<-shf(FO_data_x = FO_concrete_3, k=k_3,range=821:850 )
#four
k_4<-alpha4_c*1020*density #3.000022
flux_4<-shf(FO_data_x = FO_concrete_4, k=k_4,range=821:850 )

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
  geom_vline(xintercept = 0.529, col="red")+
  theme_bw()+
  ggtitle(label=paste("concrete tower - soil heat flux ", names(flux_lower[[1]][4])))

#plot as one line
dat_ensemble<-rbind(dat_1, dat_2, dat_3, dat_4)
dat_ensemble<-dat_ensemble[order(dat_ensemble$depth),]
ggplot(data=dat_ensemble, aes( depth, shf*-1))+
  geom_point()+
  xlab(label="height [m]")+
  ylab(label="shf [W/m^2]")+
  geom_line()+
  geom_vline(xintercept = 0.529, col="red")+
  theme_bw()+
  ggtitle(label=paste("concrete tower - soil heat flux ", names(flux_lower[[1]][4])))
#calculate for whole time period for two centimeters
k_1 #get k 
#calculate temp diff over depth
dT_dz<-(FO_concrete_1[10,1:3559]-FO_concrete_1[11,1:3559])/diff(FO_concrete_1$depth[10:11])
shf_vec<--k_2*dT_dz #calculate shf 
shf_whole<-data.frame("shf"=t(shf_vec), "DATETIME"=as.POSIXct(colnames(FO_concrete_2)[1:3559]))
colnames(shf_whole)[1]<-"shf" #rename first column
#plot ts for sub 2
ggplot(data=shf_whole)+
  geom_line(aes(DATETIME, shf))+
  theme_bw()+
  ylab(label="shf [W m^-2")+
  ggtitle("soil heat flux - concrete")

#calculate depth of ma shf over time with mean thermal conductivity over time
#for 1
flux_wholec1<-shf(FO_data_x=FO_concrete_1,range=1:3559 , k = k_1)
#for 2
flux_wholec2<-shf(FO_data_x=FO_concrete_1,range=1:3559 , k = k_2)
#for 3
flux_wholec3<-shf(FO_data_x=FO_concrete_3,range=1:3559 , k = k_3)
#for 4
flux_wholec4<-shf(FO_data_x=FO_concrete_4,range=1:3559 , k = k_4)
#find max value of soil heat flux over time
max_whole_cshf<-data.frame("time"=names(flux_wholec2[[1]]), "depth"=NA, "maxflux"=NA) #output dataframe
#for every time step
for(i in 1:length(flux_wholec1[[2]])){
  #rowbind fluxes from one timestep together
  dat_temp<-rbind(flux_wholec1[[2]][[i]], flux_wholec2[[2]][[i]], flux_wholec3[[2]][[i]], flux_wholec4[[2]][[i]])
  #order for depth
  dat_temp<-dat_temp[order(dat_temp$depth),]
  #find maximum value
  max_whole_cshf$depth[i]<-dat_temp$depth[which.max(abs(dat_temp$shf*-1))]
  max_whole_cshf$maxflux[i]<-max(abs(dat_temp$shf*-1))
  rm(dat_temp)
}

#plot depth over whole time 
ggplot(data=max_whole_cshf)+
  geom_line(aes(x=as.POSIXct(time), y=depth))+
  geom_hline(yintercept = 0.529, col="red")+
  theme_bw()+
  ylab("Depth [m]")+
  xlab("Time")+
  ggtitle("depth of max shf over time - concrete")

ggsave("depth_max_flux_whole.jpg", width=297, height=210, units = "mm")

#plot for mean day
max_whole_cshf$hour<-hour(max_whole_cshf$time)
ggplot(data=max_whole_cshf)+
  geom_hline(yintercept = 0.529, col="red")+
  geom_boxplot(aes(x=hour, y=depth, group=hour))+
  theme_bw()+
  ggtitle("depth of max shf for mean day - concrete")
ggsave("depth_max_flux_mean day.jpg", width=297, height=210, units = "mm")

#get median for each day
aggregate(max_whole_cshf$depth, by=list(max_whole_cshf$hour), FUN=median)
#get mean for each day
aggregate(max_whole_cshf$depth, by=list(max_whole_cshf$hour), FUN=mean)

#check for outliers
#Q3 + (1.5 * IQR)
upper<-as.vector(quantile(shf_whole$shf)[4]+(1.5*IQR(shf_whole$shf)))
#Q1 – (1.5 * IQR)
lower<-as.vector(quantile(shf_whole$shf)[2]-(1.5*IQR(shf_whole$shf)))  

outliers<-which(shf_whole$shf>upper|shf_whole$shf<lower)
shf_nooutlier<-shf_whole[-outliers,]
#plot again
ggplot(data=shf_nooutlier)+
  geom_line(aes(DATETIME, shf))+
  theme_bw()+
  ylab(label="shf [W m^-2")+
  ggtitle("soil heat flux - concrete")

spikes<-which(abs(diff(shf_whole$shf))>100)
shf_nospikes<-shf_whole[-c(spikes,spikes+1)]
#plot again
ggplot(data=shf_nospikes)+
  geom_line(aes(DATETIME, shf))+
  theme_bw()+
  ylab(label="shf [W m^-2")+
  ggtitle("soil heat flux - concrete")
#plot diurnal cycle 
shf_nospikes$hour<-hour(shf_nospikes$DATETIME) #create column with hour
#plot diurnal cycle for concrete
ggplot(data=shf_nospikes)+
  geom_boxplot(aes(y=shf, x= hour, group=hour))+
  theme_bw()+
  ylab(label="shf [W m^-2")+
  ggtitle("soil heat flux diurnal - concrete")

####pretty sample plot####
dat<-flux_lower[[2]][[3]]
ggplot(data=dat, aes( depth, shf*-1))+
  geom_point()+
  ggtitle(label=paste("concrete tower - soil heat flux ", names(flux_lower[[1]][3])))+
  xlab(label="height [m]")+
  ylab(label="shf [W/m^2]")+
  geom_line()+
  geom_vline(xintercept = 0.529, col="red")+
  theme_bw()

setwd("C:/Users/Dana/Desktop")
ggsave(filename="concrete_sample_plot.jpg")
