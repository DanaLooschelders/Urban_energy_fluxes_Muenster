setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
library(gridExtra)
library(tidyverse)
#source function
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/functions_alpha.r")
#calculate for individual soil profile
FO_concrete_aG<-read.csv("FO_concrete_20cm_aG_10cm.csv")
FO_concrete_depth<-read.csv("FO_concrete_20cm.csv")
FO_concrete_depth<-FO_concrete_aG[,c(1:53, 63)]
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
alpha_1<-calc_alpha(FO_data_x=FO_concrete_1, range = 821:850)
median(unlist(alpha_1[[1]]))
boxplot(alpha_1[[1]])

#####for second
#color_5th_value(point=2)
#plot_5th_value(FO_data_x = FO_concrete_2)
alpha_2<-calc_alpha(FO_data_x=FO_concrete_2)
median(unlist(alpha_2[[1]]))
#boxplot(alpha_2[[1]])
#####for third
#color_5th_value(point=3)
alpha_3<-calc_alpha(FO_data_x=FO_concrete_3)
median(unlist(alpha_3[[1]]))
#boxplot(alpha_3[[1]])

#####for fourth
#color_5th_value(point=4)
alpha_4<-calc_alpha(FO_data_x=FO_concrete_4)
median(unlist(alpha_4[[1]]))
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

alpha1_c<-median(unlist(alpha_1[[1]])) # 1.679633e-06
#with 2cm aboveground: 1.531781e-06

alpha2_c<-median(unlist(alpha_2[[1]])) # 1.429565e-06
#with 2cm aboveground: 1.21812e-06

alpha3_c<-median(unlist(alpha_3[[1]])) # 1.232641e-06
#with 2cm aboveground: aboveground: 

alpha4_c<-median(unlist(alpha_4[[1]])) # 1.220921e-06
##with 2cm aboveground: 1.046379e-06

#bootstrap for specific heat or find reliable value
specific_heat_lower<-1000
specific_heat_higher<-1200  #mean=1140, sd=25) 
density<-2.409*1000 #measured

#calculate k
k_lower<-alpha*specific_heat_lower*density
k_median<-alpha*1100*density
k_upper<-alpha*specific_heat_higher*density

#from Howlander et al 2012
#-> concrete with similar density and diffusivity
k_est<-alpha2_c*1020*density 
#test
flux_lower<-shf(FO_data_x = FO_concrete_2, k=k_est,range=821:850 )
plot_shf_concrete(flux_dat=flux_lower)

flux_lower[[2]][[7]]
#calculate fluxes to merge together
#one
k_1<-alpha1_c*1020*density 
flux_1<-shf(FO_data_x = FO_concrete_1, k=k_1,range=821:850 )
#two
k_2<-alpha2_c*1020*density 
flux_2<-shf(FO_data_x = FO_concrete_2, k=k_2,range=821:850 )
#three
k_3<-alpha3_c*1020*density 
flux_3<-shf(FO_data_x = FO_concrete_3, k=k_3,range=821:850 )
#four
k_4<-alpha4_c*1020*density 
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

####pretty plot####
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
ggsave(filename="concrete_sample_plot.jpg", width=297, height=210, units = "mm")
