#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/data_FO_grass_QAQC_plot.R")
beep()
#calculate diffusivity
library(cmna)
library(plyr)
library(bigsnpr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
#change original heat function to use a vector of heights and times
#no need when using the upmost 10-20 cm (same heights of 0.005080137 there)
heat_heights <- function (u, alpha , xdelta , tdelta , n) {
  m <- length (u)
  uarray <- matrix (u, nrow = 1)
  newu <- u
  for(i in 1:n) {
    for(j in 2:(m - 1)) {
      h <- alpha * tdelta / xdelta[j - 1] ^2 #set xdelta to individual height
      ustep <- (u[j - 1] + u[j + 1] - 2 * u[j])
      newu [j] <- u[j] + h * ustep
    }
    u <- newu
    u[1] <- u[m]
    uarray <- rbind (uarray , u)
  }
  return ( uarray )
}


#####heat function####
#u = initial values of u
#alpha = thermal diffusivity
#xdelta = change in x (space) at each step in u
#tdelta = time step
#n = number of steps to take

####prep data####

#choose soil layer
#0 - 5 cm
FO_grass_layer<-FO_grass_df[,83:93]
layer_name<-"5_15"

#0 - 10 cm
FO_grass_layer<-FO_grass_df[,74:93]
layer_name<-"0_10"

#transpose dataframe
FO_grass_df_t<-as.data.frame(t(FO_grass_layer))
colnames(FO_grass_df_t)<-FO_grass_temp_time_df_order$time #set time as colnames

#split data in test and validation data
#2/3 test and 1/3 Validation
#subset test
FO_grass_df_test<-FO_grass_df_t[,1:round(length(FO_grass_df_t)/3*2, 0)]
#choose only 2 1 day periods from test data.frame to reduce computational cost
60*60*24/24 #3600 Files sind 1 tag
#subset day 1
range_test_1<-range(which(colnames(FO_grass_df_test)>"2021-07-30 08:00:00 CEST"&colnames(FO_grass_df_test)<"2021-07-31 08:00:00 CEST"))
FO_grass_df_test_subset_1<-FO_grass_df_test[,range_test_1[1]:range_test_1[2]]
#range(FO_grass_temp_time_df_order$time[10000:13600]) #timespan subset 1
#get time differences for subset 1
difftime_grass_1<-as.vector(diff.POSIXt(FO_grass_temp_time_df_order$time[range_test_1[1]:range_test_1[2]]))

#subset validation
FO_grass_df_validation<-FO_grass_df_t[,round(length(FO_grass_df_t)/3*2, 0):length(FO_grass_df_t)]

####run  loop for subset 1 ####
#run heat function for every time step
#Mineral soils of different textures, containing the same amount of water, 
#exhibit very different thermal conductivities. 
#hen they are compared at the same moisture tensions, 
#their thermal conductivities are similar. 
#Moisture variations have a much greater effect on thermal conductivity of soils 
#than bulk density and grain size.

#alpha.range<-seq(1*10^-50, 11*10^-7, by=0.1*10^-7)
#try a log sequence to cover greater range of values

alpha.range<-seq_log(1*10^-15, 1*10^-3, length.out = 100)
#create output dataframe for alpha and RMSEs
alpha_rmse_1<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#create vector of measured data for RMSE calculation
FO_grass_df_test_subset_1_measured<-FO_grass_df_test_subset_1
FO_grass_df_test_subset_1_measured<-FO_grass_df_test_subset_1_measured[-1,] #remove first row (invalid)
FO_grass_df_test_subset_1_measured<-FO_grass_df_test_subset_1_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_grass_df_test_subset_1_measured))
#run loop for broad range of alphas

for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_grass_df_pred_1<-setNames(data.frame(matrix(ncol = ncol(FO_grass_df_test_subset_1), 
                                                    nrow = nrow(FO_grass_df_test_subset_1))), 
                                  colnames(FO_grass_df_test_subset_1))
  rownames(FO_grass_df_pred_1)<-rownames(FO_grass_df_test_subset_1)
  for(i in 1:ncol(FO_grass_df_test_subset_1)){
    #print(i)
    pred_temp<-heat_heights(u=FO_grass_df_test_subset_1[,i], alpha=alpha.range[x], 
                    xdelta=diff(as.numeric(rownames(FO_grass_df_test_subset_1))), tdelta=difftime_grass_1[i], n=2)
    FO_grass_df_pred_1[,i+1]<-pred_temp[2,] #
  }
  FO_grass_df_pred_1<-FO_grass_df_pred_1[-1,] #remove first row (invalid)
  FO_grass_df_pred_1<-FO_grass_df_pred_1[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_1<-FO_grass_df_pred_1[,-dim(FO_grass_df_pred_1)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_1))
  alpha_rmse_1$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot(alpha_rmse_1$alpha, alpha_rmse_1$RMSE)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(alpha_rmse_1, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = paste("Grass_test_subset_1", layer_name, "rmse_coarse_spectrum_full_plot.png", sep="_"),
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_1[60:70,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = paste("Grass_test_subset_1", layer_name, "rmse_coarse_spectrum_subset_plot.png", sep="_"),
       width=297, height=210, units = "mm")

alpha_1_1<-alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] #for a day  7.564633e-08
rmse_1_1<-min(alpha_rmse_1$RMSE) #for a day  0.1108015
#define new threshold
lower<-alpha_1_1-(alpha_1_1*30/100)
upper<-alpha_1_1+(alpha_1_1*30/100)
by_unit<-substr(lower, nchar(lower)-3, stop=nchar(lower))
#optimal alpha was 7.564633e-08
alpha.range<-seq(lower, upper, by=as.numeric(paste(0.01, by_unit, sep="")))
#create output dataframe for alpha and RMSEs
alpha_rmse_1<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#run loop with narrow range of alpha
for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_grass_df_pred_1<-setNames(data.frame(matrix(ncol = ncol(FO_grass_df_test_subset_1), 
                                                    nrow = nrow(FO_grass_df_test_subset_1))), 
                                  colnames(FO_grass_df_test_subset_1))
  rownames(FO_grass_df_pred_1)<-rownames(FO_grass_df_test_subset_1)
  for(i in 1:ncol(FO_grass_df_test_subset_1)){
    #print(i)
    pred_temp<-heat_heights(u=FO_grass_df_test_subset_1[,i], alpha=alpha.range[x], 
                    xdelta=diff(as.numeric(rownames(FO_grass_df_test_subset_1))), tdelta=difftime_grass_1[i], n=2)
    FO_grass_df_pred_1[,i+1]<-pred_temp[2,]
  }
  FO_grass_df_pred_1<-FO_grass_df_pred_1[-1,] #remove first row (invalid)
  FO_grass_df_pred_1<-FO_grass_df_pred_1[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_1<-FO_grass_df_pred_1[,-dim(FO_grass_df_pred_1)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_1))
  alpha_rmse_1$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot results
ggplot(data=alpha_rmse_1)+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Grass_test_subset_1_rmse_fine_spectrum_full_plot.png",
       width=297, height=210, units = "mm")
alpha_1_2<-alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] #for one day 2.92e-07
rmse_1_2<-min(alpha_rmse_1$RMSE) #for one day 0.08614053

#####try for second subset####
#subset hour 2
range_test_2<-range(which(colnames(FO_grass_df_test)>"2021-08-04 00:00:00 CEST"&colnames(FO_grass_df_test)<"2021-08-05 00:00:00 CEST"))
FO_grass_df_test_subset_2<-FO_grass_df_test[,range_test_2[1]:range_test_2[2]]
#range(FO_grass_temp_time_df_order$time[40000:43600]) #timespan subset 2

#get time differences for subset 1
difftime_grass_2<-as.vector(diff.POSIXt(FO_grass_temp_time_df_order$time[range_test_2[1]:range_test_2[2]]))
#run heat function for every time step
#Effects-of-aggregate-types-on-thermal-properties-of-grass (2012)
#alpha.range<-seq(1*10^-8, 11*10^-7, by=0.1*10^-7)
alpha.range<-seq_log(1*10^-15, 1*10^-3, length.out = 100)
#create output dataframe for alpha and RMSEs
alpha_rmse_2<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#create vector of measured data for RMSE calculation
FO_grass_df_test_subset_2_measured<-FO_grass_df_test_subset_2
FO_grass_df_test_subset_2_measured<-FO_grass_df_test_subset_2_measured[-1,] #remove first row (invalid)
FO_grass_df_test_subset_2_measured<-FO_grass_df_test_subset_2_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_grass_df_test_subset_2_measured))
#run loop for broad range of alphas
for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_grass_df_pred_2<-setNames(data.frame(matrix(ncol = ncol(FO_grass_df_test_subset_2), 
                                                    nrow = nrow(FO_grass_df_test_subset_2))), 
                                  colnames(FO_grass_df_test_subset_2))
  rownames(FO_grass_df_pred_2)<-rownames(FO_grass_df_test_subset_2)
  for(i in 1:ncol(FO_grass_df_test_subset_2)){
    #print(i)
    pred_temp<-heat_heights(u=FO_grass_df_test_subset_2[,i], alpha=alpha.range[x], 
                    xdelta=diff(as.numeric(rownames(FO_grass_df_test_subset_2))), tdelta=difftime_grass_2[i], n=2)
    FO_grass_df_pred_2[,i+1]<-pred_temp[2,]
  }
  FO_grass_df_pred_2<-FO_grass_df_pred_2[-1,] #remove first row (invalid)
  FO_grass_df_pred_2<-FO_grass_df_pred_2[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_2<-FO_grass_df_pred_2[,-dim(FO_grass_df_pred_2)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_2))
  alpha_rmse_2$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(alpha_rmse_2, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Grass_test_subset_2_rmse_coarse_spectrum_full_plot.png",
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_2[55:75,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Grass_test_subset_2_rmse_coarse_spectrum_subset_plot.png",
       width=297, height=210, units = "mm")



alpha_2_1<-alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #for a day 1.321941e-07
rmse_2_1<-min(alpha_rmse_2$RMSE) #for a day 0.09555659
#define new alpha range
lower<-alpha_2_1-(alpha_2_1*30/100)
upper<-alpha_2_1+(alpha_2_1*30/100)
by_unit<-substr(lower, nchar(lower)-3, stop=nchar(lower))

alpha.range<-seq(lower, upper, by=as.numeric(paste(0.01, by_unit, sep="")))

#create output dataframe for alpha and RMSEs
alpha_rmse_2<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#run loop with narrow range of alpha
for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_grass_df_pred_2<-setNames(data.frame(matrix(ncol = ncol(FO_grass_df_test_subset_2), 
                                                    nrow = nrow(FO_grass_df_test_subset_2))), 
                                  colnames(FO_grass_df_test_subset_2))
  rownames(FO_grass_df_pred_2)<-rownames(FO_grass_df_test_subset_2)
  for(i in 1:ncol(FO_grass_df_test_subset_2)){
    #print(i)
    pred_temp<-heat_heights(u=FO_grass_df_test_subset_2[,i], alpha=alpha.range[x], 
                    xdelta=diff(as.numeric(rownames(FO_grass_df_test_subset_2))), tdelta=difftime_grass_2[i], n=2)
    FO_grass_df_pred_2[,i+1]<-pred_temp[2,]
  }
  FO_grass_df_pred_2<-FO_grass_df_pred_2[-1,] #remove first row (invalid)
  FO_grass_df_pred_2<-FO_grass_df_pred_2[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_2<-FO_grass_df_pred_2[,-dim(FO_grass_df_pred_2)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_2))
  alpha_rmse_2$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot results
ggplot(data=alpha_rmse_2)+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Grass_test_subset_2_rmse_fine_spectrum_full_plot.png",
       width=297, height=210, units = "mm")

ggplot(data=alpha_rmse_2[60:120,])+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Grass_test_subset_2_rmse_fine_spectrum_subset_plot.png",
       width=297, height=210, units = "mm")

alpha_2_2<-alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #for a day 1.29e-07
rmse_2_2<-min(alpha_rmse_2$RMSE) #for a day  0.09555573

#mean_alpha<-mean(c(1.95*10^-7, 1.83*10^-7)) #1.89e-07

#####try for third subset####
#subset hour 3
range_test_3<-range(which(colnames(FO_grass_df_test)>"2021-08-11 00:00:00 CEST"&colnames(FO_grass_df_test)<"2021-08-12 00:00:00 CEST"))
FO_grass_df_test_subset_3<-FO_grass_df_test[,range_test_3[1]:range_test_3[2]]
#range(FO_grass_temp_time_df_order$time[44000:47600]) #timespan subset 2

#get time differences for subset 1
difftime_grass_3<-as.vector(diff.POSIXt(FO_grass_temp_time_df_order$time[range_test_3[1]:range_test_3[2]]))
#run heat function for every time step
#Effects-of-aggregate-types-on-thermal-properties-of-grass (2012)
#alpha.range<-seq(1*10^-8, 11*10^-7, by=0.1*10^-7)
alpha.range<-seq_log(1*10^-15, 1*10^-3, length.out = 100)
#create output dataframe for alpha and RMSEs
alpha_rmse_3<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#create vector of measured data for RMSE calculation
FO_grass_df_test_subset_3_measured<-FO_grass_df_test_subset_3
FO_grass_df_test_subset_3_measured<-FO_grass_df_test_subset_3_measured[-1,] #remove first row (invalid)
FO_grass_df_test_subset_3_measured<-FO_grass_df_test_subset_3_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_grass_df_test_subset_3_measured))
#run loop for broad range of alphas

for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_grass_df_pred_3<-setNames(data.frame(matrix(ncol = ncol(FO_grass_df_test_subset_3), 
                                                 nrow = nrow(FO_grass_df_test_subset_3))), 
                               colnames(FO_grass_df_test_subset_3))
  rownames(FO_grass_df_pred_3)<-rownames(FO_grass_df_test_subset_3)
  for(i in 1:ncol(FO_grass_df_test_subset_3)){
    #print(i)
    pred_temp<-heat_heights(u=FO_grass_df_test_subset_3[,i], alpha=alpha.range[x], 
                            xdelta=diff(as.numeric(rownames(FO_grass_df_test_subset_3))), tdelta=difftime_grass_3[i], n=2)
    FO_grass_df_pred_3[,i+1]<-pred_temp[2,]
  }
  FO_grass_df_pred_3<-FO_grass_df_pred_3[-1,] #remove first row (invalid)
  FO_grass_df_pred_3<-FO_grass_df_pred_3[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_3<-FO_grass_df_pred_3[,-dim(FO_grass_df_pred_3)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_3))
  alpha_rmse_3$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(alpha_rmse_3, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Grass_test_subset_3_rmse_coarse_spectrum_full_plot.png",
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_3[35:72,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Grass_test_subset_3_rmse_coarse_spectrum_subset_plot.png",
       width=297, height=210, units = "mm")

#plot(alpha_rmse_2$alpha, alpha_rmse_2$RMSE)
alpha_3_1<-alpha_rmse_3$alpha[which.min(alpha_rmse_3$RMSE)] #for a day 1.321941e-07
rmse_3_1<-min(alpha_rmse_3$RMSE) #for a day 0.08416563

lower<-alpha_3_1-(alpha_3_1*30/100)
upper<-alpha_3_1+(alpha_3_1*30/100)
by_unit<-substr(lower, nchar(lower)-3, stop=nchar(lower))

alpha.range<-seq(lower, upper, by=as.numeric(paste(0.01, by_unit, sep="")))

#create output dataframe for alpha and RMSEs
alpha_rmse_3<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#run loop with narrow range of alpha
for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 3
  FO_grass_df_pred_3<-setNames(data.frame(matrix(ncol = ncol(FO_grass_df_test_subset_3), 
                                                 nrow = nrow(FO_grass_df_test_subset_3))), 
                               colnames(FO_grass_df_test_subset_3))
  rownames(FO_grass_df_pred_3)<-rownames(FO_grass_df_test_subset_3)
  for(i in 1:ncol(FO_grass_df_test_subset_3)){
    #print(i)
    pred_temp<-heat(u=FO_grass_df_test_subset_3[,i], alpha=alpha.range[x], 
                            xdelta=0.005080137, tdelta=difftime_grass_3[i], n=2)
    FO_grass_df_pred_3[,i+1]<-pred_temp[2,]
  }
  FO_grass_df_pred_3<-FO_grass_df_pred_3[-1,] #remove first row (invalid)
  FO_grass_df_pred_3<-FO_grass_df_pred_3[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_3<-FO_grass_df_pred_3[,-dim(FO_grass_df_pred_3)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_3))
  alpha_rmse_3$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot results
ggplot(data=alpha_rmse_3)+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Grass_test_subset_3_rmse_fine_spectrum_full_plot.png",
       width=297, height=210, units = "mm")

ggplot(data=alpha_rmse_3[105:150,])+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Grass_test_subset_3_rmse_fine_spectrum_subset_plot.png",
       width=297, height=210, units = "mm")

alpha_3_2<-alpha_rmse_3$alpha[which.min(alpha_rmse_3$RMSE)] #for a day  1.195e-07
rmse_3_2<-min(alpha_rmse_3$RMSE) #for a day 0.08455864

mean_alpha <- mean(c(alpha_1_2, alpha_2_2, alpha_3_2)) #1.803333e-07
sd_alpha <- sd(c(alpha_1_2, alpha_2_2, alpha_3_2))
#####validate for day in last third of dataframe####
#subset hour 1
range_validation<-range(which(colnames(FO_grass_df_validation)>"2021-08-14 16:00:00 CEST"&colnames(FO_grass_df_validation)<"2021-08-15 16:00:00 CEST"))

FO_grass_df_validation_subset<-FO_grass_df_validation[,range_validation[1]:range_validation[2]]
#create vector of measured data for RMSE calculation
FO_grass_df_validation_subset_measured<-FO_grass_df_validation_subset
FO_grass_df_validation_subset_measured<-FO_grass_df_validation_subset_measured[-1,] #remove first row (invalid)
FO_grass_df_validation_subset_measured<-FO_grass_df_validation_subset_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_grass_df_validation_subset_measured))

#get time differences for subset 3
difftime_grass_3<-as.vector(diff.POSIXt(as.POSIXct(colnames(FO_grass_df_validation_subset))))

#create output dataframe for validation
FO_grass_df_pred_3<-setNames(data.frame(matrix(ncol = ncol(FO_grass_df_validation_subset), 
                                                  nrow = nrow(FO_grass_df_validation_subset))), 
                                colnames(FO_grass_df_validation_subset))
rownames(FO_grass_df_pred_3)<-rownames(FO_grass_df_validation_subset)
for(i in 1:ncol(FO_grass_df_validation_subset)){
  #print(i)
  pred_temp<-heat_heights_heights(u=FO_grass_df_validation_subset[,i], alpha=mean_alpha, 
                  xdelta=diff(as.numeric(rownames(FO_grass_df_validation_subset))), tdelta=difftime_grass_3[i], n=2)
  FO_grass_df_pred_3[,i+1]<-pred_temp[2,]
}
FO_grass_df_pred_3<-FO_grass_df_pred_3[-1,] #remove first row (invalid)
FO_grass_df_pred_3<-FO_grass_df_pred_3[,-1] #remove first column (cannot be predicted)
FO_grass_df_pred_3<-FO_grass_df_pred_3[,-dim(FO_grass_df_pred_3)[2]] #remove last column -> no measured values
data_predicted<-as.vector(t(FO_grass_df_pred_3))
rmse_validation<-sqrt(mean((data_measured - data_predicted)^2))
#0.08997937
result<-data.frame("alpha"=c(alpha_1_1,
                          alpha_1_2,
                          alpha_2_1,
                          alpha_2_2,
                          alpha_3_1,
                          alpha_3_2,
                          mean_alpha),
                "rmse"=c(rmse_1_1,
                         rmse_1_2,
                         rmse_2_1,
                         rmse_2_2,
                         rmse_3_1,
                         rmse_3_2,
                         rmse_validation))
####plot####
plotTestSubset<- function(subsetName){
  #prepare data
  FO_grass_to_melt<-subsetName
  FO_grass_to_melt$ID<-as.factor(round(as.numeric(rownames(subsetName)), 3))
  FO_grass_melted = reshape2::melt(FO_grass_to_melt, id.vars = "ID")
#plot
ggplot(data=FO_grass_melted)+
  geom_line(aes(x=as.POSIXct(variable), y=value, col=ID))+
  theme_bw()+
  xlab(label="time")+
  ylab(label="Temperature [°C]")+
  scale_color_discrete_sequential(palette="Blues 3")
}
#test 1
plotTestSubset(FO_grass_df_test_subset_1)
ggsave(filename=paste("Temp_grass_testsubset_1", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
#test 2
plotTestSubset(FO_grass_df_test_subset_2)
ggsave(filename=paste("Temp_grass_testsubset_2", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
#test 3
plotTestSubset(FO_grass_df_test_subset_3)
ggsave(filename=paste("Temp_grass_testsubset_3", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
#validation
plotTestSubset(FO_grass_df_validation_subset)
ggsave(filename=paste("Temp_grass_validationsubset", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
#whole data
plotTestSubset(FO_grass_df_t)
ggsave(filename=paste("Temp_grass", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
