#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/data_FO_concrete_QAQC_plot.R")
#calculate diffusivity
library(cmna)
library(plyr)
library(bigsnpr)
#####heat function####
    #u = initial values of u
    #alpha = thermal diffusivity
    #xdelta = change in x (space) at each step in u
    #tdelta = time step
    #n = number of steps to take

#transpose dataframe
FO_concrete_df_t<-as.data.frame(t(FO_concrete_df))
colnames(FO_concrete_df_t)<-FO_concrete_temp_time_df$time #set time as colnames
#split data in test and validation data
#2/3 test and 1/3 Validation
#subset test
FO_concrete_df_test<-FO_concrete_df_t[,1:round(length(FO_concrete_df_t)/3*2, 0)]
#choose only 3 1 day periods from test data.frame to reduce computational cost
60*60*24/24 #3600 Files sind 1 tag
#subset day 1
FO_concrete_df_test_subset_1<-FO_concrete_df_test[,10000:13600]
range(FO_concrete_temp_time_df$time[10000:13600]) #timespan subset 1
#get time differences for subset 1
difftime_concrete_1<-as.vector(diff.POSIXt(FO_concrete_temp_time_df$time[10000:13600]))

#subset validation
FO_concrete_df_validation<-FO_concrete_df_t[,1:round(length(FO_concrete_df_t)/3, 0)]
####run  loop for subset 1 ####
#run heat function for every time step
#Effects-of-aggregate-types-on-thermal-properties-of-concrete (2012)
#alpha.range<-seq(1*10^-50, 11*10^-7, by=0.1*10^-7)
#try a log sequence to cover greater range of values

alpha.range<-seq_log(1*10^-10, 1*10^-6, length.out = 100)
#create output dataframe for alpha and RMSEs
alpha_rmse_1<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#create vector of measured data for RMSE calculation
FO_concrete_df_test_subset_1_measured<-FO_concrete_df_test_subset_1
FO_concrete_df_test_subset_1_measured<-FO_concrete_df_test_subset_1_measured[-1,] #remove first row (invalid)
FO_concrete_df_test_subset_1_measured<-FO_concrete_df_test_subset_1_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_concrete_df_test_subset_1_measured))
#run loop for broad range of alphas

for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_concrete_df_pred_1<-setNames(data.frame(matrix(ncol = ncol(FO_concrete_df_test_subset_1), 
                                                    nrow = nrow(FO_concrete_df_test_subset_1))), 
                                  colnames(FO_concrete_df_test_subset_1))
  rownames(FO_concrete_df_pred_1)<-rownames(FO_concrete_df_test_subset_1)
for(i in 1:ncol(FO_concrete_df_test_subset_1)){
  #print(i)
pred_temp<-heat(u=FO_concrete_df_test_subset_1[,i], alpha=alpha.range[x], 
     xdelta=0.005089005, tdelta=difftime_concrete_1[i], n=2)
FO_concrete_df_pred_1[,i+1]<-pred_temp[2,]
}
  FO_concrete_df_pred_1<-FO_concrete_df_pred_1[-1,] #remove first row (invalid)
  FO_concrete_df_pred_1<-FO_concrete_df_pred_1[,-1] #remove first column (cannot be predicted)
  FO_concrete_df_pred_1<-FO_concrete_df_pred_1[,-dim(FO_concrete_df_pred_1)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_concrete_df_pred_1))
  alpha_rmse_1$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
 
}


#plot(alpha_rmse_1$alpha, alpha_rmse_1$RMSE)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(alpha_rmse_1, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Concrete_test_subset_1_rmse_coarse_spectrum_full_plot.png",
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_1[40:85,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Concrete_test_subset_1_rmse_coarse_spectrum_subset_plot.png",
       width=297, height=210, units = "mm")
alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] # 9.7701e-08
min(alpha_rmse_1$RMSE) #0.07632424

#optimal alpha was 9.7701e-08
alpha.range<-seq(8.8*10^-8, 1.2*10^-7, by=0.01*10^-8)
#create output dataframe for alpha and RMSEs
alpha_rmse_1<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#run loop with narrow range of alpha
for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_concrete_df_pred_1<-setNames(data.frame(matrix(ncol = ncol(FO_concrete_df_test_subset_1), 
                                                    nrow = nrow(FO_concrete_df_test_subset_1))), 
                                  colnames(FO_concrete_df_test_subset_1))
  rownames(FO_concrete_df_pred_1)<-rownames(FO_concrete_df_test_subset_1)
  for(i in 1:ncol(FO_concrete_df_test_subset_1)){
    #print(i)
    pred_temp<-heat(u=FO_concrete_df_test_subset_1[,i], alpha=alpha.range[x], 
                    xdelta=0.005089005, tdelta=difftime_concrete_1[i], n=2)
    FO_concrete_df_pred_1[,i+1]<-pred_temp[2,]
  }
  FO_concrete_df_pred_1<-FO_concrete_df_pred_1[-1,] #remove first row (invalid)
  FO_concrete_df_pred_1<-FO_concrete_df_pred_1[,-1] #remove first column (cannot be predicted)
  FO_concrete_df_pred_1<-FO_concrete_df_pred_1[,-dim(FO_concrete_df_pred_1)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_concrete_df_pred_1))
  alpha_rmse_1$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot results
ggplot(data=alpha_rmse_1)+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Concrete_test_subset_1_rmse_fine_spectrum_full_plot.png", 
       width=297, height=210, units = "mm")

ggplot(data=alpha_rmse_1[35:100,])+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Concrete_test_subset_1_rmse_fine_spectrum_subset_plot.png", 
       width=297, height=210, units = "mm")

alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] #9.46e-08
min(alpha_rmse_1$RMSE) # 0.07632312


#####try for second subset####
#subset hour 2
FO_concrete_df_test_subset_2<-FO_concrete_df_test[,40000:43600]
range(FO_concrete_temp_time_df$time[40000:43600]) #timespan subset 2

#get time differences for subset 1
difftime_concrete_2<-as.vector(diff.POSIXt(FO_concrete_temp_time_df$time[40000:43600]))
#run heat function for every time step
#Effects-of-aggregate-types-on-thermal-properties-of-concrete (2012)
#alpha.range<-seq(1*10^-8, 11*10^-7, by=0.1*10^-7)
alpha.range<-seq_log(1*10^-10, 1*10^-6, length.out = 100)
#create output dataframe for alpha and RMSEs
alpha_rmse_2<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#create vector of measured data for RMSE calculation
FO_concrete_df_test_subset_2_measured<-FO_concrete_df_test_subset_2
FO_concrete_df_test_subset_2_measured<-FO_concrete_df_test_subset_2_measured[-1,] #remove first row (invalid)
FO_concrete_df_test_subset_2_measured<-FO_concrete_df_test_subset_2_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_concrete_df_test_subset_2_measured))
#run loop for broad range of alphas
for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_concrete_df_pred_2<-setNames(data.frame(matrix(ncol = ncol(FO_concrete_df_test_subset_2), 
                                                    nrow = nrow(FO_concrete_df_test_subset_2))), 
                                  colnames(FO_concrete_df_test_subset_2))
  rownames(FO_concrete_df_pred_2)<-rownames(FO_concrete_df_test_subset_2)
  for(i in 1:ncol(FO_concrete_df_test_subset_2)){
    #print(i)
    pred_temp<-heat(u=FO_concrete_df_test_subset_2[,i], alpha=alpha.range[x], 
                    xdelta=0.005089005, tdelta=difftime_concrete_2[i], n=2)
    FO_concrete_df_pred_2[,i+1]<-pred_temp[2,]
  }
  FO_concrete_df_pred_2<-FO_concrete_df_pred_2[-1,] #remove first row (invalid)
  FO_concrete_df_pred_2<-FO_concrete_df_pred_2[,-1] #remove first column (cannot be predicted)
  FO_concrete_df_pred_2<-FO_concrete_df_pred_2[,-dim(FO_concrete_df_pred_2)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_concrete_df_pred_2))
  alpha_rmse_2$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot(alpha_rmse_2$alpha, alpha_rmse_2$RMSE)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(alpha_rmse_2, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Concrete_test_subset_2_rmse_coarse_spectrum_full_plot.png",
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_2[50:90,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Concrete_test_subset_2_rmse_coarse_spectrum_subset_plot.png",
       width=297, height=210, units = "mm")

alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #1.29155e-07
min(alpha_rmse_2$RMSE) #0.07255682

#optimal alpha was 1.29155e-07
alpha.range<-seq(9.9*10^-8, 1.6*10^-7, by=0.01*10^-8)
#create output dataframe for alpha and RMSEs
alpha_rmse_2<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#run loop with narrow range of alpha
for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_concrete_df_pred_2<-setNames(data.frame(matrix(ncol = ncol(FO_concrete_df_test_subset_2), 
                                                    nrow = nrow(FO_concrete_df_test_subset_2))), 
                                  colnames(FO_concrete_df_test_subset_2))
  rownames(FO_concrete_df_pred_2)<-rownames(FO_concrete_df_test_subset_2)
  for(i in 1:ncol(FO_concrete_df_test_subset_2)){
    #print(i)
    pred_temp<-heat(u=FO_concrete_df_test_subset_2[,i], alpha=alpha.range[x], 
                    xdelta=0.005089005, tdelta=difftime_concrete_2[i], n=2)
    FO_concrete_df_pred_2[,i+1]<-pred_temp[2,]
  }
  FO_concrete_df_pred_2<-FO_concrete_df_pred_2[-1,] #remove first row (invalid)
  FO_concrete_df_pred_2<-FO_concrete_df_pred_2[,-1] #remove first column (cannot be predicted)
  FO_concrete_df_pred_2<-FO_concrete_df_pred_2[,-dim(FO_concrete_df_pred_2)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_concrete_df_pred_2))
  alpha_rmse_2$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot results
ggplot(data=alpha_rmse_2)+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Concrete_test_subset_2_rmse_fine_spectrum_full_plot.png",
       width=297, height=210, units = "mm")

ggplot(data=alpha_rmse_2[220:280,])+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Concrete_test_subset_2_rmse_fine_spectrum_subset_plot.png",
       width=297, height=210, units = "mm")

alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #1.241e-07
min(alpha_rmse_2$RMSE) #0.0725546

####run  loop for subset 3 ####
#subset day 3
FO_concrete_df_test_subset_3<-FO_concrete_df_test[,44000:47600]
range(FO_concrete_temp_time_df$time[44000:47600]) #timespan subset 1
#get time differences for subset 1
difftime_concrete_3<-as.vector(diff.POSIXt(FO_concrete_temp_time_df$time[44000:47600]))

alpha.range<-seq_log(1*10^-10, 1*10^-6, length.out = 100)
#create output dataframe for alpha and RMSEs
alpha_rmse_3<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#create vector of measured data for RMSE calculation
FO_concrete_df_test_subset_3_measured<-FO_concrete_df_test_subset_3
FO_concrete_df_test_subset_3_measured<-FO_concrete_df_test_subset_3_measured[-1,] #remove first row (invalid)
FO_concrete_df_test_subset_3_measured<-FO_concrete_df_test_subset_3_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_concrete_df_test_subset_3_measured))
#run loop for broad range of alphas

for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_concrete_df_pred_3<-setNames(data.frame(matrix(ncol = ncol(FO_concrete_df_test_subset_3), 
                                                    nrow = nrow(FO_concrete_df_test_subset_3))), 
                                  colnames(FO_concrete_df_test_subset_3))
  rownames(FO_concrete_df_pred_3)<-rownames(FO_concrete_df_test_subset_3)
  for(i in 1:ncol(FO_concrete_df_test_subset_3)){
    #print(i)
    pred_temp<-heat(u=FO_concrete_df_test_subset_3[,i], alpha=alpha.range[x], 
                    xdelta=0.005089005, tdelta=difftime_concrete_3[i], n=2)
    FO_concrete_df_pred_3[,i+1]<-pred_temp[2,]
  }
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[-1,] #remove first row (invalid)
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[,-1] #remove first column (cannot be predicted)
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[,-dim(FO_concrete_df_pred_3)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_concrete_df_pred_3))
  alpha_rmse_3$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot(alpha_rmse_1$alpha, alpha_rmse_1$RMSE)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(alpha_rmse_3, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Concrete_test_subset_3_rmse_coarse_spectrum_full_plot.png",
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_3[40:80,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = "Concrete_test_subset_3_rmse_coarse_spectrum_subset_plot.png",
       width=297, height=210, units = "mm")
alpha_rmse_3$alpha[which.min(alpha_rmse_3$RMSE)] #5.59081e-08
min(alpha_rmse_3$RMSE) #0.07192516

#optimal alpha was 5.59081e-08
alpha.range<-seq(4.3*10^-8, 6.8*10^-8, by=0.01*10^-8)
#create output dataframe for alpha and RMSEs
alpha_rmse_3<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#run loop with narrow range of alpha
for(x in 1:length(alpha.range)){
  print(x)
  #create output dataframe for subset 1
  FO_concrete_df_pred_3<-setNames(data.frame(matrix(ncol = ncol(FO_concrete_df_test_subset_3), 
                                                    nrow = nrow(FO_concrete_df_test_subset_3))), 
                                  colnames(FO_concrete_df_test_subset_3))
  rownames(FO_concrete_df_pred_3)<-rownames(FO_concrete_df_test_subset_3)
  for(i in 1:ncol(FO_concrete_df_test_subset_3)){
    #print(i)
    pred_temp<-heat(u=FO_concrete_df_test_subset_3[,i], alpha=alpha.range[x], 
                    xdelta=0.005089005, tdelta=difftime_concrete_3[i], n=2)
    FO_concrete_df_pred_3[,i+1]<-pred_temp[2,]
  }
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[-1,] #remove first row (invalid)
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[,-1] #remove first column (cannot be predicted)
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[,-dim(FO_concrete_df_pred_3)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_concrete_df_pred_3))
  alpha_rmse_3$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

#plot results
ggplot(data=alpha_rmse_3)+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Concrete_test_subset_3_rmse_fine_spectrum_full_plot.png", 
       width=297, height=210, units = "mm")

ggplot(data=alpha_rmse_3[85:130,])+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename="Concrete_test_subset_3_rmse_fine_spectrum_subset_plot.png", 
       width=297, height=210, units = "mm")

alpha_rmse_3$alpha[which.min(alpha_rmse_3$RMSE)] #5.37e-08
min(alpha_rmse_3$RMSE) #0.07192419

#calculate mean optimal alpha
mean_alpha<-mean(c(9.46*10^-8,1.241*10^-7, 5.37*10^-8)) #9.08e-08
#####validate for day in last third of dataframe####
#subset hour 1
FO_concrete_df_validation_subset<-FO_concrete_df_validation[,10000:13600]
#create vector of measured data for RMSE calculation
FO_concrete_df_validation_subset_measured<-FO_concrete_df_validation_subset
FO_concrete_df_validation_subset_measured<-FO_concrete_df_validation_subset_measured[-1,] #remove first row (invalid)
FO_concrete_df_validation_subset_measured<-FO_concrete_df_validation_subset_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_concrete_df_validation_subset_measured))

#get time differences for subset 3
difftime_concrete_3<-as.vector(diff.POSIXt(as.POSIXct(colnames(FO_concrete_df_validation_subset))))

#create output dataframe for validation
  FO_concrete_df_pred_3<-setNames(data.frame(matrix(ncol = ncol(FO_concrete_df_validation_subset), 
                                                    nrow = nrow(FO_concrete_df_validation_subset))), 
                                  colnames(FO_concrete_df_validation_subset))
  rownames(FO_concrete_df_pred_3)<-rownames(FO_concrete_df_validation_subset)
  for(i in 1:ncol(FO_concrete_df_validation_subset)){
    #print(i)
    pred_temp<-heat(u=FO_concrete_df_validation_subset[,i], alpha=mean_alpha, 
                    xdelta=0.005089005, tdelta=difftime_concrete_3[i], n=2)
    FO_concrete_df_pred_3[,i+1]<-pred_temp[2,]
  }
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[-1,] #remove first row (invalid)
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[,-1] #remove first column (cannot be predicted)
  FO_concrete_df_pred_3<-FO_concrete_df_pred_3[,-dim(FO_concrete_df_pred_3)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_concrete_df_pred_3))
  RMSE_validation<-sqrt(mean((data_measured - data_predicted)^2)) #0.07632487
