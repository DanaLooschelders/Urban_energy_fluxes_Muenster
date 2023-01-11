#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/data_FO_concrete_QAQC_plot.R")
beep()
#calculate diffusivity
library(cmna)
library(plyr)
library(bigsnpr)
library(colorspace)
library(scico)

#####heat function####
    #u = initial values of u
    #alpha = thermal diffusivity
    #xdelta = change in x (space) at each step in u
    #tdelta = time step
    #n = number of steps to take

####prep data####
#extract only first 15 cm
#0 - 20 cm
FO_concrete_layer<-FO_concrete_10min[,64:105]
#0 - 10 cm --> FO_concrete_layer<-FO_concrete_df[,85:105]
layer_name<-"0_20cm"
#choose soil layer
#aggregate Data to 10 min
FO_concrete_layer  
#save aggregated and cut data as csv
FO_concretete_csv<-cbind(FO_concrete_layer, concrete_time)
getwd()
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
write.csv(FO_concretete_csv, file="FO_concrete_20cm.csv", row.names=F)
#transpose dataframe
FO_concrete_df_t<-as.data.frame(t(FO_concrete_layer))
colnames(FO_concrete_df_t)<-concrete_time #set time as colnames
#split data in test and validation data
#2/3 test and 1/3 Validation
#subset test

FO_concrete_df_test<-FO_concrete_df_t[,1:round(length(FO_concrete_df_t)/3*2, 0)]
#choose only 3 1 day periods from test data.frame to reduce computational cost
#60*60*24/24 #3600 Files sind 1 tag
#subset day 1 - find cols with chosen period
range_test_1<-range(which(colnames(FO_concrete_df_test)>"2021-07-30 08:00:00 CEST"&colnames(FO_concrete_df_test)<"2021-07-31 08:00:00 CEST"))

FO_concrete_df_test_subset_1<-FO_concrete_df_test[,range_test_1[1]:range_test_1[2]]
#range(FO_concrete_temp_time_df$time[10000:13600]) #timespan subset 1
#get time differences for subset 1 and multipy times sixty to get seconds
difftime_concrete_1<-as.vector(diff.POSIXt(concrete_time[range_test_1[1]:range_test_1[2]]))*60
if(mean(difftime_concrete_1)!=600){
  print("Error!!! check times")
}else{}

#subset validation
FO_concrete_df_validation<-FO_concrete_df_t[,round(length(FO_concrete_df_t)/3*2, 0):length(FO_concrete_df_t)]
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
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
ggplot(alpha_rmse_1, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = paste("Concrete_test_subset_1", layer_name, "rmse_coarse_spectrum_full_plot.png", sep="_"),
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_1[40:85,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = paste("Concrete_test_subset_1", layer_name, "rmse_coarse_spectrum_subset_plot.png", sep="_"),
       width=297, height=210, units = "mm")
alpha_1_1 <- alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] # 
rmse_1_1 <- min(alpha_rmse_1$RMSE) #

#define new threshold
lower<-alpha_1_1-(alpha_1_1*30/100)
upper<-alpha_1_1+(alpha_1_1*30/100)
by_unit<-substr(lower, nchar(lower)-3, stop=nchar(lower))
#optimal alpha was 7.564633e-08
alpha.range<-seq(lower, upper, by=as.numeric(paste(0.01, by_unit, sep="")))
range(alpha.range)
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
ggsave(filename=paste("Concrete_test_subset_1", layer_name, "rmse_fine_spectrum_full_plot.png", sep="_"), 
       width=297, height=210, units = "mm")

ggplot(data=alpha_rmse_1[85:140,])+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename=paste("Concrete_test_subset_1", layer_name, "rmse_fine_spectrum_subset_plot.png", sep="_"),
       width=297, height=210, units = "mm")

alpha_1_2 <- alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] #3.14e-08
rmse_1_2 <- min(alpha_rmse_1$RMSE) # 0.1676387


#####try for second subset####
#subset hour 2
range_test_2<-range(which(colnames(FO_concrete_df_test)>"2021-08-04 00:00:00 CEST"&colnames(FO_concrete_df_test)<"2021-08-05 00:00:00 CEST"))

FO_concrete_df_test_subset_2<-FO_concrete_df_test[,range_test_2[1]:range_test_2[2]]


#range(FO_concrete_temp_time_df$time[40000:43600]) #timespan subset 2

#get time differences for subset 1
difftime_concrete_2<-as.vector(diff.POSIXt(concrete_time[range_test_2[1]:range_test_2[2]]))*60
if(mean(difftime_concrete_2)!=600){
  print("Error!!! check times")
}else{}
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
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
ggplot(alpha_rmse_2, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = paste("Concrete_test_subset_2", layer_name,"rmse_coarse_spectrum_full_plot.png", sep="_"),
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_2[40:65,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = paste("Concrete_test_subset_2", layer_name, "rmse_coarse_spectrum_subset_plot.png", sep="_"),
       width=297, height=210, units = "mm")

alpha_2_1 <- alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #2.205131e-08
rmse_2_1 <- min(alpha_rmse_2$RMSE) #0.19263

#define new threshold
lower<-alpha_2_1-(alpha_2_1*30/100)
upper<-alpha_2_1+(alpha_2_1*30/100)
by_unit<-substr(lower, nchar(lower)-3, stop=nchar(lower))
#optimal alpha was 

alpha.range<-seq(lower, upper, by=as.numeric(paste(0.01, by_unit, sep="")))
range(alpha.range)
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
ggsave(filename=paste("Concrete_test_subset_2", layer_name, "rmse_fine_spectrum_full_plot.png", sep="_"),
       width=297, height=210, units = "mm")

ggplot(data=alpha_rmse_2[40:70,])+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename=paste("Concrete_test_subset_2", layer_name, "rmse_fine_spectrum_subset_plot.png", sep="_"),
       width=297, height=210, units = "mm")

alpha_2_2 <- alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #2.24e-08
rmse_2_2 <- min(alpha_rmse_2$RMSE) # 0.19263

####run  loop for subset 3 ####
#subset day 3
range_test_3<-range(which(colnames(FO_concrete_df_test)>"2021-08-11 00:00:00 CEST"&colnames(FO_concrete_df_test)<"2021-08-12 00:00:00 CEST"))

FO_concrete_df_test_subset_3<-FO_concrete_df_test[,range_test_3[1]:range_test_3[2]]
#range(FO_concrete_temp_time_df$time[44000:47600]) #timespan subset 1
#get time differences for subset 1
difftime_concrete_3<-as.vector(diff.POSIXt(concrete_time[range_test_3[1]:range_test_3[2]]))*60
if(mean(difftime_concrete_1)!=600){
  print("Error!!! check times")
}else{}

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
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
ggplot(alpha_rmse_3, aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = paste("Concrete_test_subset_3", layer_name, "rmse_coarse_spectrum_full_plot.png", sep="_"),
       width=297, height=210, units = "mm")

ggplot(alpha_rmse_3[40:70,], aes(x=alpha, y=RMSE))+
  geom_point()+
  theme_bw()+
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')
ggsave(filename = paste("Concrete_test_subset_3", layer_name,"rmse_coarse_spectrum_subset_plot.png", sep="_"),
       width=297, height=210, units = "mm")

alpha_3_1 <- alpha_rmse_3$alpha[which.min(alpha_rmse_3$RMSE)] #2.009233e-08
rmse_3_1 <- min(alpha_rmse_3$RMSE) # 0.1522922

#define new threshold
lower<-alpha_3_1-(alpha_3_1*30/100)
upper<-alpha_3_1+(alpha_3_1*30/100)
by_unit<-substr(lower, nchar(lower)-3, stop=nchar(lower))
#optimal alpha was 7.564633e-08
alpha.range<-seq(lower, upper, by=as.numeric(paste(0.01, by_unit, sep="")))
range(alpha.range)
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
ggsave(filename=paste("Concrete_test_subset_3", layer_name, "rmse_fine_spectrum_full_plot.png", sep="_"), 
       width=297, height=210, units = "mm")

ggplot(data=alpha_rmse_3[40:70,])+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()
ggsave(filename=paste("Concrete_test_subset_3", layer_name, "rmse_fine_spectrum_subset_plot.png", sep="_"), 
       width=297, height=210, units = "mm")

alpha_3_2 <- alpha_rmse_3$alpha[which.min(alpha_rmse_3$RMSE)] #1.98e-08
rmse_3_2 <- min(alpha_rmse_3$RMSE) #0.1522921

#calculate mean optimal alpha
mean_alpha <- mean(c(alpha_1_2,alpha_2_2, alpha_3_2)) #2.453333e-08
sd_alpha <- sd(c(alpha_1_2,alpha_2_2, alpha_3_2)) #6.087145e-09
#####validate for day in last third of dataframe####
range_validation<-range(which(colnames(FO_concrete_df_validation)>"2021-08-14 16:00:00 CEST"&colnames(FO_concrete_df_validation)<"2021-08-15 16:00:00 CEST"))

FO_concrete_df_validation_subset<-FO_concrete_df_validation[,range_validation[1]:range_validation[2]]
#subset hour 1
#FO_concrete_df_validation_subset<-FO_concrete_df_validation[,10000:13600]
#create vector of measured data for RMSE calculation
FO_concrete_df_validation_subset_measured<-FO_concrete_df_validation_subset
FO_concrete_df_validation_subset_measured<-FO_concrete_df_validation_subset_measured[-1,] #remove first row (invalid)
FO_concrete_df_validation_subset_measured<-FO_concrete_df_validation_subset_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_concrete_df_validation_subset_measured))

#get time differences for svalidation
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
  rmse_validation<-sqrt(mean((data_measured - data_predicted)^2)) #0.2008705
  
  result<-data.frame("alpha"=c(alpha_1_1,
                               alpha_1_2,
                               alpha_2_1,
                               alpha_2_2,
                               alpha_3_1,
                               alpha_3_2,
                               mean_alpha,
                               sd_alpha),
                     "rmse"=c(rmse_1_1,
                              rmse_1_2,
                              rmse_2_1,
                              rmse_2_2,
                              rmse_3_1,
                              rmse_3_2,
                              rmse_validation,
                              NA))

  getwd()
  ####plot####
  plotTestSubset<- function(subsetName){
    #prepare data
    FO_concrete_to_melt<-subsetName
    FO_concrete_to_melt$ID<-as.factor(round(as.numeric(rownames(subsetName)), 3))
    FO_concrete_melted = reshape2::melt(FO_concrete_to_melt, id.vars = "ID")
    #plot
    ggplot(data=FO_concrete_melted)+
      geom_line(aes(x=as.POSIXct(variable), y=value, col=ID))+
      theme_bw()+
      xlab(label="time")+
      ylab(label="Temperature [°C]")+
      scale_color_discrete_sequential(palette= "Teal")
  }
  
  #test 1
  plotTestSubset(FO_concrete_df_test_subset_1)
  ggsave(filename=paste("Temp_concrete_testsubset_1", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
  #test 2
  plotTestSubset(FO_concrete_df_test_subset_2)
  ggsave(filename=paste("Temp_concrete_testsubset_2", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
  #test 3
  plotTestSubset(FO_concrete_df_test_subset_3)
  ggsave(filename=paste("Temp_concrete_testsubset_3", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
  #validation
  plotTestSubset(FO_concrete_df_validation_subset)
  ggsave(filename=paste("Temp_concrete_validationsubset", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
  #whole data
  plotTestSubset(FO_concrete_df_t)
  ggsave(filename=paste("Temp_concrete", layer_name, "cm.png", sep="_"), width=297, height=210, units = "mm")
  

  