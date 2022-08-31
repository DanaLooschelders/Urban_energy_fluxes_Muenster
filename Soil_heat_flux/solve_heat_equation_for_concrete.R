#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/read_netcfd_files_FO_columns.R")
#calculate diffusivity
library(cmna)
library(plyr)
library(bigsnpr)

####concrete####
#use not aggregated data
FO_concrete_temp_time<-vector(mode='list', length=length(files))
for(i in 1:length(FO_concrete_only_temp)){
  print(i)
  #get starting datetime
  start_date<-as.POSIXct(names(FO_concrete_only_temp)[i])
  #get time seq from starting time
  time<-seq.POSIXt(from=start_date, by= "24 sec", 
                   length.out=dim(FO_concrete_only_temp[[i]])[1])
  #convert matrix to dataframe
  FO_concrete_temp_time[[i]]<-as.data.frame(FO_concrete_only_temp[[i]])
  #set colnames to correspond to height
  colnames(FO_concrete_temp_time[[i]])<-FO_concrete_list[[i]]$z
  #add time as variable
  FO_concrete_temp_time[[i]]$time<-time
}
#rind list to one dataframe and fill missing cols with NA
FO_concrete_temp_time_df<-rbind.fill(FO_concrete_temp_time)
#order columns
FO_concrete_temp_time_df_order<-FO_concrete_temp_time_df[ ,order(colnames(FO_concrete_temp_time_df))]
threshold_concrete<-0.6846209  #0.6846209 (median value)
#get index of columns over threshold
cols<-which(as.numeric(colnames(FO_concrete_temp_time_df_order[, -length(FO_concrete_temp_time_df_order)]))>=threshold_concrete)
#remove those columns
FO_concrete_temp_time_df_short<-FO_concrete_temp_time_df_order[,-cols]
#rename for convenience
FO_concrete_df<-FO_concrete_temp_time_df_short
#get spatial difference of measurements
heights_concrete<-diff(as.numeric(colnames(FO_concrete_df[-length(FO_concrete_df)])))

#remove column with time
FO_concrete_df<-FO_concrete_df[,-length(FO_concrete_df)]

#clear up environment
#rm(FO_concrete_temp_time, FO_concrete_list, FO_concrete_only_temp, FO_concrete_temp_time_df_order)


#####heat function####
    #u = initial values of u
    #alpha = thermal diffusivity
    #xdelta = change in x (space) at each step in u
    #tdelta = time step
    #n = number of steps to take

#change original heat function to use a vector of heights and times
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
#transpose dataframe
FO_concrete_df_t<-as.data.frame(t(FO_concrete_df))
colnames(FO_concrete_df_t)<-FO_concrete_temp_time_df$time #set time as colnames
#split data in test and validation data
#2/3 test and 1/3 Validation
#subset test
FO_concrete_df_test<-FO_concrete_df_t[,1:round(length(FO_concrete_df_t)/3*2, 0)]
#choose only 2 1 day periods from test data.frame to reduce computational cost
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
alpha.range<-seq(1*10^-50, 11*10^-7, by=0.1*10^-7)
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


plot(alpha_rmse_1$alpha, alpha_rmse_1$RMSE)
alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] #for an hour 2.4e-07 and for a day 8.111308e-08
min(alpha_rmse_1$RMSE) #for an hour 0.07550493 and for a day  0.1258294

#optimal alpha was 8.111308e-08
alpha.range<-seq(7*10^-8, 9*10^-8, by=0.01*10^-8)
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

alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] #for one hour 2.38e-07 for one day 8.5e-08
min(alpha_rmse_1$RMSE) #for one hour 0.07550471 for one day 0.125828


#####try for second subset####
#subset hour 2
FO_concrete_df_test_subset_2<-FO_concrete_df_test[,40000:43600]
range(FO_concrete_temp_time_df$time[40000:43600]) #timespan subset 2

#get time differences for subset 1
difftime_concrete_2<-as.vector(diff.POSIXt(FO_concrete_temp_time_df$time[40000:43600]))
#run heat function for every time step
#Effects-of-aggregate-types-on-thermal-properties-of-concrete (2012)
alpha.range<-seq(1*10^-8, 11*10^-7, by=0.1*10^-7)
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

plot(alpha_rmse_2$alpha, alpha_rmse_2$RMSE)
alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #for an hour 2.4e-07 for a day 8.902151e-08
min(alpha_rmse_2$RMSE) #for an hour 0.07550493 for a day 0.1242301

#optimal alpha was 2.4e-7  for a day 8.902151e-08
alpha.range<-seq(7.9*10^-8, 9.5*10^-8, by=0.01*10^-8)
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
ggplot(data=alpha_rmse_2)+
  geom_point(aes(x=alpha, y=RMSE))+
  theme_bw()

alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #2.38e-07  for a day 8.902151e-08
min(alpha_rmse_2$RMSE) #for an hour 0.07550471 for a day 0.1242301

mean_alpha<-mean(c(8.5*10^-8, 8.9*10^-8))

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
  RMSE_validation<-sqrt(mean((data_measured - data_predicted)^2)) #0.1258283

####plot first prediction try####
#reshape into long format for plotting
FO_concrete_df_pred_t<-data.frame(t(FO_concrete_df_pred))
colnames(FO_concrete_df_pred_t)<-colnames(FO_concrete_df) #set proper colnames
FO_concrete_df_pred_t<-FO_concrete_df_pred_t[-c(dim(FO_concrete_df_pred_t)[1]),] #drop last empty row
FO_concrete_df_pred_t$time<-FO_concrete_temp_time_df_short$time #add time as var
FO_concrete_df_pred_t$time<-as.POSIXct(FO_concrete_df_pred_t$time) #reformat time
FO_concrete_df_pred_long<-gather(data = FO_concrete_df_pred_t, key, value, -time) #get into long format

#transform class of vars to numeric
FO_concrete_df_pred_long$key<-as.numeric(FO_concrete_df_pred_long$key)
FO_concrete_df_pred_long$value<-as.numeric(FO_concrete_df_pred_long$value)

#plot as heatmap
ggplot(FO_concrete_df_pred_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  #geom_hline(aes(yintercept=threshold_concrete, col="Boundary"))+
  scale_color_manual(values = c("black"))+
  theme_bw()+
  ggtitle(label="FO Column Concrete Prediction")

#prüfen hinreichend/notwenig --> bei Tiefpunktberechnung rms für alpha
