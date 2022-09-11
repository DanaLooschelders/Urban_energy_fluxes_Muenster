#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/data_FO_columns_calc_threshold_grass.R")
#calculate diffusivity
library(cmna)
library(plyr)
library(bigsnpr)
library(dplyr)
library(ggplot2)

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


####prep data####

#use not aggregated data
FO_grass_temp_time<-vector(mode='list', length=length(files))
for(i in 1:length(FO_grass_only_temp)){
  print(i)
  #get starting datetime
  start_date<-as.POSIXct(names(FO_grass_only_temp)[i])
  #get time seq from starting time
  time<-seq.POSIXt(from=start_date, by= "24 sec", 
                   length.out=dim(FO_grass_only_temp[[i]])[1])
  #convert matrix to dataframe
  FO_grass_temp_time[[i]]<-as.data.frame(FO_grass_only_temp[[i]])
  #set colnames to correspond to height
  colnames(FO_grass_temp_time[[i]])<-FO_grass_list[[i]]$z
  #add time as variable
  FO_grass_temp_time[[i]]$time<-time
}
#rind list to one dataframe and fill missing cols with NA
FO_grass_temp_time_df<-rbind.fill(FO_grass_temp_time)
#order columns
FO_grass_temp_time_df_order<-FO_grass_temp_time_df[ ,order(colnames(FO_grass_temp_time_df))]
#height of measurements shiftet slightly during measurements
#if difference in heights is less than a threshold --> merge
#cols_to_keep<-which(diff(as.numeric(colnames(FO_grass_temp_time_df_order)[-length(FO_grass_temp_time_df_order)]))>0.006)
#FO_grass_temp_time_df_order_merged<-FO_grass_temp_time_df_order[,cols_to_keep]

#shorten dataframe to height below 80 cm to reducue computing time
FO_grass_temp_time_df_short<-FO_grass_temp_time_df_order[,1:354]
#if difference less than 0.006 -> merge
FO_grass_merged<-FO_grass_temp_time_df_short
#loop through every second column and compare with column after

for(i in seq(1, length(FO_grass_merged)-2, by=2)){
  print(i) #check
  #if difference smaller than 0.006
  if(diff(c(as.numeric(colnames(FO_grass_merged)[i]),
          as.numeric(colnames(FO_grass_merged)[i+1]))) <0.006){
    #merge those two columns and write result in first column
    FO_grass_merged[,i]<-coalesce(FO_grass_merged[,i], 
                                            FO_grass_merged[,i+1])
  #rename column to mean of the two columns
  colnames(FO_grass_merged)[i]<-as.character(mean(c(as.numeric(colnames(FO_grass_merged)[i]),
                                                               as.numeric(colnames(FO_grass_merged)[i+1]))))
  FO_grass_merged[,i+1]<-NA #set second column to NA
  }else{} #do nothing
}

threshold_grass<-0.5986373 #0.5986373 (median value)
#get index of columns over threshold
cols<-which(as.numeric(colnames(FO_grass_merged))>=threshold_grass)
#remove those columns
FO_grass_merged_short<-FO_grass_merged[,-cols]

#drop all columns that are only NA (the ones that were merged previously)
FO_grass_merged_short_cut<-FO_grass_merged_short[colSums(!is.na(FO_grass_merged_short)) > 0]
#QAQC
#check that values are NA
length(which(is.na(FO_grass_merged_short_cut)))

#rename for convenience
FO_grass_df<-FO_grass_merged_short_cut
#get spatial difference of measurements
heights_grass<-diff(as.numeric(colnames(FO_grass_df)))
mean(heights_grass) # 0.0051
sd(heights_grass) #0.00028

#clear up environment
rm(FO_grass_temp_time, FO_grass_list, FO_grass_merged, 
   FO_grass_merged_short, FO_grass_merged_short_cut, FO_grass_temp_time_df, 
   FO_grass_temp_time_df_short, FO_grass_temp_time_df_order_merged)


#transpose dataframe
FO_grass_df_t<-as.data.frame(t(FO_grass_df))
colnames(FO_grass_df_t)<-FO_grass_temp_time_df_order$time #set time as colnames
#split data in test and validation data
#2/3 test and 1/3 Validation
#subset test
FO_grass_df_test<-FO_grass_df_t[,1:round(length(FO_grass_df_t)/3*2, 0)]
#choose only 2 1 day periods from test data.frame to reduce computational cost
60*60*24/24 #3600 Files sind 1 tag
#subset day 1
FO_grass_df_test_subset_1<-FO_grass_df_test[,10000:13600]
range(FO_grass_temp_time_df_order$time[10000:13600]) #timespan subset 1
#get time differences for subset 1
difftime_grass_1<-as.vector(diff.POSIXt(FO_grass_temp_time_df_order$time[10000:13600]))

#subset validation
FO_grass_df_validation<-FO_grass_df_t[,1:round(length(FO_grass_df_t)/3, 0)]

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
                    xdelta=heights_grass, tdelta=difftime_grass_1[i], n=2)
    FO_grass_df_pred_1[,i+1]<-pred_temp[2,]
  }
  FO_grass_df_pred_1<-FO_grass_df_pred_1[-1,] #remove first row (invalid)
  FO_grass_df_pred_1<-FO_grass_df_pred_1[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_1<-FO_grass_df_pred_1[,-dim(FO_grass_df_pred_1)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_1))
  alpha_rmse_1$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}


plot(alpha_rmse_1$alpha, alpha_rmse_1$RMSE)
alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] #for a day 1.747528e-07
min(alpha_rmse_1$RMSE) #for a day  0.1372653

#optimal alpha was 1.747528e-07
alpha.range<-seq(1*10^-7, 2*10^-7, by=0.01*10^-7)
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
                    xdelta=heights_grass, tdelta=difftime_grass_1[i], n=2)
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

alpha_rmse_1$alpha[which.min(alpha_rmse_1$RMSE)] #for one day 1.56e-07
min(alpha_rmse_1$RMSE) #for one day 0.1372462


#####try for second subset####
#subset hour 2
FO_grass_df_test_subset_2<-FO_grass_df_test[,40000:43600]
range(FO_grass_temp_time_df_order$time[40000:43600]) #timespan subset 2

#get time differences for subset 1
difftime_grass_2<-as.vector(diff.POSIXt(FO_grass_temp_time_df_order$time[40000:43600]))
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
                    xdelta=heights_grass, tdelta=difftime_grass_2[i], n=2)
    FO_grass_df_pred_2[,i+1]<-pred_temp[2,]
  }
  FO_grass_df_pred_2<-FO_grass_df_pred_2[-1,] #remove first row (invalid)
  FO_grass_df_pred_2<-FO_grass_df_pred_2[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_2<-FO_grass_df_pred_2[,-dim(FO_grass_df_pred_2)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_2))
  alpha_rmse_2$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}

plot(alpha_rmse_2$alpha, alpha_rmse_2$RMSE)
alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #for a day 1e-07
min(alpha_rmse_2$RMSE) #for a day 0.1387557

#optimal alpha was for a day 1e-07
alpha.range<-seq(8*10^-8, 2*10^-7, by=0.01*10^-7)
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
                    xdelta=heights_grass, tdelta=difftime_grass_2[i], n=2)
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

alpha_rmse_2$alpha[which.min(alpha_rmse_2$RMSE)] #for a day  1.14e-07
min(alpha_rmse_2$RMSE) #for a day 0.1387406

mean_alpha<-mean(c(1.56*10^-7, 1.14*10^-7))

#####validate for day in last third of dataframe####
#subset hour 1
FO_grass_df_validation_subset<-FO_grass_df_validation[,10000:13600]
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
  pred_temp<-heat(u=FO_grass_df_validation_subset[,i], alpha=mean_alpha, 
                  xdelta=0.005089005, tdelta=difftime_grass_3[i], n=2)
  FO_grass_df_pred_3[,i+1]<-pred_temp[2,]
}
FO_grass_df_pred_3<-FO_grass_df_pred_3[-1,] #remove first row (invalid)
FO_grass_df_pred_3<-FO_grass_df_pred_3[,-1] #remove first column (cannot be predicted)
FO_grass_df_pred_3<-FO_grass_df_pred_3[,-dim(FO_grass_df_pred_3)[2]] #remove last column -> no measured values
data_predicted<-as.vector(t(FO_grass_df_pred_3))
RMSE_validation<-sqrt(mean((data_measured - data_predicted)^2)) #0.1258283