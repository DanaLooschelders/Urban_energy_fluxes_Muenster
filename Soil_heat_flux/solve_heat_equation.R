#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/read_netcfd_files_FO_columns.R")
#calculate diffusivity
library(cmna)
library(plyr)
####Grass####
i=1
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
#get index of columns over threshold
cols<-which(as.numeric(colnames(FO_grass_temp_time_df_order[, -length(FO_grass_temp_time_df_order)]))>=threshold_grass)
#remove those columns
FO_grass_temp_time_df_short<-FO_grass_temp_time_df_order[,-cols]
#rename for convenience
FO_grass_df<-FO_concrete_temp_time_df_short
#get spatial difference of measurements
heights_grass<-diff(as.numeric(colnames(FO_grass_temp_time_df_short[-length(FO_grass_temp_time_df_short)])))
difftime_grass<-diff.POSIXt(FO_grass_df$time)
#remove column with time
FO_grass_df<-FO_grass_df[,-length(FO_grass_df)]

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
#get index of columns over threshold
cols<-which(as.numeric(colnames(FO_concrete_temp_time_df_order[, -length(FO_concrete_temp_time_df_order)]))>=threshold_concrete)
#remove those columns
FO_concrete_temp_time_df_short<-FO_concrete_temp_time_df_order[,-cols]
#rename for convenience
FO_concrete_df<-FO_concrete_temp_time_df_short
#get spatial difference of measurements
heights_concrete<-diff(as.numeric(colnames(FO_concrete_df[-length(FO_concrete_df)])))
difftime_conrete<-as.vector(diff.POSIXt(FO_concrete_df$time))
#remove column with time
FO_concrete_df<-FO_concrete_df[,-length(FO_concrete_df)]

#####heat function####
    #u = initial values of u
    #alpha = thermal diffusivity
    #xdelta = change in x (space) at each step in u
    #tdelta = time step
    #n = number of steps to take
#original function
heat <- function (u, alpha , xdelta , tdelta , n) {
  m <- length (u)
  uarray <- matrix (u, nrow = 1)
  newu <- u
  for(i in 1:n) {
    for(j in 2:(m - 1)) {
      h <- alpha * tdelta / xdelta ^2 
      ustep <- (u[j - 1] + u[j + 1] - 2 * u[j])
      newu [j] <- u[j] + h * ustep
    }
    u <- newu
    u[1] <- u[m]
    uarray <- rbind (uarray , u)
  }
  return ( uarray )
}

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
FO_concrete_df_t<-t(FO_concrete_df)
#create output dataframe
FO_concrete_df_pred<-setNames(data.frame(matrix(ncol = ncol(FO_concrete_df), 
                                                nrow = nrow(FO_concrete_df))), 
                              colnames(FO_concrete_df))

#run heat function for every time step
for(i in 1:nrow(FO_concrete_df)){
pred_temp<-heat(u=as.vector(FO_concrete_df[1,]), alpha=0.5*10^-6, 
     xdelta=0.005089005, tdelta=difftime_conrete[i], n=2)
FO_concrete_df_pred[i+1,]<-pred_temp
}

test<-as.data.frame(test)
test[1,]==df_grass_soil[,1]

15+17-(2*16)
13+17-(2*16)
15+18-(2*16)

#hinreichend/notwenig --> bei Tiefpunktberechnung
