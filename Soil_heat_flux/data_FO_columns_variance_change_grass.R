#source script to load netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/data_FO_grass_QAQC_plot.R")
beep()
#calculate diffusivity
library(cmna)
library(plyr)
library(bigsnpr)
library(dplyr)
library(tidyverse)
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
#FO_grass_temp_time_df_short<-FO_grass_temp_time_df_order[,1:354]
#if difference less than 0.006 -> merge
FO_grass_merged<-FO_grass_temp_time_df_order
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

#threshold_grass<-0.5986373 #0.5986373 (median value)
#get index of columns over threshold
#cols<-which(as.numeric(colnames(FO_grass_merged))>=threshold_grass)
#remove those columns
#FO_grass_merged_short<-FO_grass_merged[,-cols]

#drop all columns that are only NA (the ones that were merged previously)
FO_grass_merged_cut<-FO_grass_merged[colSums(!is.na(FO_grass_merged)) > 0]
#QAQC
#check that values are NA
length(which(is.na(FO_grass_merged_cut)))

#use 10 min rolling mean and then calculate variance to determine threshold between soil/atmosphere
library(zoo)
rollmean_10min = as.data.frame(lapply(FO_grass_merged_cut[,1:327], 
                                      function(x) rollmean(x, k = 25, fill = NA))) #10 min rolling mean 
vars_grass<-data.frame("height"=as.numeric(colnames(FO_grass_merged_cut)[1:327]))
vars_grass$var<-as.numeric(c(colwise(var, na.rm=T)(rollmean_10min)))


#plot
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(data=vars_grass[80:150,], aes(x=height, y=var))+
  geom_line()+
  theme_bw()+
  ylab(label="variance [°C]")+
  xlab(label="height [m]")+
  ggtitle(label="Soil-Atmosphere Threshold for Gras")
ggsave(filename="Threshold_var_subset_grass.png")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(data=vars_grass, aes(x=height, y=var))+
  geom_line()+
  theme_bw()+
  ylab(label="variance [°C]")+
  xlab(label="height [m]")+
  ggtitle(label="Soil-Atmosphere Threshold for Gras")
ggsave(filename="Threshold_var_all_grass.png")

plot(vars_grass$height, vars_grass$var, type="l")

#get first peak (probably soil/vegetation boundary)
vars_grass$height[1:100][which.max(vars_grass$var[1:100])]
#0.47
#get second peak (probably vegetation/atmosphere boundary)
vars_grass$height[100:150][which.max(vars_grass$var[100:150])]
#0.533174

#plot Variance over the whole timeseries
#calculate variance for every 10 mins 
var_hour_grass = as.data.frame(lapply(FO_grass_temp_time_df_order[,1:636], 
                                function(x) aggregate(list(temp_var=x), 
                                                      list(time=cut(FO_grass_temp_time_df_order$time, "hour")), var)))
time_grass<-var_hour_grass$X0.time
var_hour_grass_clean <- select(var_hour_grass, -contains("time"))
var_hour_grass_clean$time<-time_grass
#colnames to height (numeric)
colnames(var_hour_grass_clean)[1:636]<-as.numeric(colnames(FO_grass_temp_time_df_order)[1:636])
#get into long fromat
var_hour_grass_long<-gather(data = var_hour_grass_clean, key, value, -time)
str(var_hour_grass_long)
#transform class of vars to numeric
var_hour_grass_long$key<-as.numeric(var_hour_grass_long$key)
#time as posixct
var_hour_grass_long$time<-as.POSIXct(var_hour_grass_long$time)
#plot as heatmap
ggplot(var_hour_grass_long, aes(time, key)) +
  geom_tile(aes(fill=value), height=1.5, color = NA) +
  scale_fill_viridis_c("Variance of Temp. [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  ggtitle(label="FO Column grass")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_hourly_variance_grass.png", width=297, height=210, units = "mm")
