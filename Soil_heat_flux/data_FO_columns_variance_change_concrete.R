#source script to load netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/data_FO_concrete_QAQC_plot.R")
beep()
#calculate diffusivity
library(cmna)
library(plyr)
library(bigsnpr)
library(dplyr)
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


#use 10 min rolling mean and then calculate variance to determine threshold between soil/atmosphere
library(zoo)
rollmean_hour = as.data.frame(lapply(FO_concrete_temp_time_df_order[,1:376], 
                                      function(x) rollmean(x, k = 25, fill = NA))) #10 min rolling mean 
vars_concrete<-data.frame("height"=as.numeric(colnames(FO_concrete_temp_time_df_order)[1:376]))
vars_concrete$var<-as.numeric(c(colwise(var, na.rm=T)(rollmean_hour)))
#plot
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(data=vars_concrete, aes(x=height, y=var))+
  geom_line()+
  theme_bw()+
  ylab(label="variance [°C]")+
  xlab(label="height [m]")+
  ggtitle(label="Soil-Atmosphere Threshold for Conrete")
ggsave(filename="Threshold_var_concrete.png")

plot(vars_concrete$height, vars_concrete$var, type="l")
vars_concrete$height[which.max(vars_concrete$var)]
#0.53 

#plot Variance over the whole timeseries
#calculate variance for every 10 mins 
var_hour = as.data.frame(lapply(FO_concrete_temp_time_df_order[,1:376], 
                                      function(x) aggregate(list(temp_var=x), 
                                                                 list(time=cut(FO_concrete_temp_time_df_order$time, "hour")), var)))
time<-var_hour$X0.time
var_hour_clean <- select(var_hour, -contains("time"))
var_hour_clean$time<-time
#colnames to height (numeric)
colnames(var_hour_clean)[1:376]<-as.numeric(colnames(FO_concrete_temp_time_df_order)[1:376])
#get into long fromat
var_hour_long<-gather(data = var_hour_clean, key, value, -time)
str(var_hour_long)
#transform class of vars to numeric
var_hour_long$key<-as.numeric(var_hour_long$key)
#time as posixct
var_hour_long$time<-as.POSIXct(var_hour_long$time)
#plot as heatmap
ggplot(var_hour_long, aes(time, key)) +
  geom_tile(aes(fill=value), height=0.1, color = NA) +
  scale_fill_viridis_c("Variance of Temp. [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  ggtitle(label="FO Column concrete")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_hourly_variance_concrete.png", width=297, height=210, units = "mm")
