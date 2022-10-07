#source script to load netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/data_FO_columns_calc_threshold_concrete.R")
#calculate diffusivity
library(cmna)
library(plyr)
library(bigsnpr)

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
rollmean_10min = as.data.frame(lapply(FO_concrete_temp_time_df_order[,1:376], 
                                      function(x) rollmean(x, k = 25, fill = NA))) #10 min rolling mean 
vars_concrete<-data.frame("height"=as.numeric(colnames(FO_concrete_temp_time_df_order)[1:376]))
vars_concrete$var<-as.numeric(c(colwise(var, na.rm=T)(rollmean_10min)))
#plot
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(data=vars_concrete, aes(x=height, y=var))+
  geom_line()+
  theme_bw()+
  xlab(label="variance [°C]")+
  ylab(label="height [m]")+
  ggtitle(label="Soil-Atmosphere Threshold for Conrete")
ggsave(filename="Threshold_var_concrete.png")

plot(vars_concrete$height, vars_concrete$var, type="l")
vars_concrete$height[which.max(vars_concrete$var)]
#0.53