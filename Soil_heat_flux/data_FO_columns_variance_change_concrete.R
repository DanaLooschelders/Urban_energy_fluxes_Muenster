#source script to load netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/data_FO_concrete_QAQC_plot.R")
beep()
#calculate diffusivity
library(cmna)
library(plyr)
FO_grass_list[["FO-column-grass_final_20210729-1400_unheated_PVC.nc"]][["LAF"]]
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
#0.5341675


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

#calculate variance for individual days
#create date column
FO_concrete_temp_time_df_order$date<-date(FO_concrete_temp_time_df_order$time)
#create output dataframe
daily_var<-data.frame("date"=unique(FO_concrete_temp_time_df_order$date), "maxvar"=NA)
#loop through days
for(i in unique(FO_concrete_temp_time_df_order$date)){
  #calculate rolling mean per hour
rollmean_hour_temp = as.data.frame(lapply(FO_concrete_temp_time_df_order[FO_concrete_temp_time_df_order$date==i,1:376], 
                                     function(x) rollmean(x, k = 25, fill = NA))) #10 min rolling mean 
vars_concrete_temp<-data.frame("height"=as.numeric(colnames(FO_concrete_temp_time_df_order)[1:376]))
#calculate variance over whole time
vars_concrete_temp$var<-as.numeric(c(colwise(var, na.rm=T)(rollmean_hour_temp)))
#write into output dataframe
daily_var$maxvar[daily_var$date==i]<-vars_concrete_temp$height[which.max(vars_concrete_temp$var)]
}

#get rain data
dat.rain.merge$date<-date(dat.rain.merge$TIMESTAMP)
rain_daily<-aggregate(Rain_mm_Tot~date, FUN=sum, data=dat.rain.merge)  

daily_var<-left_join(rain_daily, daily_var, by="date")
daily_var<-daily_var[,c(1,3)]
daily_var$index<-"variance"
colnames(daily_var)[2]<-"value"
rain_daily$index<-"rain"
colnames(rain_daily)[2]<-"value"
rainvar<-rbind(rain_daily, daily_var)
#plot variance over days
ggplot(data=rainvar)+
  geom_line(aes(x=date, y=value))+
  theme_bw()+
  #geom_hline(aes(yintercept=0.5341675), col="red")+
  facet_grid(rows=vars(index), scales = "free_y")

i=unique(FO_concrete_temp_time_df_order$date)[2]
#calcualte daily variance over whole time
daily_var_simple<-data.frame("date"=unique(rain_daily$date), "maxvar"=NA)
#loop through days
for(i in unique(FO_concrete_temp_time_df_order$date)){
  #create data frame
  vars_concrete_temp<-data.frame("height"=as.numeric(colnames(FO_concrete_temp_time_df_order)[1:376]))
  #calculate variance over whole time for every day
  vars_concrete_temp$var <- c(unlist(lapply(FO_concrete_temp_time_df_order[FO_concrete_temp_time_df_order$date==i,1:376], 
                                            function(x) var(x)))) #10 min rolling mean 
  #write into output dataframe
  daily_var_simple$maxvar[daily_var_simple$date==i]<-vars_concrete_temp$height[which.max(vars_concrete_temp$var)]
}

var_simple<-ggplot(daily_var_simple)+
  geom_point(aes(x=as.factor(date), y=maxvar), size=5)+
  theme_bw()+
  ylab("Height of \nmax Var [m]")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  geom_hline(aes(yintercept=0.5341675), col="red", linetype="dotted")

rain<-ggplot(rain_daily)+
  geom_bar(aes(x=as.factor(date), y=value), stat="identity")+
  ylab(label="Rain [l/m^2]")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

var_simple+rain+plot_layout(nrow=2)

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="daily_rain_maxvar.png", width=297, height=210, units = "mm")
