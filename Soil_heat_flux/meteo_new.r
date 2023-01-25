#read in meteo data for sample day
meteo_all<-read.csv("meteo_sample_day.csv")
meteo_sub<-meteo_all[meteo_all$TIMESTAMP>="2021-08-04 07:00:00"&
                       meteo_all$TIMESTAMP<="2021-08-04 09:30:00",]
#reshape
meteo_all_long <- meteo_all %>%                             
  gather(variable, value, -c(TIMESTAMP))
#reshape
meteo_sub_long <- meteo_sub %>%                             
  gather(variable, value, -c(TIMESTAMP))

colnames(meteo_all)
str(meteo_all_long)
meteo_all_long$TIMESTAMP<-as.POSIXct(meteo_all_long$TIMESTAMP)
meteo_sub_long$TIMESTAMP<-as.POSIXct(meteo_sub_long$TIMESTAMP)

#plot ind var
plot_meteo(meteo_sub_long,"AirTC_Avg_beton")
plot_meteo(meteo_sub_long,"SUp_Avg_kiebitz")
plot_meteo(meteo_sub_long, "Rain_mm_Tot")
