#meteorology for sample day
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R") 
#subset 2: "2021-08-10 13:05:12 CEST"  to "2021-08-11 13:05:36 CEST"
starttime_2 <- as.POSIXct("2021-08-04 00:00:00 CEST")
endtime_2 <- as.POSIXct("2021-08-05 00:00:00 CEST")
#subset to 2
meteo_2<-dat.meteo.merge[dat.meteo.merge$TIMESTAMP>=starttime_2&dat.meteo.merge$TIMESTAMP<=endtime_2,]
rain_2<-dat.rain.merge[dat.rain.merge$TIMESTAMP>=starttime_2&dat.rain.merge$TIMESTAMP<=endtime_2,]
soil_2<-dat.soil.merge[dat.soil.merge$TIMESTAMP>=starttime_2&dat.soil.merge$TIMESTAMP<=endtime_2,]
####meteo###
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Meteo")
#meteo
#convert timestamp
meteo_2$TIMESTAMP<-as.POSIXct(meteo_2$TIMESTAMP)
#reduce unesseccary variables
meteo_2<-meteo_2[,!names(meteo_2) %in% c("RECORD_beton", "RECORD_kiebitz", 
                                         "CNR4TC_Avg_beton", "CNR4TC_Avg_kiebitz", 
                                         "Albedo_Avg_beton", "Albedo_Avg_kiebitz", 
                                         "TotRNet_Avg_beton", "TotRNet_Avg_kiebitz", 
                                         "RlNet_Avg_beton", "RlNet_Avg_kiebitz",
                                         "RsNet_Avg_beton", "RsNet_Avg_kiebitz")]
#soil
rain_2$TIMESTAMP<-as.POSIXct(rain_1$TIMESTAMP)
rain_2<-rain_2[,!names(rain_2) %in% c("RECORD.x")]
#soil
soil_2$TIMESTAMP<-as.POSIXct(soil_2$TIMESTAMP)
#remove unneccessary columns
soil_2<-soil_2[,c(1, 3, 9, 15)]

meteo_all<-cbind(meteo_2, rain_2, soil_2)
meteo_all<-meteo_all[,-c(20,18)]

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
write.csv(meteo_all, file="meteo_sample_day.csv", row.names=F)

meteo_all<-read.csv("meteo_sample_day.csv")
#reshape
meteo_all_long <- meteo_all %>%                             
  gather(variable, value, -c(TIMESTAMP))

colnames(meteo_all)

#plot ind var
plot_meteo<-function(var){
  ggplot(meteo_all_long[meteo_all_long$variable==var,], aes(TIMESTAMP, value)) + 
    geom_line()+
    ylab(label=var)+
    theme_bw()
}

plot_meteo("AirTC_Avg_beton")

ggplot(meteo_2_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=3, scales="free")+
  theme_bw()
ggsave(filename="meteo_2_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#get stats
meteo_stats$meteo_2<-colMeans(meteo_2[,3:length(meteo_2)], na.rm=T)
#transpose
meteo_stats<-as.data.frame(t(meteo_stats))

#rain
#convert timestamp
rain_2$TIMESTAMP<-as.POSIXct(rain_1$TIMESTAMP)
rain_2<-rain_2[,!names(rain_2) %in% c("RECORD.x")]

#reshape
rain_2_long <- rain_2 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(rain_2_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=2, scales="free")+
  theme_bw()

ggsave(filename="rain_2_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#stats
meteo_stats$rain[2]<-mean(rain_2$Rain_mm_Tot, na.rm=T)
#soil
#convert timestamp
soil_2$TIMESTAMP<-as.POSIXct(soil_2$TIMESTAMP)
#remove unneccessary columns
soil_2<-soil_2[,c(1, 3, 9, 15)]
#reshape
soil_2_long <- soil_2 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(soil_2_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., nrow=3, scales="free")+
  theme_bw()

ggsave(filename="soil_2_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#stats
soil_stats_2<-data.frame("VWC_1"=mean(soil_2$WC01_VWC_Avg),
                         "VWC_2"=mean(soil_2$WC02_VWC_Avg),
                         "VWC_3"=mean(soil_2$WC03_VWC_Avg))
soil_stats<-rbind(soil_stats_1, soil_stats_2)