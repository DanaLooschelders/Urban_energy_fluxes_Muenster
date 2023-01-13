library(tidyr)
library(ggplot2)
library(tidyverse)
#install.packages("ggnewscale")
library(ggnewscale)
#check meteorological conditions for grass subset
#subset 1: "2021-08-02 05:07:12 CEST" to "2021-08-03 05:06:48 CEST"
starttime_1 <- as.POSIXct("2021-07-30 08:00:00 CEST")
endtime_1 <- as.POSIXct("2021-07-31 08:00:00 CEST")

#subset 2: "2021-08-10 13:05:12 CEST"  to "2021-08-11 13:05:36 CEST"
starttime_2 <- as.POSIXct("2021-08-04 00:00:00 CEST")
endtime_2 <- as.POSIXct("2021-08-05 00:00:00 CEST")

#subset 3 "2021-08-10 19:46:48 CEST" "2021-08-11 19:47:12 CEST"
starttime_3<-as.POSIXct("2021-08-11 00:00:00 CEST")
endtime_3<-as.POSIXct("2021-08-12 00:00:00 CEST")

#Validation
#subset 4
starttime_4<-as.POSIXct("2021-08-14 16:00:00 CEST")
endtime_4<-as.POSIXct("2021-08-15 16:00:00 CEST")
#load slow data
#source script to load slow data and QAQC
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Slow_data/QAQC_slow_data.R") 

#subset to 1
meteo_1<-dat.meteo.merge[dat.meteo.merge$TIMESTAMP>=starttime_1&dat.meteo.merge$TIMESTAMP<=endtime_1,]
rain_1<-dat.rain.merge[dat.rain.merge$TIMESTAMP>=starttime_1&dat.rain.merge$TIMESTAMP<=endtime_1,]
soil_1<-dat.soil.merge[dat.soil.merge$TIMESTAMP>=starttime_1&dat.soil.merge$TIMESTAMP<=endtime_1,]

#subset to 2
meteo_2<-dat.meteo.merge[dat.meteo.merge$TIMESTAMP>=starttime_2&dat.meteo.merge$TIMESTAMP<=endtime_2,]
rain_2<-dat.rain.merge[dat.rain.merge$TIMESTAMP>=starttime_2&dat.rain.merge$TIMESTAMP<=endtime_2,]
soil_2<-dat.soil.merge[dat.soil.merge$TIMESTAMP>=starttime_2&dat.soil.merge$TIMESTAMP<=endtime_2,]

#subset to 3
meteo_3<-dat.meteo.merge[dat.meteo.merge$TIMESTAMP>=starttime_3&dat.meteo.merge$TIMESTAMP<=endtime_3,]
rain_3<-dat.rain.merge[dat.rain.merge$TIMESTAMP>=starttime_3&dat.rain.merge$TIMESTAMP<=endtime_3,]
soil_3<-dat.soil.merge[dat.soil.merge$TIMESTAMP>=starttime_3&dat.soil.merge$TIMESTAMP<=endtime_3,]

#subset to 4
meteo_4<-dat.meteo.merge[dat.meteo.merge$TIMESTAMP>=starttime_4&dat.meteo.merge$TIMESTAMP<=endtime_4,]
rain_4<-dat.rain.merge[dat.rain.merge$TIMESTAMP>=starttime_4&dat.rain.merge$TIMESTAMP<=endtime_4,]
soil_4<-dat.soil.merge[dat.soil.merge$TIMESTAMP>=starttime_4&dat.soil.merge$TIMESTAMP<=endtime_4,]
####meteo###
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Meteo")
#convert timestamp
meteo_1$TIMESTAMP<-as.POSIXct(meteo_1$TIMESTAMP)
meteo_1<-meteo_1[,!names(meteo_1) %in% c("RECORD_beton", "RECORD_kiebitz", 
                     "CNR4TC_Avg_beton", "CNR4TC_Avg_kiebitz", 
                     "Albedo_Avg_beton", "Albedo_Avg_kiebitz", 
                     "TotRNet_Avg_beton", "TotRNet_Avg_kiebitz", 
                     "RlNet_Avg_beton", "RlNet_Avg_kiebitz",
                     "RsNet_Avg_beton", "RsNet_Avg_kiebitz")]
#meteo_1$Record<-seq(1, nrow(meteo_1), by=1)
#reshape
meteo_1_long <- meteo_1 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(meteo_1_long, aes(TIMESTAMP, value)) + 
  geom_line()+
facet_wrap(variable ~ ., ncol=3, scales="free")+
  theme_bw()
ggsave(filename="meteo_1_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#get stats
meteo_stats<-data.frame("meteo_1"=colMeans(meteo_1[,3:length(meteo_1)], na.rm=T))

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
#reshape
meteo_2_long <- meteo_2 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
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
#meteo 3
#convert timestamp
meteo_3$TIMESTAMP<-as.POSIXct(meteo_3$TIMESTAMP)
#reduce vars
meteo_3<-meteo_3[,!names(meteo_3) %in% c("RECORD_beton", "RECORD_kiebitz", 
                                         "CNR4TC_Avg_beton", "CNR4TC_Avg_kiebitz", 
                                         "Albedo_Avg_beton", "Albedo_Avg_kiebitz", 
                                         "TotRNet_Avg_beton", "TotRNet_Avg_kiebitz", 
                                         "RlNet_Avg_beton", "RlNet_Avg_kiebitz",
                                         "RsNet_Avg_beton", "RsNet_Avg_kiebitz")]
#meteo_1$Record<-seq(1, nrow(meteo_1), by=1)
#reshape
meteo_3_long <- meteo_3 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(meteo_3_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=3, scales="free")+
  theme_bw()
ggsave(filename="meteo_3_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#get stats
meteo_stats["meteo_3",]<-colMeans(meteo_3[,3:length(meteo_3)], na.rm=T)
#convert timestamp
meteo_4$TIMESTAMP<-as.POSIXct(meteo_4$TIMESTAMP)
meteo_4<-meteo_4[,!names(meteo_4) %in% c("RECORD_beton", "RECORD_kiebitz", 
                                         "CNR4TC_Avg_beton", "CNR4TC_Avg_kiebitz", 
                                         "Albedo_Avg_beton", "Albedo_Avg_kiebitz", 
                                         "TotRNet_Avg_beton", "TotRNet_Avg_kiebitz", 
                                         "RlNet_Avg_beton", "RlNet_Avg_kiebitz",
                                         "RsNet_Avg_beton", "RsNet_Avg_kiebitz")]
#meteo_1$Record<-seq(1, nrow(meteo_1), by=1)
#reshape
meteo_4_long <- meteo_4 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(meteo_4_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=3, scales="free")+
  theme_bw()
ggsave(filename="meteo_4_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#get stats
meteo_stats["meteo_4",]<-colMeans(meteo_4[,3:length(meteo_4)], na.rm=T)

####rain###
#convert timestamp
rain_1$TIMESTAMP<-as.POSIXct(rain_1$TIMESTAMP)
rain_1<-rain_1[,!names(rain_1) %in% c("RECORD.x")]
#reshape
rain_1_long <- rain_1 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(rain_1_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=2, scales="free")+
  theme_bw()
ggsave(filename="rain_1_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#add col for rain
meteo_stats$rain<-NA
#stats
meteo_stats$rain[1]<-mean(rain_1$Rain_mm_Tot)
#soil
#convert timestamp
soil_1$TIMESTAMP<-as.POSIXct(soil_1$TIMESTAMP)
#remove unneccessary columns
soil_1<-soil_1[,c(1, 3, 9, 15)]
#reshape
soil_1_long <- soil_1 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(soil_1_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., nrow=3, scales="free")+
  theme_bw()
ggsave(filename="soil_1_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")

soil_stats_1<-data.frame("VWC_1"=mean(soil_1$WC01_VWC_Avg),
                         "VWC_2"=mean(soil_1$WC02_VWC_Avg),
                         "VWC_3"=mean(soil_1$WC03_VWC_Avg))

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
#rain
#convert timestamp
rain_3$TIMESTAMP<-as.POSIXct(rain_3$TIMESTAMP)
rain_3<-rain_3[,!names(rain_3) %in% c("RECORD.x")]

#reshape
rain_3_long <- rain_3 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(rain_3_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=2, scales="free")+
  theme_bw()

ggsave(filename="rain_3_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#stats
meteo_stats$rain[3]<-mean(rain_3$Rain_mm_Tot, na.rm=T)
#soil
#convert timestamp
soil_3$TIMESTAMP<-as.POSIXct(soil_3$TIMESTAMP)
#remove unneccessary columns
soil_3<-soil_3[,c(1, 3, 9, 15)]
#reshape
soil_3_long <- soil_3 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(soil_3_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., nrow=3, scales="free")+
  theme_bw()

ggsave(filename="soil_3_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
####
#rain
#convert timestamp
rain_4$TIMESTAMP<-as.POSIXct(rain_4$TIMESTAMP)
rain_4<-rain_4[,!names(rain_4) %in% c("RECORD.x")]

#reshape
rain_4_long <- rain_4 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(rain_4_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=2, scales="free")+
  theme_bw()

ggsave(filename="rain_4_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#stats
meteo_stats$rain[4]<-mean(rain_4$Rain_mm_Tot, na.rm=T)
#soil
#convert timestamp
soil_4$TIMESTAMP<-as.POSIXct(soil_4$TIMESTAMP)
#remove unneccessary columns
soil_4<-soil_4[,c(1, 3, 9, 15)]
#reshape
soil_4_long <- soil_4 %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(soil_4_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., nrow=3, scales="free")+
  theme_bw()

ggsave(filename="soil_4_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#stats
soil_stats_3<-data.frame("VWC_1"=mean(soil_2$WC01_VWC_Avg),
                         "VWC_2"=mean(soil_2$WC02_VWC_Avg),
                         "VWC_3"=mean(soil_2$WC03_VWC_Avg))
soil_stats<-rbind(soil_stats_1, soil_stats_2, soil_stats_3)

meteo_stats<-cbind(meteo_stats, soil_stats)

#####calculate overall stats to compare####
####meteo###
#convert timestamp
dat.meteo.merge$TIMESTAMP<-as.POSIXct(dat.meteo.merge$TIMESTAMP)
#reduce vars
dat.meteo.merge<-dat.meteo.merge[,!names(dat.meteo.merge) %in% c("RECORD_beton", "RECORD_kiebitz", 
                                         "CNR4TC_Avg_beton", "CNR4TC_Avg_kiebitz", 
                                         "Albedo_Avg_beton", "Albedo_Avg_kiebitz", 
                                         "TotRNet_Avg_beton", "TotRNet_Avg_kiebitz", 
                                         "RlNet_Avg_beton", "RlNet_Avg_kiebitz",
                                         "RsNet_Avg_beton", "RsNet_Avg_kiebitz")]
#meteo_1$Record<-seq(1, nrow(meteo_1), by=1)
#reshape
dat_kiebitz_meteo_long <- dat.meteo.merge %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(dat_kiebitz_meteo_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=3, scales="free")+
  theme_bw()

ggsave(filename="meteo_whole_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
##add green background for sample days!

#get stats
meteo_whole_stats<-data.frame("meteo_1"=colMeans(dat.meteo.merge[,3:length(dat.meteo.merge)], na.rm=T))

#transpose
meteo_whole_stats<-as.data.frame(t(meteo_whole_stats))
#convert timestamp
dat.rain.merge$TIMESTAMP<-as.POSIXct(dat.rain.merge$TIMESTAMP)
#reshape
dat_kiebitz_rain_long <- dat.rain.merge %>%                             
  gather(variable, value, -c(TIMESTAMP, RECORD.x))

#plot
ggplot(dat_kiebitz_rain_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., ncol=2, scales="free")+
  theme_bw()
ggsave(filename="rain_whole_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")
#add col for rain
meteo_whole_stats$rain<-NA
#stats
meteo_whole_stats$rain[1]<-mean(dat.rain.merge$Rain_mm_Tot, na.rm=T)
#soil
#convert timestamp
dat.soil.merge$TIMESTAMP<-as.POSIXct(dat.soil.merge$TIMESTAMP)
#remove unneccessary columns
dat.soil.merge<-dat.soil.merge[,c(1, 3, 9, 15)]
#reshape
dat_kiebitz_soil_long <- dat.soil.merge %>%                             
  gather(variable, value, -c(TIMESTAMP))

#plot
ggplot(dat_kiebitz_soil_long, aes(TIMESTAMP, value)) + 
  geom_line()+
  facet_wrap(variable ~ ., nrow=3, scales="free")+
  theme_bw()

ggsave(filename="soil_whole_all_vars.pdf", 
       device="pdf", width = 297, height = 210 , units = "mm")

#stats
soil_stats_whole<-data.frame("VWC_1"=mean(dat.soil.merge$WC01_VWC_Avg, na.rm=T),
                         "VWC_2"=mean(dat.soil.merge$WC02_VWC_Avg, na.rm=T),
                         "VWC_3"=mean(dat.soil.merge$WC03_VWC_Avg, na.rm=T))

meteo_whole<-cbind(meteo_whole_stats, soil_stats_whole)
meteo_stats[4,]<-meteo_whole
rownames(meteo_stats)[4]<-"whole"

write.csv2(meteo_stats, "meteo_stats.csv")
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Meteo")
dat.soil.merge$mean_VWC<-rowMeans(dat.soil.merge[,c("WC01_VWC_Avg","WC02_VWC_Avg", "WC03_VWC_Avg")], na.rm=T )
dat.soil.merge$TIMESTAMP<-as.POSIXct(dat.soil.merge$TIMESTAMP)
#plot rain together with SW Radiation for whole time span for Kiebitz
dat.meteo.merge$TIMESTAMP<-as.POSIXct(dat.meteo.merge$TIMESTAMP)
ggplot(data=dat.meteo.merge)+
  geom_rect(aes(xmin=starttime_1, xmax=endtime_1, 
                ymin=-Inf, ymax=Inf, fill="Test"))+
  geom_rect(aes(xmin=starttime_2, xmax=endtime_2, 
                ymin=-Inf, ymax=Inf, fill="Test"))+
  geom_rect(aes(xmin=starttime_3, xmax=endtime_3, 
                ymin=-Inf, ymax=Inf, fill="Test"))+
  geom_rect(aes(xmin=starttime_4, xmax=endtime_4, 
                ymin=-Inf, ymax=Inf, fill="Validation"))+
  scale_fill_manual('Chosen Subsets', labels=c("Test", "Validation"),
                    values = c('lightblue', 'lightgreen' ),  
                    guide = guide_legend(override.aes = list(alpha = 1)))+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg_kiebitz, col="Radiation"))+
  geom_line(data=dat.soil.merge, aes(x=TIMESTAMP, y=mean_VWC*1000, col="Volumetric Water \nContent [mm/m^3]"))+
  theme_bw()+
  scale_y_continuous(name = bquote(atop('SW Rad. [W' ~m^-2* ']', 'Volumetric Water Content [mm/'*m^-3~']')), limits=c(0,1000),
                     sec.axis = sec_axis(~./70, name = "Precipitation [mm]"))+
  scale_color_manual("Lines",values=c("grey", "black"))+
  xlab(label="Time")+
  new_scale_color()+
  #geom_point(data=dat.rain.merge, aes(x=TIMESTAMP, y=Rain_mm_Tot*70))+
  scale_color_manual('Bars', labels=c("Precipitation"),values = c('blue'))+
  geom_bar(data=dat.rain.merge, aes(x=TIMESTAMP, y=Rain_mm_Tot*70, color="blue"), 
           stat="identity", position="dodge", fill="blue")

ggsave(filename = "Rad_Precip_VWC_Subsets_kiebitz.png",
       device="png",width=297, height=110, units = "mm",)
plot(dat.soil.merge$mean_VWC)
#same for beton
#  dat.meteo.merge$TIMESTAMP<-as.POSIXct(dat.meteo.merge$TIMESTAMP)
  ggplot(data=dat.meteo.merge)+
    geom_rect(aes(xmin=starttime_1, xmax=endtime_1, 
                  ymin=-Inf, ymax=Inf, fill="Test"))+
    geom_rect(aes(xmin=starttime_2, xmax=endtime_2, 
                  ymin=-Inf, ymax=Inf, fill="Test"))+
    geom_rect(aes(xmin=starttime_3, xmax=endtime_3, 
                  ymin=-Inf, ymax=Inf, fill="Test"))+
    geom_rect(aes(xmin=starttime_4, xmax=endtime_4, 
                  ymin=-Inf, ymax=Inf, fill="Validation"))+
    scale_fill_manual('Chosen Subsets', labels=c("Test", "Validation"),
                      values = c('lightblue', 'lightgreen' ),  
                      guide = guide_legend(override.aes = list(alpha = 1)))+
    geom_line(aes(x=TIMESTAMP, y=SUp_Avg_beton, col="Radiation"))+
    theme_bw()+
    scale_y_continuous(name = bquote('SW Rad. [W' ~m^-2* ']'), limits=c(0,1000),
                       sec.axis = sec_axis(~./70, name = "Precipitation [mm]"))+
    scale_color_manual("Lines",values=c("grey"))+
    xlab(label="Time")+
    new_scale_color()+
    #geom_point(data=dat.rain.merge, aes(x=TIMESTAMP, y=Rain_mm_Tot*70))+
    scale_color_manual('Bars', labels=c("Precipitation"),values = c('blue'))+
    geom_bar(data=dat.rain.merge, aes(x=TIMESTAMP, y=Rain_mm_Tot*70, color="blue"), 
             stat="identity", position="dodge", fill="blue")
  
  ggsave(filename = "Rad_Precip_Subsets_beton.png",
         device="png",width=297, height=110, units = "mm")
  
  
  #test
  ggplot(data=dat.rain.merge,)+
  geom_point( aes(x=TIMESTAMP, y=Rain_mm_Tot*70))+
  geom_bar(aes(x=TIMESTAMP,y=Rain_mm_Tot*70), stat="identity", col="red")
  
   max(dat.rain.merge$Rain_mm_Tot, na.rm=T)
