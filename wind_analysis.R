library(tidyverse)
#wind analysis
#meandering 
#calculate scalar and vector wind speed
        #scalar wind speed (arithmetic mean)
        #vector wind speed --> sqrt((dat$u^2)+(dat$v^2)) 
#list raw files
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/03_Rohdaten_konvertiert/EC02_converted")
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/03_Rohdaten_konvertiert/EC04_converted/")

#execute once for EC02 and once for EC04!!!!
files_list=list.files(pattern ="\\.csv")

#mean(sqrt((test_file$u^2)+(test_file$v^2))) #scalar
#sqrt(mean(test_file$u^2)+mean(test_file$v^2)) #vector
rm(output)
for(i in files_list){
  if(!exists("output")){
    j=1
    print(i)
    dat=read.table(i, sep = ",", dec = ".", skip=4, header = T, 
                   na.strings = c("-9999"), stringsAsFactors = FALSE,
                   col.names = read.table(i, 
                                          skip=1, sep=",", dec=".", nrows = 1))
    if(any(is.numeric(dat$u)&any(is.numeric(dat$v)))){
    #write in output
    output<-data.frame(name = i,											### was alles ausgegeben werden soll, Dateiname
                       timestamp_start = dat$TIMESTAMP[1],									### timestamp start of Interval 
                       timestamp_end=dat$TIMESTAMP[length(dat$TIMESTAMP)], ###timestamp end of interval
                       vector_windspeed = sqrt(mean(dat$u^2, na.rm=T)+mean(dat$v^2, na.rm=T)),	
                       scalar_windspeed = mean(sqrt((dat$u^2)+(dat$v^2)), na.rm=T)
                           )
    }else{}
  } else {
    print(i)
    j=j+1
    dat=read.table(i, sep = ",", dec = ".", skip=4, header = T, 
                   na.strings = c("-9999"), stringsAsFactors = FALSE,
                   col.names = read.table(i, 
                                          skip=1, sep=",", dec=".", nrows = 1))
   
       #write in output
      if(any(is.numeric(dat$u)&any(is.numeric(dat$v))&nrow(dat)>=1)){  #error handling
    output_temp<-data.frame(name = i,											### was alles ausgegeben werden soll, Dateiname
                            timestamp_start = dat$TIMESTAMP[1],									### timestamp start of Interval 
                            timestamp_end=dat$TIMESTAMP[length(dat$TIMESTAMP)], ###timestamp end of interval
                            vector_windspeed = sqrt(mean(dat$u^2, na.rm=T)+mean(dat$v^2, na.rm=T)),
                            scalar_windspeed = mean(sqrt((dat$u^2)+(dat$v^2)), na.rm=T)
                                )
    output=rbind(output, output_temp)
    remove(output_temp)
      }else{}
  }
}


setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Data")
#write.csv(output, file = "wind_beton.csv", row.names = F)
write.csv(output, file = "wind_kiebitz.csv", row.names = F)

#read csv files
wind_beton<-read.csv(file="wind_beton.csv")
wind_kiebitz<-read.csv(file="wind_kiebitz.csv")

#convert timestamp
wind_beton$TIMESTAMP<-as.POSIXct(wind_beton$timestamp_start)
wind_kiebitz$TIMESTAMP<-as.POSIXct(wind_kiebitz$timestamp_start)
#wind steadiness: vector wind / scalar wind
wind_beton$steadiness<-wind_beton$vector_windspeed/wind_beton$scalar_windspeed
wind_kiebitz$steadiness<-wind_kiebitz$vector_windspeed/wind_kiebitz$scalar_windspeed

#create continious timestamp
cont_time<- data.frame(TIMESTAMP=seq(as.POSIXct(wind_beton$TIMESTAMP[1]), as.POSIXct(wind_beton$TIMESTAMP[nrow(wind_beton)]), by="30 min"))
wind_beton <- merge(wind_beton,cont_time,by.x='TIMESTAMP',by.y='TIMESTAMP',all.x=T,all.y=T)
wind_kiebitz <- merge(wind_kiebitz,cont_time,by.x='TIMESTAMP',by.y='TIMESTAMP',all.x=T,all.y=T)
##### plot 
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Ratios")
#####beton
#wind scalar and vector
ggplot(data=wind_beton)+
  geom_line(aes(x=TIMESTAMP, y=scalar_windspeed, col="scalar"))+
  geom_line(aes(x=TIMESTAMP, y=vector_windspeed, col="vector"))+
  theme_bw()+
  ggtitle(label="Vector vs Scalar Windspeed - EC02")
ggsave(filename="Vector_scalar_ws_EC02.pdf",
       device="pdf",width=297, height=210, units = "mm")
#steadiness
ggplot(data=wind_beton)+
  geom_line(aes(x=TIMESTAMP, y=steadiness))+
  theme_bw()+
  geom_abline(aes(intercept=1, slope=0), col="red")+
  ggtitle(label="Wind steadiness", subtitle = "Vector windspeed [m/s] /scalar windspeed [m/s]")
ggsave(filename="wind_steadiness_EC02.pdf",
       device="pdf",width=297, height=210, units = "mm")

####kiebitz
ggplot(data=wind_kiebitz)+
  geom_line(aes(x=TIMESTAMP, y=scalar_windspeed, col="scalar"))+
  geom_line(aes(x=TIMESTAMP, y=vector_windspeed, col="vector"))+
  theme_bw()+
  ggtitle(label="Vector vs Scalar Windspeed - EC04")
ggsave(filename="Vector_scalar_ws_EC04.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggplot(data=wind_kiebitz)+
  geom_line(aes(x=TIMESTAMP, y=steadiness))+
  theme_bw()+
  geom_abline(aes(intercept=1, slope=0), col="red")+
  ggtitle(label="Wind steadiness", subtitle = "Vector windspeed [m/s] /scalar windspeed [m/s]")
ggsave(filename="wind_steadiness_EC04.pdf",
       device="pdf",width=297, height=210, units = "mm")

