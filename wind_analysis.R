library(tidyverse)
library(openair)
options(digits.secs = 0)
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
      dat$ws<-sqrt(dat$u^2+dat$v^2)
      dat$wd<-(atan2(dat$u, dat$v) * 360/2/pi) + 180
      names(dat)[1]<-"date"
      dat$date<-as.POSIXct(dat$date, format = "%Y-%m-%d %H:%M:%OS") #format date
      dat<-dat[,c("date", "ws", "wd")] #remove unneccessary columns
    #write in output
    output<-data.frame("name" = i,											### was alles ausgegeben werden soll, Dateiname
                       "timestamp_start" = dat$date[1],									### timestamp start of Interval 
                       "timestamp_end"=dat$date[length(dat$date)], ###timestamp end of interval
                       "vector_windspeed" = timeAverage(mydata=dat, vector.ws = TRUE)$ws,	
                       "scalar_windspeed" = timeAverage(mydata=dat, vector.ws = FALSE)$ws 
                           )
    }else{}
  } else {
    print(i)
    j=j+1
    dat=read.table(i, sep = ",", dec = ".", skip=4, header = T, 
                   na.strings = c("-9999"), stringsAsFactors = FALSE,
                   col.names = read.table(i, 
                                          skip=1, sep=",", dec=".", nrows = 1))
    if(any(is.numeric(dat$u)&any(is.numeric(dat$v)))){
    dat$ws<-sqrt(dat$u^2+dat$v^2)
    names(dat)[1]<-"date"
    dat$wd<-(atan2(dat$u, dat$v) * 360/2/pi) + 180
    dat$date<-as.POSIXct(dat$date, format = "%Y-%m-%d %H:%M:%OS") #format date
    dat<-dat[,c("date", "ws", "wd")] #remove unneccessary columns 
    #write in output
    output_temp<-data.frame("name" = i,											### was alles ausgegeben werden soll, Dateiname
                       "timestamp_start" = dat$date[1],									### timestamp start of Interval 
                       "timestamp_end"=dat$date[length(dat$date)], ###timestamp end of interval
                       "vector_windspeed" = timeAverage(mydata=dat,vector.ws = TRUE)$ws,	
                       "scalar_windspeed" = timeAverage(mydata=dat, vector.ws = FALSE)$ws 
    )
    output=rbind(output, output_temp)
    remove(output_temp)
      }else{}
  }
}

output$vector_windspeed
plot(output$vector_windspeed, type="l")
lines(output$scalar_windspeed, type="l", col="red")
#wind_kiebitz<-output
#wind_beton<-output

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Data")
write.csv(wind_beton, file = "wind_beton.csv", row.names = F)
write.csv(wind_kiebitz, file = "wind_kiebitz.csv", row.names = F)

#read csv files
wind_beton<-read.csv(file="wind_beton.csv")
wind_kiebitz<-read.csv(file="wind_kiebitz.csv")

#convert timestamp
wind_beton$TIMESTAMP<-as.POSIXct(wind_beton$timestamp_start)
wind_kiebitz$TIMESTAMP<-as.POSIXct(wind_kiebitz$timestamp_start)

#create continious timestamp for beton
cont_time<- data.frame(TIMESTAMP=seq(as.POSIXct("2021-07-23 00:00:00 MEST", tz="MET"), 
                                     as.POSIXct("2021-08-23 08:00:00 MEST", tz="MET"), by="30 min"))
wind_beton <- right_join(wind_beton,cont_time,by.x='TIMESTAMP',by.y='TIMESTAMP')
#remove rows with duplicated timestamps
wind_beton <- wind_beton %>%   distinct(TIMESTAMP, .keep_all = TRUE)
#for kiebitz
wind_kiebitz <- merge(wind_kiebitz,cont_time,by.x='TIMESTAMP',by.y='TIMESTAMP',all.x=F,all.y=T)
#remove rows with duplicated timestamps
wind_kiebitz <- wind_kiebitz %>%   distinct(TIMESTAMP, .keep_all = TRUE)

#wind steadiness: vector wind / scalar wind
wind_beton$steadiness<-wind_beton$vector_windspeed/wind_beton$scalar_windspeed
range(wind_beton$steadiness, na.rm=T)
mean(wind_beton$steadiness, na.rm=T)
ggplot(data=wind_beton)+
  geom_line(aes(x=TIMESTAMP, y=steadiness))+
  theme_bw()

wind_kiebitz$steadiness<-wind_kiebitz$vector_windspeed/wind_kiebitz$scalar_windspeed
range(wind_kiebitz$steadiness, na.rm=T)

mean(wind_kiebitz$steadiness, na.rm=T)
ggplot(data=wind_kiebitz)+
  geom_line(aes(x=TIMESTAMP, y=steadiness))+
  theme_bw()

#wind persistance 
#P = 2/pi * sin^-1(k)
wind_beton$Persistance<-(2/pi)*asin(wind_beton$steadiness)
range(wind_beton$Persistance, na.rm=T)
hist(wind_beton$Persistance)
wind_kiebitz$Persistance<-(2/pi)*asin(wind_kiebitz$steadiness)
range(wind_kiebitz$Persistance, na.rm=T)
hist(wind_kiebitz$Persistance)

plot(diff(wind_beton$TIMESTAMP))
plot(diff(wind_kiebitz$TIMESTAMP))

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Data")
write.csv(wind_beton, file = "wind_beton_calc.csv", row.names = F)
write.csv(wind_kiebitz, file = "wind_kiebitz_calc.csv", row.names = F)



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
#The vector average wind speeds are always lower than the scalar average wind speeds because of the behaviour of vectors

#steadiness
ggplot(data=wind_beton)+
  geom_line(aes(x=TIMESTAMP, y=steadiness))+
  theme_bw()+
  geom_abline(aes(intercept=1, slope=0), col="red")+
  ggtitle(label="Wind steadiness", subtitle = "Vector windspeed [m/s] /scalar windspeed [m/s]")
ggsave(filename="wind_steadiness_EC02.pdf",
       device="pdf",width=297, height=210, units = "mm")

#persistance
ggplot(data=wind_beton)+
  geom_line(aes(x=TIMESTAMP, y=Persistance))+
  theme_bw()+
  geom_abline(aes(intercept=1, slope=0), col="red")+
  ggtitle(label="Wind persistance EC02", subtitle = "P = 2/pi*sin^-1(k)")
ggsave(filename="wind_persistance_EC02.pdf",
       device="pdf",width=297, height=210, units = "mm")

####kiebitz
#scalar and vector
ggplot(data=wind_kiebitz)+
  geom_line(aes(x=TIMESTAMP, y=scalar_windspeed, col="scalar"))+
  geom_line(aes(x=TIMESTAMP, y=vector_windspeed, col="vector"))+
  theme_bw()+
  ggtitle(label="Vector vs Scalar Windspeed - EC04")
ggsave(filename="Vector_scalar_ws_EC04.pdf",
       device="pdf",width=297, height=210, units = "mm")
#steadiness
ggplot(data=wind_kiebitz)+
  geom_line(aes(x=TIMESTAMP, y=steadiness))+
  theme_bw()+
  geom_abline(aes(intercept=1, slope=0), col="red")+
  ggtitle(label="Wind steadiness", subtitle = "Vector windspeed [m/s] /scalar windspeed [m/s]")
ggsave(filename="wind_steadiness_EC04.pdf",
       device="pdf",width=297, height=210, units = "mm")

#persistance
ggplot(data=wind_kiebitz)+
  geom_line(aes(x=TIMESTAMP, y=Persistance))+
  theme_bw()+
  geom_abline(aes(intercept=1, slope=0), col="red")+
  ggtitle(label="Wind persistance EC04", subtitle = "P = 2/pi*sin^-1(k)")
ggsave(filename="wind_persistance_EC04.pdf",
       device="pdf",width=297, height=210, units = "mm")

#wind stats
#kiebitz
range(wind_kiebitz$vector_windspeed, na.rm=T)
range(wind_kiebitz$scalar_windspeed, na.rm=T)
hist(wind_kiebitz$vector_windspeed)
hist(wind_kiebitz$scalar_windspeed)
#overall mean
grass_wind_dat<-kiebitz[,c("TIMESTAMP", "wind_speed", "wind_dir")]
colnames(grass_wind_dat)<-c("date", "ws", "wd")

timeAverage(avg.time = "year", mydata=grass_wind_dat,vector.ws = TRUE)$ws #vector
#1.27418
timeAverage(avg.time = "year", mydata=grass_wind_dat,vector.ws = FALSE)$ws #scalar
# 1.524129

#beton
range(wind_beton$vector_windspeed, na.rm=T)
range(wind_beton$scalar_windspeed, na.rm=T)
hist(wind_beton$vector_windspeed)
hist(wind_beton$scalar_windspeed)

#overall mean
beton_wind_dat<-beton[,c("TIMESTAMP", "wind_speed", "wind_dir")]
colnames(beton_wind_dat)<-c("date", "ws", "wd")

timeAverage(avg.time = "year", mydata=beton_wind_dat,vector.ws = TRUE)$ws #vector
#1.448099
timeAverage(avg.time = "year", mydata=beton_wind_dat,vector.ws = FALSE)$ws #scalar
#1.960371

#plot both
wind_beton$index<-"Concrete"
wind_kiebitz$index<-"Grass"

wind_both<-rbind(wind_beton, wind_kiebitz)

ggplot(data=wind_both)+
  geom_histogram(aes(x=vector_windspeed))+
  theme_bw()+
  xlab(bquote('Wind Speed [' ~ms^-1* ']'))+
  facet_grid(cols=vars(index))

