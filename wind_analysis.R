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
for(i in files_list) {
  if(!exists("output")) {
    j=1
    print(i)
    dat=read.table(i, sep = ",", dec = ".", skip=4, header = T, 
                   na.strings = c("-9999"), stringsAsFactors = FALSE,
                   col.names = read.table(i, 
                                          skip=1, sep=",", dec=".", nrows = 1))
    #write in output
    output<-data.frame(name = i,											### was alles ausgegeben werden soll, Dateiname
                       timestamp_start = dat$TIMESTAMP[1],									### timestamp start of Interval 
                       timestamp_end=dat$TIMESTAMP[length(dat$TIMESTAMP)], ###timestamp end of interval
                       vector_windspeed = sqrt(mean(test_file$u^2, na.rm=T)+mean(test_file$v^2, na.rm=T)),					### gemittelte Windgeschwindigkeit, normales "mean"
                       scalar_windspeed = mean(sqrt((test_file$u^2)+(test_file$v^2)), na.rm=T)
                           )
  } else {
    print(i)
    j=j+1
    dat=read.table(i, sep = ",", dec = ".", skip=4, header = T, 
                   na.strings = c("-9999"), stringsAsFactors = FALSE,
                   col.names = read.table(i, 
                                          skip=1, sep=",", dec=".", nrows = 1))
    if(nrow(dat)>=1){ #error handling emtpy files
       #write in output
    output_temp<-data.frame(name = i,											### was alles ausgegeben werden soll, Dateiname
                            timestamp_start = dat$TIMESTAMP[1],									### timestamp start of Interval 
                            timestamp_end=dat$TIMESTAMP[length(dat$TIMESTAMP)], ###timestamp end of interval
                            vector_windspeed = sqrt(mean(test_file$u^2, na.rm=T)+mean(test_file$v^2, na.rm=T)),					### gemittelte Windgeschwindigkeit, normales "mean"
                            scalar_windspeed = mean(sqrt((test_file$u^2)+(test_file$v^2)), na.rm=T)
                                )
    output=rbind(output, output_temp)
    remove(output_temp)
    }
  }
}


setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Data")
#write.csv(output, file = "wind_beton.csv", row.names = F)
write.csv(output, file = "wind_kiebitz.csv", row.names = F)
