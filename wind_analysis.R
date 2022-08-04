#wind analysis
#meandering 
#calculate scalar and vector wind speed
        #scalar wind speed (arithmetic mean)
        #vector wind speed --> sqrt((dat$u^2)+(dat$v^2)) 
#list raw files
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/03_Rohdaten_konvertiert/EC02_converted")

files_list=list.files(pattern ="\\.csv")
files_list[200]
test_file<-read.table(file=files_list[200], skip = 4, sep=",", dec=".", 
                      col.names = read.table(file=files_list[1], 
                                             skip=1, sep=",", dec=".", nrows = 1))

mean(sqrt((test_file$u^2)+(test_file$v^2))) #scalar
sqrt(mean(test_file$u^2)+mean(test_file$v^2)) #vector

for(i in files_list) {
  if(!exists("output")) {
    j=1
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
    j=j+1
    dat=read.table(i, sep = ",", dec = ".", skip=4, header = T, 
                   na.strings = c("-9999"), stringsAsFactors = FALSE,
                   col.names = read.table(i, 
                                          skip=1, sep=",", dec=".", nrows = 1))
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
