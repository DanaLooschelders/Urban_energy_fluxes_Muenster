#set general values
plot_dir<-"Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken"
#script_dir
#data_dir
#####modified function from FERddyPro Package####
readEP<-function(dataFile="Filename", na = "NaN"){
  n <- read.table(dataFile, header = TRUE, nrows = 1, sep = ",", 
                  fill = TRUE, skip=1)
  df <- read.table(dataFile, header = FALSE, skip = 3, 
                   sep = ",",, na.strings = "-9999")
  names(df) <- names(n)
  df[df == na] <- NA
  return(df)
}
####Beton EC02####
setwd("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/EddyPro/EC02_Ver1/")
beton=readEP("eddypro_Ver1_full_output_2021-12-21T092740_adv.csv")

#check
str(beton)
#create new col with datetime
beton$datetime=paste(beton$date, beton$time)
#convert datetime to POSixlt
beton$datetime=strptime(beton$datetime, format="%Y-%m-%d %H:%M", tz = "MET")
#check
str(beton$datetime)
#convert to Posixct
beton$datetime=as.POSIXct(beton$datetime)

#add hour of day as column
beton$hour<-format(beton$datetime, format="%H")
str(beton$hour)

#add index for night and day
beton$tod<-NA
#set index for night: between 22 and 6 
beton$hour_num<-as.numeric(beton$hour)
beton$tod[beton$hour_num<=6|beton$hour_num>=22]<-"night"
#set index for day: between 6 and 22
beton$tod[beton$hour_num>6&beton$hour_num<22]<-"day"

##exclude fluxes with qc values >6
beton$co2_flux[beton$qc_co2_flux>6]<-NA
beton$h2o_flux[beton$qc_h2o_flux>6]<-NA
beton$LE[beton$qc_LE>6]<-NA
beton$Tau[beton$qc_Tau>6]<-NA
beton$H[beton$qc_H>6]<-NA

#cut data to correct length
range(beton$datetime)

beton<-beton[beton$datetime>=as.POSIXct("2021-07-23 00:00:00 MEST", tz="MET")&
               beton$datetime<=as.POSIXct("2021-08-23 08:00:00 MEST", tz="MET"),]


####Kiebitz EC04####
setwd("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/EddyPro/EC04_Ver1/")
kiebitz=readEP("eddypro_Ver1_full_output_2021-12-21T114643_adv.csv")

#check
str(kiebitz)
#create new col with datetime
kiebitz$datetime=paste(kiebitz$date, kiebitz$time)
#convert datetime to POSixlt
kiebitz$datetime=strptime(kiebitz$datetime, format="%Y-%m-%d %H:%M", tz="MET")
#check
str(kiebitz$datetime)
#convert to Posixct
kiebitz$datetime=as.POSIXct(kiebitz$datetime)

#add hour of day as column
kiebitz$hour<-format(kiebitz$datetime, format="%H")
str(kiebitz$hour)

#add index for night and day
kiebitz$tod<-NA
#set index for night: between 22 and 6 
kiebitz$hour_num<-as.numeric(kiebitz$hour)
kiebitz$tod[kiebitz$hour_num<=6|kiebitz$hour_num>=22]<-"night"
#set index for day: between 6 and 22
kiebitz$tod[kiebitz$hour_num>6&kiebitz$hour_num<22]<-"day"

##exclude fluxes with qc values >6
kiebitz$co2_flux[kiebitz$qc_co2_flux>6]<-NA
kiebitz$h2o_flux[kiebitz$qc_h2o_flux>6]<-NA
kiebitz$LE[kiebitz$qc_LE>6]<-NA
kiebitz$Tau[kiebitz$qc_Tau>6]<-NA
kiebitz$H[kiebitz$qc_H>6]<-NA

#cut data to correct length
range(kiebitz$datetime)

kiebitz<-kiebitz[kiebitz$datetime>=as.POSIXct("2021-07-23 00:00:00 MEST", tz="MET")&
               kiebitz$datetime<=as.POSIXct("2021-08-23 08:00:00 MEST", tz="MET"),]

