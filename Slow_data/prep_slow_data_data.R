#Slow Meteodata Analysis EC02 Beton and EC04 Kiebitz
#global settings
options(digits.secs=3)
Sys.setenv(TZ='Etc/GMT') #ewige Winterzeit
#load libraries
library(tidyverse)
#set wd
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/")
daten_dir <- "Z:/klima/Projekte/2021_CalmCity/2_Daten"

#raw data source from EC02 Beton
dir_beton_meteo <- "./1_EC2_Beton_readonly/meteo/"
#list files from EC02 Beton
files_beton_meteo <- list.files(paste0(dir_beton_meteo), pattern = ".dat")

#raw data source from EC04 Kiebitz
dir_kiebitz_meteo <- "./2_EC4_Kiebitz_readonly/meteo/"
#list files from EC04 Kiebitz
files_kiebitz_meteo <- list.files(paste0(dir_kiebitz_meteo), pattern = ".dat")

#raw data source from EC04 Kiebitz Rain
dir_kiebitz_rain <- "./2_EC4_Kiebitz_readonly/rain/"
#list files from EC04 Kiebitz
files_kiebitz_rain <- list.files(paste0(dir_kiebitz_rain), pattern = ".dat")

#raw data source from EC04 Kiebitz Soil data
dir_kiebitz_soil<-"./2_EC4_Kiebitz_readonly/vwc/"
files_kiebitz_soil<-list.files(paste0(dir_kiebitz_soil), pattern = ".dat")

#read in data
#EC02 Beton Meteo data
i=1
for(i in c(1:length(files_beton_meteo))) {
  filename.dat <- files_beton_meteo[i]
  dat.meteo.tmp <- read.csv(paste(dir_beton_meteo, filename.dat, sep="/"), header=FALSE,
                            skip=4, stringsAsFactors = F, na.strings=c("NAN", "NA"),
                            col.names=colnames(read.csv(paste(dir_beton_meteo, 
                                                              filename.dat, sep="/"), 
                                                        header=T, skip=1, nrows=5)))[ ,1:14] #read only the first 14 columns (the last two contain only the time offset)
  if(filename.dat==files_beton_meteo[1]) {
    dat.beton.meteo <- dat.meteo.tmp
  } else {
    dat.beton.meteo <- rbind(dat.beton.meteo, dat.meteo.tmp)
  }
}
#convert timestamp
dat.beton.meteo$TIMESTAMP <- strptime(dat.beton.meteo$TIMESTAMP, "%Y-%m-%d %H:%M:%S")
#sort by date
dat.beton.meteo<-dat.beton.meteo[order(dat.beton.meteo$TIMESTAMP),]

#EC04 Kiebitz Meteo data
i=1
for(i in c(1:length(files_kiebitz_meteo))) {
  filename.dat <- files_kiebitz_meteo[i]
  dat.meteo.tmp <- read.csv(paste(dir_kiebitz_meteo, filename.dat, sep="/"), header=FALSE,
                            skip=4, stringsAsFactors = F, na.strings=c("NAN", "NA"),
                            col.names=colnames(read.csv(paste(dir_kiebitz_meteo, 
                                                              filename.dat, sep="/"), 
                                                        header=T, skip=1, nrows=5)))[ ,1:16] #read only the first 16 columns (the last two contain only the time offset)
  if(filename.dat==files_kiebitz_meteo[1]) {
    dat.kiebitz.meteo <- dat.meteo.tmp
  } else {
    dat.kiebitz.meteo <- rbind(dat.kiebitz.meteo, dat.meteo.tmp)
  }
}
#convert timestamp
dat.kiebitz.meteo$TIMESTAMP <- strptime(dat.kiebitz.meteo$TIMESTAMP, "%Y-%m-%d %H:%M:%S")
#sort by date
dat.kiebitz.meteo<-dat.kiebitz.meteo[order(dat.kiebitz.meteo$TIMESTAMP),]

#EC04 Kiebitz Rain data
i=1
for(i in c(1:length(files_kiebitz_rain))) {
  filename.dat <- files_kiebitz_rain[i]
  dat.rain.tmp <- read.csv(paste(dir_kiebitz_rain, filename.dat, sep="/"), header=FALSE,
                            skip=4, stringsAsFactors = F, na.strings=c("NAN", "NA"),
                            col.names=colnames(read.csv(paste(dir_kiebitz_rain, 
                                                              filename.dat, sep="/"), 
                                                        header=T, skip=1, nrows=5))) 
  if(filename.dat==files_kiebitz_rain[1]) {
    dat.kiebitz.rain <- dat.rain.tmp
  } else {
    dat.kiebitz.rain <- rbind(dat.kiebitz.rain, dat.rain.tmp)
  }
}

#convert timestamp
dat.kiebitz.rain$TIMESTAMP <- strptime(dat.kiebitz.rain$TIMESTAMP, "%Y-%m-%d %H:%M:%S")
#sort by date
dat.kiebitz.rain<-dat.kiebitz.rain[order(dat.kiebitz.rain$TIMESTAMP),]

#EC04 Kiebitz Soil data
i=1
for(i in c(1:length(files_kiebitz_soil))) {
  filename.dat <- files_kiebitz_soil[i]
  dat.soil.tmp <- read.csv(paste(dir_kiebitz_soil, filename.dat, sep="/"), header=FALSE,
                            skip=4, stringsAsFactors = F, na.strings=c("NAN", "NA"),
                            col.names=colnames(read.csv(paste(dir_kiebitz_soil, 
                                                              filename.dat, sep="/"), 
                                                        header=T, skip=1, nrows=5)))
  if(filename.dat==files_kiebitz_soil[1]) {
    dat.kiebitz.soil <- dat.soil.tmp
  } else {
    dat.kiebitz.soil <- rbind(dat.kiebitz.soil, dat.soil.tmp)
  }
}

#convert timestamp
dat.kiebitz.soil$TIMESTAMP <- strptime(dat.kiebitz.soil$TIMESTAMP, "%Y-%m-%d %H:%M:%S")
#sort by date
dat.kiebitz.soil<-dat.kiebitz.soil[order(dat.kiebitz.soil$TIMESTAMP),]


#get data to the same length
dat.meteo.merge<-merge(dat.beton.meteo, dat.kiebitz.meteo, 
                       by="TIMESTAMP", #merge by timestamp
                       all = T, #keep all rows and add NA for missing data
                       suffixes = c("_beton","_kiebitz")) #set suffix for new dataframe

dat.rain.merge<-merge(dat.beton.meteo, dat.kiebitz.rain, 
                      by="TIMESTAMP", #merge by timestamp
                      all=T) #keep all rows and add NA for missing data
dat.rain.merge<-dat.rain.merge[,c(1,2,16)]

dat.soil.merge<-merge(dat.beton.meteo, dat.kiebitz.soil,
                      by="TIMESTAMP", #merge by timestamp
                      all=T) #keep all rows and add NA for missing data
dat.soil.merge<-dat.soil.merge[,c(1,2,16:33)]
#change TIMESTAMP to POSIXct
dat.meteo.merge$TIMESTAMP<-as.POSIXct(dat.meteo.merge$TIMESTAMP)
dat.rain.merge$TIMESTAMP<-as.POSIXct(dat.rain.merge$TIMESTAMP)
dat.soil.merge$TIMESTAMP<-as.POSIXct((dat.soil.merge$TIMESTAMP))

#cut to even time
dat.meteo.merge<-dat.meteo.merge[dat.meteo.merge$TIMESTAMP>="2021-07-21 10:30:00 GMT",]
dat.rain.merge<-dat.rain.merge[dat.rain.merge$TIMESTAMP>="2021-07-21 10:30:00 GMT",]
dat.soil.merge<-dat.soil.merge[dat.soil.merge$TIMESTAMP>="2021-07-21 10:30:00 GMT",]
