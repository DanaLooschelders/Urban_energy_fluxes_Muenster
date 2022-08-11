library(tidyverse)
library(openair)
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

for(i in c(1:length(files_list))) {
  filename.dat <- files_list[i]
  dat.wind.tmp <- read.table(filename.dat, 
                             sep = ",", 
                             dec = ".", header = F, skip=4, 
                             col.names = colnames(read.table(filename.dat, sep = ",", skip=1,
                                                             dec = ".", header = T, nrows=1,
                                                             stringsAsFactors = FALSE)),
                             na.strings = c("NAN", "-99999"), stringsAsFactors = FALSE)
  if(filename.dat==files_list[1]) {
    dat.wind <- dat.wind.tmp
  } else {
    dat.wind <- rbind(dat.wind, dat.wind.tmp)
  }
}
