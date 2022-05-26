####Rename Files for EddyPro to include beginn of intervall timestamp####
library(stringr)
#set working directory
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/03_Rohdaten_konvertiert/EC04_converted/")
#save file path to use in rename.file
file_path="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/03_Rohdaten_konvertiert/EC04_converted/"
#list files to read in 
files_list=list.files(pattern ="*.csv",recursive = T)
#loop through all the files
for(i in files_list[1:length(files_list)]){
  #read in file to get timestamp
  dat=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                 na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
  #extract timestamp
  timestamp=substr(dat[1,1],1,19)
  #reformat timestamp to replace all sep with underscore
  timestamp=str_replace_all(timestamp, c(" "="_", "-"="_", ":"="_"))
  #define new name
  newname=paste0(substr(i,1, nchar(i)-4),"_",timestamp, ".csv")
  #rename file
  file.rename(paste0(file_path,i),
              paste0(file_path,newname))
  #remove temporary file
  remove(dat)
}

#set completely new names for EC02
#TOA5_CalmCity_EC2_Beton_EC
for(i in files_list[1:length(files_list)]){
  #read in file to get timestamp
  dat=read.table(i, sep = ",", dec = ".", header = F, skip = 4,
                 na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
  #extract timestamp
  timestamp=substr(dat[1,1],1,19)
  #reformat timestamp to replace all sep with underscore
  timestamp=str_replace_all(timestamp, c(" "="_", "-"="_", ":"="_"))
  #define new name
  newname=paste0("TOA5_CalmCity_EC4_Kiebitz_EC","_",timestamp, ".csv")
  #rename file
  file.rename(paste0(file_path,i),
              paste0(file_path,newname))
  #remove temporary file
  remove(dat)
}
#####copy sonic temperature to another column to use as ambient temp####
#list files to read in 
files_list=list.files(pattern ="*.dat")
#loop through all the files
for(i in files_list){
  #set working directory
  setwd("Z:/Studienprojekte/BioAtmo_HalleMünsterland_2021/data_converted_renamed-for-eddypro/")
  #read in file
  dat=read.table(i, sep = ",", dec = ".", header = F,
                 na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
  #copy column with temp 
  dat$dummy_T_amb=dat[,7]
  #set new working directory to write files into
  setwd("Z:/Studienprojekte/BioAtmo_HalleMünsterland_2021/data_converted_renamed-for-eddypro_double_temp/")
  #write file with same file name into new folder
  write.table(dat,file =i, sep = ",", dec = ".", row.names = F, header=F)
}

####read in one testfile####
setwd("Z:/Studienprojekte/BioAtmo_HalleMünsterland_2021/data_converted/data_converted_renamed-for-eddypro_test_subset/")
#read in file and skip header
dat=read.table(file="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00.csv", 
               sep = ",", dec = ".", header = F,skip=4,
               na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#read in header line
line=read.table(file="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00.csv", 
               sep = ",", dec = ".", header = F,skip=1,nrows = 1,
               na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
colnames(dat)=line
#replace NANs with -9999
dat[is.na(dat)] = -9999

#write File
write.table(dat,file ="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00_noNAN_singleheader.csv", 
            sep = ",", dec = ".", row.names = F)

####test files from local file####
setwd("C:/00_Dana/Uni/2. Mastersemester/Messtechnik_Praktikum_Klima/test/normal")
#file 1
#read in file and skip header
dat=read.table(file="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00.csv", 
               sep = ",", dec = ".", header = F,skip=1,
               na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#read in header line
line=read.table(file="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00.csv", 
                sep = ",", dec = ".", header = F,skip=0,nrows = 1,
                na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#add empty columns to first line to match dataframe
line[,9:17]=NA
dat=rbind(line, dat)
#replace NANs with -9999
dat[is.na(dat)] = -9999
setwd("C:/00_Dana/Uni/2. Mastersemester/Messtechnik_Praktikum_Klima/test/noNAN_r/")
#write File
write.table(dat,file ="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00_noNAN.csv", 
            sep = ",", dec = ".", row.names = F, col.names = F)
#file 2
setwd("C:/00_Dana/Uni/2. Mastersemester/Messtechnik_Praktikum_Klima/test/normal")
#read in file and skip header
dat=read.table(file="TOA5_CalmCity_EC4_EC_793_2021_06_03_01_00_00.csv", 
               sep = ",", dec = ".", header = F,skip=1,
               na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#read in header line
line=read.table(file="TOA5_CalmCity_EC4_EC_793_2021_06_03_01_00_00.csv", 
                sep = ",", dec = ".", header = F,skip=0,nrows = 1,
                na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#add empty columns to first line to match dataframe
line[,9:17]=NA
dat=rbind(line, dat)
#replace NANs with -9999
dat[is.na(dat)] = -9999

setwd("C:/00_Dana/Uni/2. Mastersemester/Messtechnik_Praktikum_Klima/test/noNAN_r/")
#write File
write.table(dat,file ="TOA5_CalmCity_EC4_EC_793_2021_06_03_01_00_00_noNAN.csv", 
            sep = ",", dec = ".", row.names = F, col.names = F)
####test file with single header####
setwd("C:/00_Dana/Uni/2. Mastersemester/Messtechnik_Praktikum_Klima/test/normal")
#file 1
#read in file and skip header
dat=read.table(file="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00.csv", 
               sep = ",", dec = ".", header = F,skip=4,
               na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#read in header line
line=read.table(file="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00.csv", 
                sep = ",", dec = ".", header = F,skip=1,nrows = 1,
                na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#add empty columns to first line to match dataframe
#line[,9:17]=NA
colnames(dat)=line
#replace NANs with -9999
dat[is.na(dat)] = -9999
setwd("C:/00_Dana/Uni/2. Mastersemester/Messtechnik_Praktikum_Klima/test/noNAN_r/")
#write File
write.table(dat,file ="TOA5_CalmCity_EC4_EC_792_2021_06_03_00_30_00_noNAN.csv", 
            sep = ",", dec = ".", row.names = F, col.names = F)
#file 2
setwd("C:/00_Dana/Uni/2. Mastersemester/Messtechnik_Praktikum_Klima/test/normal")
#read in file and skip header
dat=read.table(file="TOA5_CalmCity_EC4_EC_793_2021_06_03_01_00_00.csv", 
               sep = ",", dec = ".", header = F,skip=4,
               na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#read in header line
line=read.table(file="TOA5_CalmCity_EC4_EC_793_2021_06_03_01_00_00.csv", 
                sep = ",", dec = ".", header = F,skip=1,nrows = 1,
                na.strings = c("<NA>", "NAN"), stringsAsFactors = FALSE)
#add empty columns to first line to match dataframe
#line[,9:17]=NA
colnames(dat)=line
#replace NANs with -9999
dat[is.na(dat)] = -9999

setwd("C:/00_Dana/Uni/2. Mastersemester/Messtechnik_Praktikum_Klima/test/noNAN_r/")
#write File
write.table(dat,file ="TOA5_CalmCity_EC4_EC_793_2021_06_03_01_00_00_noNAN.csv", 
            sep = ",", dec = ".", row.names = F, col.names = F)
