#FODS data
library(ncdf4)
library(reshape2)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyr)

####grass column####
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/11_FODS_Columns/FO-column-grass/FO-column-grass/final")
files<-list.files(pattern="nc")
#create output list
FO_grass_list<-vector(mode='list', length=length(files))
names(FO_grass_list)<-files
  for (i in 1:length(files)){
    print(i)
      #check if file exists
      if(file.exists(files[i])==TRUE && !is.null(files[i])){
        #check if file is empty (if empty, skip)
        info=file.info(files[i])
        if(info$size!=0){
          #open file
          nc_tmp = nc_open(paste0(files[i]))
          #read out parameters and write into list
          names<-attributes(nc_tmp$var)$names
          cal_temp <- data.frame(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[1]))
          LAF = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[2]))
          unheated_PVC = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[3]))
          x_dim = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[4]))
          y_dim = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[5]))
          z_dim = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[6]))
          #close file
          nc_close(nc_tmp)
          #create nested list
          temp_list<-list(cal_temp, LAF, unheated_PVC, x_dim, y_dim, z_dim)
          FO_grass_list[[i]]<-temp_list
          names(FO_grass_list[[i]])<-names
        }else{}
      }else{}
  }
####concrete column####
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/11_FODS_Columns/FO-column-concrete/FO-column-concrete/final")

files<-list.files(pattern="nc")
#create output list
FO_concrete_list<-vector(mode='list', length=length(files))
names(FO_concrete_list)<-files
for (i in 1:length(files)){
  print(i)
  #check if file exists
  if(file.exists(files[i])==TRUE && !is.null(files[i])){
    #check if file is empty (if empty, skip)
    info=file.info(files[i])
    if(info$size!=0){
      #open file
      nc_tmp = nc_open(paste0(files[i]))
      #read out parameters and write into list
      names<-attributes(nc_tmp$var)$names
      cal_temp <- data.frame(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[1]))
      LAF = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[2]))
      unheated_PVC = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[3]))
      x_dim = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[4]))
      y_dim = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[5]))
      z_dim = as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[6]))
      #close file
      nc_close(nc_tmp)
      #create nested list
      temp_list<-list(cal_temp, LAF, unheated_PVC, x_dim, y_dim, z_dim)
      FO_concrete_list[[i]]<-temp_list
      names(FO_concrete_list[[i]])<-names
    }else{}
  }else{}
}

####data exploration####
#access metadata for grass
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/11_FODS_Columns/FO-column-grass/FO-column-grass/final")
files<-list.files(pattern="nc")
file<-nc_open(filename=files[2])
print(file)
nc_close(filename=files[2])
#access metadata for concrete

head(FO_concrete_list[[2]]$LAF)

table(FO_concrete_list[[2]]$unheated_PVC)
length(FO_concrete_list[[2]]$unheated_PVC)

head(FO_concrete_list[[2]]$cal_temp)
dim(FO_concrete_list[[2]]$cal_temp)

#time var:  time  Size:149 units: seconds since 2021-07-29 15:00:24
#dt: 24s (-> 60*60/24 = 150)
#dLAF: 0.254
#unheated_PVC: wrap_1cm;wrap_2cm;wrap_5cm;wrap_8cm
#length(FO_concrete_list[[1]]$LAF)
#60*60/24
#x y z
head(FO_concrete_list[[1]]$x)
tail(FO_concrete_list[[1]]$x)

head(FO_concrete_list[[1]]$y)
tail(FO_concrete_list[[1]]$y)

head(FO_concrete_list[[1]]$z)
tail(FO_concrete_list[[1]]$z)
length(FO_concrete_list[[1]]$z)

plot(FO_concrete_list[[1]]$unheated_PVC)

#test plot concrete
dat<-data.frame(t(FO_concrete_list[[3]]$cal_temp))
colnames(dat)<-FO_concrete_list[[3]]$z
dat$time<-seq(from=1, 60*60, by=24)

dat_long<-gather(data = dat, key, value, -time)
dat_long$key<-as.numeric(dat_long$key)
ggplot(dat_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c()+
  theme_bw()

#test plot grass
dat<-data.frame(t(FO_grass_list[[3]]$cal_temp))

colnames(dat)<-FO_grass_list[[3]]$z
dat$time<-seq(from=1, 60*60, by=24)

dat_long<-gather(data = dat, key, value, -time)
dat_long$key<-as.numeric(dat_long$key)
ggplot(dat_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c()+
  theme_bw()

####aggregate and subset data####
#grass
#output data frame
df_grass <- data.frame(matrix(ncol = dim(FO_grass_list[[300]]$cal_temp)[1] , #column for every space var
                        nrow = length(FO_grass_list)))  #row for aggregated time vars
colnames(df_grass)<-FO_grass_list[[300]]$z
df_grass$file<-names(FO_grass_list) #file identifier for time

#aggregate to one hour 
for(i in names(FO_grass_list)){ #loop through files
  print(i)
  if(dim(FO_grass_list[[i]]$cal_temp)[2]>=135){ #check if recording is continious, otherwise skip (no more than 10% missing)
  df_grass[df_grass$file==i,1:dim(FO_grass_list[[i]]$cal_temp)[1]]<-rowMeans(FO_grass_list[[i]]$cal_temp, na.rm = T)
  }else{} #skip
}

#subset to column height of 1m
which(colnames(df_grass)=="0.998")
df_grass<-df_grass[,c(1:199,378)] 
#get nchars for subset
df_grass$file[1]
nchar("FO-column-grass_final_") #22
nchar("FO-column-grass_final_20210729-1400") #35
#rename file to just time
df_grass$file<-substr(df_grass$file, start=23, stop = 35)
#convert time to POSIXct
df_grass$time<-strptime(df_grass$file, format="%Y%m%d-%H%M")
df_grass$time<-as.POSIXct(df_grass$time)
#check how many NA rows there are
any(is.na(rowSums(df_grass[,1:199])))
which(is.na(rowSums(df_grass[,1:199]))) #only first
df_grass<-df_grass[-1,]#drop first row
#reshape into long format for plotting
df_grass_long<-gather(data = df_grass, key, value, -time)
#transform class of vars to numeric
df_grass_long$key<-as.numeric(df_grass_long$key)
df_grass_long$value<-as.numeric(df_grass_long$value)
#get mean difference in height
mean(diff(unique(df_grass_long$key)), na.rm=T)#0.005040404
#plot as heatmap
ggplot(df_grass_long, aes(time, key)) +
  geom_tile(aes(fill=value), height=0.005) + #
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  ggtitle(label="FO Column Grass")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_grass.png")

#####concrete
#output data frame
df_concrete <- data.frame(matrix(ncol = dim(FO_concrete_list[[300]]$cal_temp)[1] , #column for every space var
                              nrow = length(FO_concrete_list)))  #row for aggregated time vars
colnames(df_concrete)<-FO_concrete_list[[300]]$z
df_concrete$file<-names(FO_concrete_list) #file identifier for time

#aggregate to one hour 
for(i in names(FO_concrete_list)){ #loop through files
  print(i)
  if(dim(FO_concrete_list[[i]]$cal_temp)[2]>=135){ #check if recording is continious, otherwise skip (no more than 10% missing)
    df_concrete[df_concrete$file==i,1:dim(FO_concrete_list[[i]]$cal_temp)[1]]<-rowMeans(FO_concrete_list[[i]]$cal_temp, na.rm = T)
  }else{} #skip
}

#subset to column height of 1m
which(colnames(df_concrete)=="0.995")
df_concrete<-df_concrete[,c(1:195,377)] 
#get nchars for subset
df_concrete$file[1]
nchar("FO-column-concrete_final_") #25
nchar("FO-column-concrete_final_20210729-1400") #38
#rename file to just time
df_concrete$file<-substr(df_concrete$file, start=26, stop = 38)
#convert time to POSIXct
df_concrete$time<-strptime(df_concrete$file, format="%Y%m%d-%H%M")
df_concrete$time<-as.POSIXct(df_concrete$time)
#check how many NA rows there are
any(is.na(rowSums(df_concrete[,1:195])))
which(is.na(rowSums(df_concrete[,1:195]))) #only first
df_concrete<-df_concrete[-c(1,594),]#drop first and last row
#reshape into long format for plotting
df_concrete_long<-gather(data = df_concrete, key, value, -time)
#transform class of vars to numeric
df_concrete_long$key<-as.numeric(df_concrete_long$key)
df_concrete_long$value<-as.numeric(df_concrete_long$value)
#get mean difference in height
mean(diff(unique(df_concrete_long$key)), na.rm=T)#0.005128866
#plot as heatmap
ggplot(df_concrete_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  ggtitle(label="FO Column Concrete")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_concrete.png")
