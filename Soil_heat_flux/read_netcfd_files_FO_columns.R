#FODS data
library(ncdf4)
library(reshape2)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyr)
library(SiZer)
library(beepr)

####grass column####
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/11_FODS_Columns/FO-column-grass/FO-column-grass/final")
files<-list.files(pattern="nc")
#create output lists
FO_grass_list<-vector(mode='list', length=length(files))
FO_grass_only_temp<-vector(mode='list', length=length(files))
names(FO_grass_list)<-files
names(FO_grass_only_temp)<-files
  
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
          #read out starting time
          start_time<-substr(ncatt_get(nc_tmp, "time")$units, start=15, stop=33)
          #close file
          nc_close(nc_tmp)
          #create nested list
          temp_list<-list(cal_temp, LAF, unheated_PVC, x_dim, y_dim, z_dim)
          FO_grass_list[[i]]<-temp_list
          names(FO_grass_list[[i]])<-names
          #create list with only temp with transposed cal temp
          FO_grass_only_temp[[i]]<-t(cal_temp)
          names(FO_grass_only_temp)[i]<-start_time
        }else{}
      }else{}
}


####concrete column####
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/11_FODS_Columns/FO-column-concrete/FO-column-concrete/final")

files<-list.files(pattern="nc")
#create output list
FO_concrete_list<-vector(mode='list', length=length(files))
FO_concrete_only_temp<-vector(mode='list', length=length(files))
names(FO_concrete_list)<-files
names(FO_concrete_only_temp)<-files

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
      #read out starting time
      start_time<-substr(ncatt_get(nc_tmp, "time")$units, start=15, stop=33)
      #close file
      nc_close(nc_tmp)
      #create nested list
      temp_list<-list(cal_temp, LAF, unheated_PVC, x_dim, y_dim, z_dim)
      FO_concrete_list[[i]]<-temp_list
      names(FO_concrete_list[[i]])<-names
      #create list with only temp with transposed cal temp
      FO_concrete_only_temp[[i]]<-t(cal_temp)
      names(FO_concrete_only_temp)[i]<-start_time
    }else{}
  }else{}
}
beep()
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
  theme_bw()
  ggtitle(label="FO Column Concrete")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_concrete.png")

####find breaking point between soil and atmosphere####
####grass
df_grass_t<-data.frame(t(df_grass)) #transpose
df_grass_t<-df_grass_t[-c(200:201),] #remove date rows
df_grass_t$height<-rownames(df_grass_t)

ymin<-as.numeric(min(unlist(df_grass_t[,1:198])))
ymax<-as.numeric(max(unlist(df_grass_t[,1:198])))

#visualize
plot(as.numeric(df_grass_t$height), as.numeric(df_grass_t$X2),
     type="l", ylim=c(ymin-1, ymax+5), xlab="Height [m]",
     ylab="Temperature [°C]")

for(i in 3:ncol(df_grass_t)-1){
  lines(df_grass_t$height, df_grass_t[,i])
}
#create output vector
changepoint_grass<-as.vector(rep(NA, ncol(df_grass_t)-1))
#find changing point for every function
for(i in 2:ncol(df_grass_t)-1){
print(i)
  temp_res<-piecewise.linear(x = as.numeric(df_grass_t$height),
                 y =  as.numeric(df_grass_t[,i]), 
                 middle = 1)
changepoint_grass[i]<-temp_res[[1]]
rm(temp_res)
}
#plot
plot(changepoint_grass, type="l")
threshold_grass<-mean(changepoint_grass) #0.5361664 
#take mean change point as air/soil threshold --> tbd

#plot as heatmap with threshold
ggplot(df_grass_long, aes(time, key)) +
  geom_tile(aes(fill=value), height=0.005) + #
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  geom_hline(aes(yintercept=threshold_grass, col="Boundary"))+
  scale_color_manual(values = c("black"))+
  ggtitle(label="FO Column Grass")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_grass_threshold.png")

#subset dataframe to only soil values
df_grass_soil<-df_grass_t[df_grass_t$height<=threshold_grass,]
####concrete  
df_concrete_t<-data.frame(t(df_concrete)) #transpose
df_concrete_t<-df_concrete_t[-c(196:197),] #remove date rows
df_concrete_t$height<-rownames(df_concrete_t)

ymin<-as.numeric(min(unlist(df_concrete_t[,1:198])))
ymax<-as.numeric(max(unlist(df_concrete_t[,1:198])))

#visualize
plot(as.numeric(df_concrete_t$height), as.numeric(df_concrete_t$X2),
     type="l", ylim=c(ymin-1, ymax+1), xlab="Height [m]",
     ylab="Temperature [°C]")

for(i in 3:ncol(df_concrete_t)-1){
  lines(df_concrete_t$height, df_concrete_t[,i])
}

#create output vector
changepoint_concrete<-as.vector(rep(NA, ncol(df_concrete_t)-1))
#find changing point for every function
for(i in 2:ncol(df_concrete_t)-1){
  print(i)
  temp_res<-piecewise.linear(x = as.numeric(df_concrete_t$height),
                             y =  as.numeric(df_concrete_t[,i]), 
                             middle = 1)
  changepoint_concrete[i]<-temp_res[[1]]
  rm(temp_res)
}
#plot
plot(changepoint_concrete, type="l")
threshold_concrete<-mean(changepoint_concrete) #0.5336696
#take mean change point as air/soil threshold --> tbd

#plot as heatmap
ggplot(df_concrete_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  geom_hline(aes(yintercept=threshold_concrete, col="Boundary"))+
  scale_color_manual(values = c("black"))+
  theme_bw()+
  ggtitle(label="FO Column Concrete")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_concrete_with_Threshold.png")

#subset dataframe to only soil values
df_concrete_soil<-df_concrete_t[df_concrete_t$height<=threshold_concrete,]

####Thermal diffusivity####
####for concrete
z = as.vector(df_grass_soil$height)
T = as.vector(df_grass_soil$X2) #??????????
time = dg["time"].to_numpy()  #??????????

#create the gradiants 
dT = diff(as.numeric(df_grass_soil$X2)) #Temperature
dz = diff(as.numeric(df_grass_soil$height)) #height

#Python code
#T(t) as vector with all temperature for definite time
#z(t) as vector with all height for definite time

#e = {}
#steigungen = []

#for t in d: #loop through times
 # Tt, zt = d[t]
#Tt = Tt[4:]
#zt = zt[4:]
# Nehme 0.075m als Schneehöhe an.
#idx = find_nearest(zt, 0.075)
# z Werte für Fit
#fit_z = zt[idx-2:idx+3]
#fit_T = Tt[idx-2:idx+3]
# dT/dz = m * z + b
# Primitive of dT/dz
# => T = m/2 * z**2 + b*z + c = a * z**2 + b * z + c
# => Fitte Polynom zweiter Ordnung an T(z)
# dann ist d²T/dz² = 2*a
#a, b, c = np.polyfit(fit_z, fit_T, 2)
sec_poly<-poly(dT, dz, degree = 2)
plot(sec_poly)
m=2*sec_poly
plot(m)
#m = 2*a
#e[t] = m
#steigungen.append(m)

