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
library(lubridate)
library(Hmisc)
library(plyr)
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

####data aggregated to one hour####
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
#cut df_concrete to threshold of 0.53
cols<-which(as.numeric(colnames(df_concrete[, -c(196, 197)]))>=0.53)
#remove those columns
df_concrete_short<-df_concrete[,-cols]

####QAQC aggregated data####
df_concrete_4QA<-subset(df_concrete_short, select = -c(file, time))
#check if there physically unrealistic temperatures
range(df_concrete_4QA)
#13.29028 35.22602
df_hist<-data.matrix(df_concrete_4QA)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
png(filename = "Histogramm_FO_concrete_Soil_Temp.png",
    height = 4960, width=7016)
hist(df_hist, 
     main = "Histogramm of FO Concrete - Soil Temp",
     xlab = "Temperature [°C]")
dev.off()

#check if there are spikes in time
diff_concrete_time<-diff(as.matrix(df_concrete_4QA), na.rm=T)
range(diff_concrete_time)
#-5.754533  6.424854
hist(diff_concrete_time, breaks = 100)

#check if there are spikes in space
diff_concrete_space<-diff(as.matrix(t(df_concrete_4QA)))
range(diff_concrete_space)
#-1.478578  2.624966
hist(diff_concrete_space, breaks=100)

####for non-aggregated data####
#use not aggregated data
FO_concrete_temp_time<-vector(mode='list', length=length(files))
for(i in 1:length(FO_concrete_only_temp)){
  print(i)
  #get starting datetime
  start_date<-as.POSIXct(names(FO_concrete_only_temp)[i])
  #get time seq from starting time
  time<-seq.POSIXt(from=start_date, by= "24 sec", 
                   length.out=dim(FO_concrete_only_temp[[i]])[1])
  #convert matrix to dataframe
  FO_concrete_temp_time[[i]]<-as.data.frame(FO_concrete_only_temp[[i]])
  #set colnames to correspond to height
  colnames(FO_concrete_temp_time[[i]])<-FO_concrete_list[[i]]$z
  #add time as variable
  FO_concrete_temp_time[[i]]$time<-time
}

#rind list to one dataframe and fill missing cols with NA
FO_concrete_temp_time_df<-rbind.fill(FO_concrete_temp_time)
#order columns
FO_concrete_temp_time_df_order<-FO_concrete_temp_time_df[ ,order(colnames(FO_concrete_temp_time_df))]
threshold_concrete<-0.5341675  #0.53 (from variance change)
#get index of columns over threshold
cols<-which(as.numeric(colnames(FO_concrete_temp_time_df_order[, -length(FO_concrete_temp_time_df_order)]))>=threshold_concrete)
#remove those columns
FO_concrete_temp_time_df_short<-FO_concrete_temp_time_df_order[,-cols]
#rename for convenience
FO_concrete_df<-FO_concrete_temp_time_df_short
#remove the others
#get spatial difference of measurements
heights_concrete<-diff(as.numeric(colnames(FO_concrete_df[-length(FO_concrete_df)])))
FO_concrete_df<-FO_concrete_df[-c(1:2),]
#aggregate to 10 min
FO_concrete_10min<-aggregate(FO_concrete_df, 
                              list(time_10min=cut(FO_concrete_df$time, "10 mins")),
                              mean)
#extract only time
concrete_time<-as.POSIXct(FO_concrete_10min$time_10min)
#remove column with time
FO_concrete_10min<-FO_concrete_10min[,-c(1,length(FO_concrete_10min))]

#clear up environment
#rm(FO_concrete_temp_time, FO_concrete_list, FO_concrete_only_temp, FO_concrete_temp_time_df_order)
####QAQC aggregated data####
FO_concrete_10min_4QC<-FO_concrete_10min
df_hist<-data.matrix(FO_concrete_10min_4QC)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
png(filename = "Histogramm_FO_concrete_Soil_Temp_aggregated.png",
    height = 4960, width=7016)
hist(df_hist, 
     main = "Histogramm of FO Concrete - Soil Temp",
     xlab = "Temperature [°C]")
dev.off()

#check if there are spikes in time
diff_concrete_time<-diff(as.matrix(FO_concrete_10min_4QC))
range(diff_concrete_time)
#-2.053447  1.791176
hist(diff_concrete_time, breaks = 100)
#check if there are spikes in space
diff_concrete_space<-diff(as.matrix(t(FO_concrete_10min_4QC))) 
range(diff_concrete_space)
#-2.166154  3.865688
hist(diff_concrete_space, breaks=100)

####plot as heatmap####
ggplot(df_concrete_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  ggtitle(label="FO Column Concrete")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_concrete_10min.png",
       width=297, height=210, units = "mm")

####QAQC non-aggregated data####
FO_concrete_df_4QC<-FO_concrete_df
df_hist<-data.matrix(FO_concrete_df_4QC)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
png(filename = "Histogramm_FO_concrete_Soil_Temp_notaggregated.png",
    height = 4960, width=7016)
hist(df_hist, 
     main = "Histogramm of FO Concrete - Soil Temp",
     xlab = "Temperature [°C]")
dev.off()

#check if there are spikes in time
diff_concrete_time<-diff(as.matrix(FO_concrete_df_4QC))
range(diff_concrete_time)
#-2.053447  1.791176
hist(diff_concrete_time, breaks = 100)
#check if there are spikes in space
diff_concrete_space<-diff(as.matrix(t(FO_concrete_df_4QC))) 
range(diff_concrete_space)
#-2.166154  3.865688
hist(diff_concrete_space, breaks=100)

####plot as heatmap####
ggplot(df_concrete_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  ggtitle(label="FO Column Concrete")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_concrete.png",
       width=297, height=210, units = "mm")

#remove unneccessary objects
rm(FO_concrete_df_4QC, FO_concrete_10min_4QC, FO_concrete_list, FO_concrete_only_temp, 
   FO_concrete_temp_time,
   FO_concrete_temp_time_df_short, FO_concrete_temp_time_df_order, FO_concrete_temp_time_df, 
   FO_concrete_df, FO_concrete_df_t)
rm(df_concrete, df_concrete_4QA, df_concrete_long, df_concrete_short)
rm(temp_list, nc_tmp, info)
