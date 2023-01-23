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
library(cmna)
library(plyr)
library(bigsnpr)


####aggregated data####
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
beep()
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

#cut to 10 cm above threshold of 0.4722124
threshold_grass<-0.5722124 #0.47 (from variance change)
#remove last two cloumns
df_grass_short<-subset(df_grass, select = -c(file, time))
#get index of columns over threshold
cols<-which(as.numeric(colnames(df_grass_short[, -length(df_grass_short)]))>=threshold_grass)
#remove those columns
df_grass_short<-df_grass_short[,-cols]

####QAQC aggregated data####
df_grass_4QA<-df_grass_short
#check if there physically unrealistic temperatures
range(df_grass_4QA)
#13.29028 35.22602
df_hist<-data.matrix(df_grass_4QA)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
png(filename = "Histogramm_FO_grass_Soil_Temp.png",
    height = 4960, width=7016)
hist(df_hist, 
     main = "Histogramm of FO grass - Soil Temp",
     xlab = "Temperature [°C]")
dev.off()

#check if there are spikes in time
diff_grass_time<-diff(as.matrix(df_grass_4QA))
range(diff_grass_time)
#-4.237244  6.495273
hist(diff_grass_time, breaks = 100)

#check if there are spikes in space
diff_grass_space<-diff(as.matrix(t(df_grass_4QA)))
range(diff_grass_space)
#-6.367069  1.968788
hist(diff_grass_space, breaks=100)

####not aggregated data####
FO_grass_temp_time<-vector(mode='list', length=length(files))
for(i in 1:length(FO_grass_only_temp)){
  print(i)
  #get starting datetime
  start_date<-as.POSIXct(names(FO_grass_only_temp)[i])
  #get time seq from starting time
  time<-seq.POSIXt(from=start_date, by= "24 sec", 
                   length.out=dim(FO_grass_only_temp[[i]])[1])
  #convert matrix to dataframe
  FO_grass_temp_time[[i]]<-as.data.frame(FO_grass_only_temp[[i]])
  #set colnames to correspond to height
  colnames(FO_grass_temp_time[[i]])<-FO_grass_list[[i]]$z
  #add time as variable
  FO_grass_temp_time[[i]]$time<-time
}
#rind list to one dataframe and fill missing cols with NA
FO_grass_temp_time_df<-rbind.fill(FO_grass_temp_time)
#order columns
FO_grass_temp_time_df_order<-FO_grass_temp_time_df[ ,order(colnames(FO_grass_temp_time_df))]
#height of measurements shiftet slightly during measurements
#if difference in heights is less than a threshold --> merge
#cols_to_keep<-which(diff(as.numeric(colnames(FO_grass_temp_time_df_order)[-length(FO_grass_temp_time_df_order)]))>0.006)
#FO_grass_temp_time_df_order_merged<-FO_grass_temp_time_df_order[,cols_to_keep]

#shorten dataframe to height below 80 cm to reducue computing time
FO_grass_temp_time_df_short<-FO_grass_temp_time_df_order[,1:354]
#if difference less than 0.006 -> merge
FO_grass_merged<-FO_grass_temp_time_df_short
#loop through every second column and compare with column after

for(i in seq(1, length(FO_grass_merged)-2, by=2)){
  print(i) #check
  #if difference smaller than 0.006
  if(diff(c(as.numeric(colnames(FO_grass_merged)[i]),
            as.numeric(colnames(FO_grass_merged)[i+1]))) <0.006){
    #merge those two columns and write result in first column
    FO_grass_merged[,i]<-coalesce(FO_grass_merged[,i], 
                                  FO_grass_merged[,i+1])
    #rename column to mean of the two columns
    colnames(FO_grass_merged)[i]<-as.character(mean(c(as.numeric(colnames(FO_grass_merged)[i]),
                                                      as.numeric(colnames(FO_grass_merged)[i+1]))))
    FO_grass_merged[,i+1]<-NA #set second column to NA
  }else{} #do nothing
}
#use 10cm over threshold of 0.4722124
threshold_grass<-0.5722124 #0.4722124 
#get index of columns over threshold
cols<-which(as.numeric(colnames(FO_grass_merged))>=threshold_grass)
#remove those columns
FO_grass_merged_short<-FO_grass_merged[,-cols]

#drop all columns that are only NA (the ones that were merged previously)
FO_grass_merged_short_cut<-FO_grass_merged_short[colSums(!is.na(FO_grass_merged_short)) > 0]
#QAQC
#check that values are NA
length(which(is.na(FO_grass_merged_short_cut)))

#rename for convenience
FO_grass_df<-FO_grass_merged_short_cut
#get spatial difference of measurements
heights_grass<-diff(as.numeric(colnames(FO_grass_df)))
mean(heights_grass) # 0.0051
sd(heights_grass) #0.00032
plot(heights_grass[74:93])
#add time as col
FO_grass_df$time<-FO_grass_temp_time_df$time
#remove first three rows to get clean timestamp
FO_grass_df<-FO_grass_df[-c(1:3),]
#aggregate to 10 min
FO_grass_10min<-aggregate(FO_grass_df, 
                             list(time_10min=cut(FO_grass_df$time, "10 mins")),
                             mean)
#extract only time
grass_time<-as.POSIXct(FO_grass_10min$time_10min)
#remove column with time
FO_grass_10min<-FO_grass_10min[,-c(1,length(FO_grass_10min))]

####QAQC aggregated data####
FO_grass_10min_4QC<-FO_grass_10min
df_hist<-data.matrix(FO_grass_10min_4QC)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
png(filename = "Histogramm_FO_grass_Soil_Temp_aggregated.png",
    height = 4960, width=7016)
hist(df_hist, 
     main = "Histogramm of FO grass - Soil Temp",
     xlab = "Temperature [°C]")
dev.off()

#check if there are spikes in time
diff_grass_time<-diff(as.matrix(FO_grass_10min_4QC))
range(diff_grass_time)
#-2.266154  6.760256
hist(diff_grass_time, breaks = 100)
#check if there are spikes in space
diff_grass_space<-diff(as.matrix(t(FO_grass_10min_4QC)))
range(diff_grass_space)
#-1.049230  1.832749
hist(diff_grass_space, breaks=100)

####QAQC non-aggregated data####
FO_grass_df_4QC<-FO_grass_df
df_hist<-data.matrix(FO_grass_df_4QC)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
png(filename = "Histogramm_FO_grass_Soil_Temp_notaggregated.png",
    height = 4960, width=7016)
hist(df_hist, 
     main = "Histogramm of FO grass - Soil Temp",
     xlab = "Temperature [°C]")
dev.off()

#check if there are spikes in time
diff_grass_time<-diff(as.matrix(FO_grass_df_4QC))
range(diff_grass_time)
#-2.266154  6.760256
hist(diff_grass_time, breaks = 100)
#check if there are spikes in space
diff_grass_space<-diff(as.matrix(t(FO_grass_df_4QC)))
range(diff_grass_space)
#-1.049230  1.832749
hist(diff_grass_space, breaks=100)

#remove unneccessary objects
rm(FO_grass_df_4QC, FO_grass_10min_4QC, FO_grass_list, FO_grass_only_temp, 
   FO_grass_temp_time,
   FO_grass_temp_time_df_short, FO_grass_temp_time_df_order, FO_grass_temp_time_df, 
   FO_grass_df, FO_grass_df_t)
rm(df_grass, df_grass_4QA, df_grass_long, df_grass_short)
rm(temp_list, nc_tmp, info)
