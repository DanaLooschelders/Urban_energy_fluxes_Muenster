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
i=1
#create output list
FO_grass_list<-vector(mode='list', length=length(files))
names(FO_grass_list)<-files
  for (i in 1:length(files)){
      #check if file exists
      if(file.exists(files[i])==TRUE && !is.null(files[i])){
        #check if file is empty (if empty, skip)
        info=file.info(files[i])
        if(info$size!=0){
          #open file
          i=1
          nc_tmp = nc_open(paste0(files[i]))
          #read out parameters and write into list
          cal_temp <- list(data.frame(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[1])))
          LAF = list(as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[2])))
          unheated_PVC = list(as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[3])))
          x_dim = list(as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[4])))
          y_dim = list(as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[5])))
          z_dim = list(as.vector(ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[6])))
          #close file
          nc_close(nc_tmp)
          #create nested list
          FO_grass_list[i]<-list(cal_temp, LAF, unheated_PVC, x_dim, y_dim, z_dim)
          
        }else{}
      }else{}
  }
####concrete column####
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/11_FODS_Columns/FO-column-concrete/FO-column-concrete/final")