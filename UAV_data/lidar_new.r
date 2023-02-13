#lidar data
setwd("C:/Users/Dana/sciebo/Hafen_Projekt_Lidar/")
setwd("C:/Users/Dana/Desktop/DTM_inverse_distance_weighting/")
library(raster)
library(rgdal)
library(mapview)
library(RStoolbox)
library(sf)
library(dplyr)
library(sp)
library(stars)

files<-list.files(pattern=".tif")
files_list<-vector(mode='list', length=length(files))
names(files_list)<-files

#"WGS 84 / UTM zone 32N"

#load files
for (i in files){
  tryCatch(expr={
    #read in files and set crs
    files_list[[i]]<-stack(i)
  }, error=function(e){message("Caught an error")})
} 
#check is there are empty files
any(is.null(files_list))

#moasic raster
names(files_list) <- NULL
files_list$fun <- mean
mos <- do.call(mosaic, files_list)

spplot(mos)
mapview(mos,'maxpixels =  7e+06 ')

#DTM with sea level calc
setwd("C:/Users/Dana/sciebo/Hafen_Projekt_Lidar")
library(raster)
dtm_nn<-raster("dtm_over_sea_level_inverse_distance_weighting.tif")
library(mapview)
mapview(dtm_nn)
library(raster)
setwd("C:/Users/Dana/sciebo/ndom/reproduzierbares_Beispiel")

pred_stack<-stack(file="/home/p/p_loet03/uhi/pred_stack_717.grd")
pred_stack<-stack(file="C:/Users/Dana/sciebo/ndom/reproduzierbares_Beispiel/pred_stack_717.grd")
pred_stack<-stack(x="C:/Users/Dana/sciebo/ndom/reproduzierbares_Beispiel/pred_stack_717.grd")
