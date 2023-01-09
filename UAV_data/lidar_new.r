#lidar data
setwd("D:/CHM_point-to-raster")
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

