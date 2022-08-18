#Comparison of LAI from Sentinel-2 and Drone Data
library(raster)
library(mapview)
library(rgdal)
library(tidyverse)
####Drone Data####
#load drone data
setwd("D:/")
drone_2021<-stack(x = "CalmCity_Multispectral_2021.tif")
drone_2022<-stack(x="Hafen_Multispectral.tif")
#NDVI = (NIR - Red) / (NIR + Red)
NDVI_drone_2021<-(drone_2021[[5]]-drone_2021[[3]])/(drone_2021[[5]]+drone_2021[[3]])
NDVI_drone_2022<-(drone_2022[[5]]-drone_2022[[3]])/(drone_2022[[5]]+drone_2022[[3]])
#calculate LAI
LAI_drone_2021<-0.128*exp(NDVI_drone_2021/0.311)
LAI_drone_2022<-0.128*exp(NDVI_drone_2022/0.311)
#resample to resolution of Sentinel
res(LAI_Sentinel_2021)
res(LAI_drone_2021)
LAI_drone_2021_resampled<-resample(LAI_drone_2021, LAI_Sentinel_2021, method="bilinear")
LAI_drone_2022_resampled<-resample(LAI_drone_2022, LAI_Sentinel_2022, method="bilinear")

#plot 2021
mapview(LAI_drone_2021, layer.name="Drone 2021", at=c(seq(0,3, by=0.5)))+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#plot 2022 
mapview(LAI_drone_2022, layer.name="Drone 2022", at=c(seq(0,3, by=0.5)))+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#plot 2021 resampled
mapview(LAI_drone_2021_resampled, layer.name="Drone 2021", at=c(seq(0,3, by=0.5)))+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#plot 2022 resampled
mapview(LAI_drone_2022_resampled, layer.name="Drone 2022", at=c(seq(0,3, by=0.5)))+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

####Satellite Data####
#load shapefile of Muenster
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
ms<-spTransform(gadm, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))
#get extent of study site
polys_ex<-extent(kiebitz_polys+beton_polys)
####Sentinel 2 2021#### 2021-08-15T10:36:29
setwd("C:/00_Dana/Uni/Masterarbeit/LAI_Sentinel/S2B_MSIL2A_20210815T103629_N0301_R008_T32ULC_20210815T134410/S2B_MSIL2A_20210815T103629_N0301_R008_T32ULC_20210815T134410.SAFE/GRANULE/L2A_T32ULC_A023200_20210815T104557/IMG_DATA/R10m")
#read in files
in.files<-list.files(pattern=".jp2")
#convert to tif
out.files <- gsub(".jp2", ".tif", in.files)
for(i in 1:length(in.files)){
  raster::writeRaster(raster(rgdal::readGDAL(in.files[i])),
                      file.path(out.files[i]),
                      overwrite = TRUE)
}
#load bands
B8_2021<-raster("T32ULC_20210815T103629_B08_10m.tif")
B4_2021<-raster("T32ULC_20210815T103629_B04_10m.tif")
#crop to size of MS
B4_crop_2021<-crop(B4_2021, ms)
B8_crop_2021<-crop(B8_2021, ms)
#calculate NDVI Index
#(B8-B4)/(B8+B4)
NDVI_Sentinel_2021<-(B8_crop_2021-B4_crop_2021)/(B8_crop_2021+B4_crop_2021)
#crop to study site
NDVI_Sentinel_crop_2021<-crop(NDVI_Sentinel_2021, polys_ex)
LAI_Sentinel_2021<-0.128*exp(NDVI_Sentinel_crop_2021/0.311)
#plot
mapview(LAI_Sentinel_2021, layer.name="Sentinel 2021", at=c(seq(0,3, by=0.5)))+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#####Sentinel 2 2022#### 2022-08-07 10:25:59
setwd("C:/00_Dana/Uni/Masterarbeit/LAI_Sentinel/S2B_MSIL2A_20220807T102559_N0400_R108_T32ULC_20220807T132122/S2B_MSIL2A_20220807T102559_N0400_R108_T32ULC_20220807T132122.SAFE/GRANULE/L2A_T32ULC_A028305_20220807T103507/IMG_DATA/R10m/")
#read in files
in.files<-list.files(pattern=".jp2")
#convert to tif
out.files <- gsub(".jp2", ".tif", in.files)
for(i in 1:length(in.files)){
  raster::writeRaster(raster(rgdal::readGDAL(in.files[i])),
                      file.path(out.files[i]),
                      overwrite = TRUE)
}
#load bands
B8_2022<-raster("T32ULC_20220807T102559_B08_10m.tif")
B4_2022<-raster("T32ULC_20220807T102559_B04_10m.tif")
#crop to size of MS
B4_crop_2022<-crop(B4_2022, ms)
B8_crop_2022<-crop(B8_2022, ms)
#calculate NDVI Index
#(B8-B4)/(B8+B4)
NDVI_Sentinel_2022<-(B8_crop_2022-B4_crop_2022)/(B8_crop_2022+B4_crop_2022)
#crop to study site
NDVI_Sentinel_crop_2022<-crop(NDVI_Sentinel_2022, polys_ex)
LAI_Sentinel_2022<-0.128*exp(NDVI_Sentinel_crop_2022/0.311)
#plot
mapview(LAI_Sentinel_2022, layer.name="Sentinel 2022", at=c(seq(0,3, by=0.5)))+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

mapview(kiebitz_polys)+mapview(beton_polys)
#surface or wind speed?#

####Planet scope ####
setwd("D:/Hafen_14092021_psscene_analytic_sr_udm2/files/")
planetscope_2021<-stack("20210814_094102_69_2450_3B_AnalyticMS_SR_harmonized_clip.tif")
names(planetscope_2021)
mapview(planetscope_2021)
#crop to study area
crs(planetscope_2021) # +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs
planetscope_2021_crop<-crop(planetscope_2021, polys_ex)
mapview(planetscope_2021_crop)

#calculate NDVI Index
NDVI_planetscope_2021<-(planetscope_2021_crop[[4]]-planetscope_2021_crop[[3]])/(planetscope_2021_crop[[4]]+planetscope_2021_crop[[3]])
mapview(NDVI_planetscope_2021)

LAI_planetscope_2021<-0.128*exp(NDVI_planetscope_2021/0.311)
mapview(LAI_planetscope_2021)
#plot
mapview(LAI_planetscope_2021, layer.name="Planetscope 2021", at=c(seq(0,3, by=0.5)))+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#### for 2022
setwd("D:/Hafen_07082022_psscene_analytic_sr_udm2/files/")
planetscope_2022<-stack("20220807_092939_50_2449_3B_AnalyticMS_SR_harmonized_clip.tif")
names(planetscope_2022)
mapview(planetscope_2022)
#crop to study area
crs(planetscope_2022) # +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs
planetscope_2022_crop<-crop(planetscope_2022, polys_ex)
mapview(planetscope_2022_crop)

#calculate NDVI Index
NDVI_planetscope_2022<-(planetscope_2022_crop[[4]]-planetscope_2022_crop[[3]])/(planetscope_2022_crop[[4]]+planetscope_2022_crop[[3]])
mapview(NDVI_planetscope_2022)

LAI_planetscope_2022<-0.128*exp(NDVI_planetscope_2022/0.311)
mapview(LAI_planetscope_2022)

#plot
mapview(LAI_planetscope_2022, layer.name="Planetscope 2022", at=c(seq(0,3, by=0.5)))+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)
