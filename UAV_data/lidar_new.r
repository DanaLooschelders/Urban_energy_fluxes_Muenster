#lidar data
setwd("C:/Users/Dana/sciebo/Hafen_Projekt_Lidar/")
setwd("D:/DTM_Triangular_irregular_network_new/")
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
hillshade<-raster("TIN_Hillshade.tif")
spplot(hillshade)

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

setwd("D:/")
CHM<-raster("chm_tri_filled.tif")
spplot(CHM)
mapview(CHM)+mapview(beton_polys[7])
library(raster)
?mask
CHM_kiebitz<-clip(mos, kiebitz_polys[7])
CHM_kiebitz<-raster::mask(CHM, kiebitz_polys[7])
spplot(CHM_kiebitz)

CHM_beton<-raster::mask(CHM, beton_polys[7])
spplot(CHM_beton)
class(beton_polys)
st_area(beton_polys[8])

install.packages("geosphere")
require(geosphere)
beton_poly_sf<-st_as_sf(beton_polys)
st_area(beton_poly_sf)
#80: 4561.715 

4561.715*0.0001

kiebitz_poly_sf<-st_as_sf(kiebitz_polys)
st_area(kiebitz_poly_sf)
2403.800*0.0001
#z0
z0<-CHM*0.1
z0_kiebitz<-raster::mask(z0, kiebitz_polys[7])
spplot(z0_kiebitz)

round(mean(values(z0_kiebitz), na.rm=T),5) #
round(sd(values(z0_kiebitz), na.rm=T),5) #

z0_beton<-raster::mask(z0, beton_polys[7])
spplot(CHM_beton)


round(mean(values(z0_beton), na.rm=T),5) #0.00275
round(sd(values(z0_beton), na.rm=T),5) #0.0256

####NDVI ####
setwd("D:/")
drone_2021<-stack(x = "CalmCity_Multispectral_2021.tif")
names(drone_multi)
#probably
#Blue (B): 450 nm ± 16 nm 
#green (G): 560 nm ± 16 nm 
#red (R): 650 nm ± 16 nm
#red edge (RE): 730 nm ± 16 nm
#near-infrared (NIR): 840 nm ± 26 nm

#check
plot(drone_2021)

#NDVI = (NIR - Red) / (NIR + Red)
NDVI_drone_2021<-(drone_2021[[5]]-drone_2021[[3]])/(drone_2021[[5]]+drone_2021[[3]])

NDVI_beton<-raster::mask(NDVI_drone_2021, beton_polys[7])
round(mean(values(NDVI_beton), na.rm=T),5) #
round(sd(values(NDVI_beton), na.rm=T),5) #

NDVI_grass<-raster::mask(NDVI_drone_2021, kiebitz_polys[7])
round(mean(values(NDVI_grass), na.rm=T),5) #
round(sd(values(NDVI_grass), na.rm=T),5) #

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
