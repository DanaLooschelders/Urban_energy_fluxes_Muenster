#UAV data
#read in data
#install.packages("tiff")
library(tiff)
library(raster)
library(sp)
library(rgdal)

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/04_UAV_data/UAS Data for Analysis/UAS Data for Analysis")
gli<-raster("CalmCity_GreenLeafIndex_GLI.tif")
height<-raster("CalmCity_CanopyHeightModel.tif")
veg<-raster("CalmCity_Vegetation_Classification.tif")
#check visually
spplot(gli)
spplot(height)
spplot(veg)
#check resolution
res(height)
#check that crs match polygons
crs(gli)
crs(height)
crs(veg)
crs(beton_polys_df[8,])
# cut out flux footprint with 80% polygon
####Beton####
# coerce to SpatialPolygonsDataFrame
beton_polys_df<-as(beton_polys, "SpatialPolygonsDataFrame")
## Gli
# crop and mask
mapview(gli)+mapview(beton_polys_df[7,])
gli_crop <- crop(gli, extent(beton_polys_df[7,]))
spplot(gli_crop) #check
gli_mask <- mask(gli_crop, beton_polys_df[7,])
spplot(gli_mask) #check
spplot(beton_polys_df[7,])
#save to file
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/UAV")
writeRaster(gli_mask, filename = "gli_beton.grd")
#mean and sd
mean(values(gli_mask), na.rm=T) #0.0035
sd(values(gli_mask), na.rm=T) #0.018

##height
mapview(height)+mapview(beton_polys_df[7,])
height_crop <- crop(height, extent(beton_polys_df[7,]))
spplot(height_crop, axes=T) #check
height_mask <- mask(height_crop, beton_polys_df[7,])
spplot(height_mask) #check
spplot(beton_polys_df[7,])
#save to file
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/UAV")
writeRaster(height_mask, filename = "height_beton.grd")
#mean and sd
mean(values(height_mask), na.rm=T) #0.0096
sd(values(height_mask), na.rm=T) # 0.09

#plot pretty
plot(height_mask, axes=T, xlim=c(406500, 406700))
title("Surface Height for EC02 Footprint \n(80% Contribution)")

##veg
mapview(veg)+mapview(beton_polys_df[7,])
veg_crop <- crop(veg, extent(beton_polys_df[7,]))
spplot(veg_crop) #check
veg_mask <- mask(veg_crop, beton_polys_df[7,])
spplot(veg_mask) #check
spplot(beton_polys_df[7,])
#save to file
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/UAV")
writeRaster(veg_mask, filename = "veg_beton.grd")
#mean and sd
mean(values(veg_mask), na.rm=T) #1.02
sd(values(veg_mask), na.rm=T) # 0.15

####kiebitz####