#UAV data
#read in data
#install.packages("tiff")
library(tiff)
library(raster)
library(sp)

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/04_UAV_data/UAS Data for Analysis/UAS Data for Analysis")
gli<-raster("CalmCity_GreenLeafIndex_GLI.tif")
height<-raster("CalmCity_CanopyHeightModel.tif")
veg<-raster("CalmCity_Vegetation_Classification.tif")

spplot(gli)
spplot(height)
spplot(veg)

res(height)
