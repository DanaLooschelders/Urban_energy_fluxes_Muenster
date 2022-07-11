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
#plot pretty -> work in progress
scale = list("SpatialPolygonsRescale", layout.scale.bar(), 
             offset = c(406510,5756030), scale = 500, fill=c("transparent","black"))
text1 = list("sp.text", c(406510,5756030), "0")
text2 = list("sp.text", c(406540,5756030), "500 m")
arrow = list("SpatialPolygonsRescale", layout.north.arrow(), 
             offset = c(406500,5756020), scale = 400)
spplot(gli_mask,  sp.layout=list(scale,text1,text2,arrow),
       scales=list(draw = TRUE), 
       legend = list(right = list(fun = mapLegendGrob(layout.north.arrow()))), 
       main="Green Leaf Index \nfor EC02 Footprint \n(80% Contribution)") #check
#plot simple
png(filename="gli_beton_simple.png",
    height = 300, width=400 )
spplot(gli_mask, main="EC02 - Green Leaf Index [-1 to 1]")
dev.off()
#plot with map
gli_beton_mapview<-mapview(gli_mask)
mapshot(gli_beton_mapview, 
        file = "gli_beton_mapview.png") #save

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
spplot(height_mask, scales=list(draw = TRUE)) #check
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

#plot simple
png(filename="height_beton_simple.png",
    height = 300, width=400)
spplot(height_mask, main="EC02 - Canopy Height [m]")
dev.off()

##veg
mapview(veg)+mapview(beton_polys_df[7,])
veg_crop <- crop(veg, extent(beton_polys_df[7,]))
spplot(veg_crop) #check
veg_mask <- mask(veg_crop, beton_polys_df[7,])
spplot(veg_mask, scales=list(draw = TRUE)) #check
spplot(beton_polys_df[7,])
#save to file
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/UAV")
writeRaster(veg_mask, filename = "veg_beton.grd")
#mean and sd
mean(values(veg_mask), na.rm=T) #1.02
sd(values(veg_mask), na.rm=T) # 0.15
#plot simple
png(filename="veg_beton_simple.png",
    height = 300, width=400 )
spplot(veg_mask, main="Vegetation [yes/no]",  
       cuts = 1,
       legendEntries = c("yes", "no"))
dev.off()

####kiebitz####
# coerce to SpatialPolygonsDataFrame
kiebitz_polys_df<-as(kiebitz_polys, "SpatialPolygonsDataFrame")
## Gli
# crop and mask
mapview(gli)+mapview(kiebitz_polys_df[7,])
gli_crop_k <- crop(gli, extent(kiebitz_polys_df[7,]))
spplot(gli_crop_k) #check
gli_mask_k <- mask(gli_crop_k, kiebitz_polys_df[7,])

#plot simple
png(filename="gli_kiebitz_simple.png",
    height = 300, width=400 )
spplot(gli_mask_k, main="EC04 - Green Leaf Index [-1 to 1]")
dev.off()
#plot with map
gli_kiebitz_mapview<-mapview(gli_mask_k)
mapshot(gli_kiebitz_mapview, 
        file = "gli_kiebitz_mapview.png") #save

#save to file
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/UAV")
writeRaster(gli_mask_k, filename = "gli_beton.grd")
#mean and sd
mean(values(gli_mask_k), na.rm=T) #0.1
sd(values(gli_mask_k), na.rm=T) #0.047

##height
mapview(height)+mapview(kiebitz_polys_df[7,])
height_crop_k <- crop(height, extent(kiebitz_polys_df[7,]))
spplot(height_crop_k, axes=T) #check
height_mask_k <- mask(height_crop_k, kiebitz_polys_df[7,])
spplot(height_mask_k, scales=list(draw = TRUE)) #check
spplot(kiebitz_polys_df[7,])
#save to file
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/UAV")
writeRaster(height_mask_k, filename = "height_beton.grd")
#mean and sd
mean(values(height_mask_k), na.rm=T) # 0.008
sd(values(height_mask_k), na.rm=T) # 0.044
#mean and sd without negative values
mean(values(height_mask_k)[values(height_mask_k)>=0], na.rm=T)
any(values(height_mask_k)<0)
which(values(height_mask_k)<0)

#plot pretty
plot(height_mask_k, axes=T, xlim=c(406500, 406700))
title("Surface Height for EC04 Footprint \n(80% Contribution)")

#plot simple
png(filename="height_kiebitz_simple.png",
    height = 300, width=400)
spplot(height_mask_k, main="EC04 - Canopy Height [m]")
dev.off()

##veg
mapview(veg)+mapview(kiebitz_polys_df[7,])
veg_crop_k <- crop(veg, extent(kiebitz_polys_df[7,]))
spplot(veg_crop_k) #check
veg_mask_k <- mask(veg_crop_k, kiebitz_polys_df[7,])
spplot(veg_mask_k, scales=list(draw = TRUE)) #check
spplot(kiebitz_polys_df[7,])
#save to file
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/UAV")
writeRaster(veg_mask_k, filename = "veg_kiebitz.grd")
#mean and sd
mean(values(veg_mask_k), na.rm=T) #1.02
sd(values(veg_mask_k), na.rm=T) # 0.15
#plot simple
png(filename="veg_kiebitz_simple.png",
    height = 300, width=400 )
spplot(veg_mask_k, main="Vegetation [yes/no]",  
       cuts = 1,
       legendEntries = c("yes", "no"))
dev.off()
