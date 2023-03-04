####Drone Data####
#load drone data
setwd("D:/")
drone_2021<-stack(x = "CalmCity_Multispectral_2021.tif")
drone_2022<-stack(x="Hafen_Multispectral.tif")

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
NDVI_drone_2022<-(drone_2022[[5]]-drone_2022[[3]])/(drone_2022[[5]]+drone_2022[[3]])

#plot
plot(NDVI_drone_2021)
#calculate LAI
LAI_drone_2021<-0.128*exp(NDVI_drone_2021/0.311)
LAI_drone_2022<-0.128*exp(NDVI_drone_2022/0.311)
#Investigating the relationship between NDVI and LAI in semi-arid grassland in Inner Mongolia using in-situ measurements
#Fan et al. 2009
LAI_drone_2021_mongolia<-0.128*exp(NDVI_drone_2021/0.311)
mapview(LAI_drone_2021_mongolia)

mapview(LAI_drone_2021)
mapview(LAI_drone_2022)

#Worldview (4mn)
#plot
plot(LAI_drone)
#mapview with footprint
mapview(LAI_drone_2021)+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#mapview with footprint
mapview(LAI_drone_2022)+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#calculate stats
#calculate LAI for Kiebitz
LAI_kiebitz_drone<-mask(LAI_drone_2021, kiebitz_polys[8])
#mapview(LAI_kiebitz_drone)
round(mean(values(LAI_kiebitz_drone), na.rm=T),2) #1.17
round(sd(values(LAI_kiebitz_drone), na.rm=T),2) #0.48
#calculate LAI for Beton
LAI_beton_drone<-mask(LAI_drone_2021, beton_polys[8])
mapview(LAI_beton_drone)
round(mean(values(LAI_beton_drone), na.rm=T),2) #0.23
round(sd(values(LAI_beton_drone), na.rm=T),2) #0.3
