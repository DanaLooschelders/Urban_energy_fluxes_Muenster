#calculate novel coupling metric
#install.packages("bigleaf")
library(bigleaf)
library(ggplot2)
library(rgdal)
library(gdalUtils)
library(mapview)
#library(leafem)

#z: height above the ground
#h canopy height
#we,crit: critical speed of an air parcel

  #y=constant=0.277
  #c_hut_d = ,mean drag coefficient below h (in paper 0.15)
  #LAi (leaf area index)
  #u_h = horizontal wind speed at canopy height
  #g = acceleration due to gravity
  #θ_hut = mean potential temperature below h
  #θ_e = potential temperature of the downward moving air parcel

#Uh or LAI decrease (canopy drag decreases) or 
#(θ_e - θ_hut) e or h decreases(influence of buoyancy and vertical distance decrease)

#questions:
  #which coefficient for drag? -> google
  #Leaf area index --> this years data? Sentinel-2 data?
  #temperature at canopy height? same as height z? Literature, supp. data?

  #horizontal windspeed at canopy height -> calculate with log function from wind speed at height z?
####Wind Kiebitz####
df_kiebitz<-data.frame("Tair"=dat.kiebitz.flux.meteo$AirTC_Avg_kiebitz,
               "pressure"=dat.beton.flux.meteo$AirP_Avg,
               "ustar"=dat.kiebitz.flux.meteo$u.,
               "H"=dat.kiebitz.flux.meteo$H,
               "wind"=dat.kiebitz.flux.meteo$wind_speed+0.3)#dat.kiebitz.flux.meteo$wind_speed)
plot(dat.kiebitz.flux.meteo$wind_speed+0.5, type="l")
abline(h=0, col="red")


plot(df_kiebitz$wind*0.1, type="l")
plot(df_kiebitz$wind[1:20], type="l")
plot(dat.beton.flux.meteo$wind_speed, type="l")
lines(dat.kiebitz.flux.meteo$wind_speed, type="l", col="red")

mean(dat.beton.flux.meteo$wind_speed, na.rm=T)

windprofile_kiebitz<-wind.profile(data=df_kiebitz, 
             z=c(0.15), #Vector with heights for which wind speed is to be calculated. 
             Tair = "Tair", #Air Temp
             pressure = "pressure", #Air Pressure
             ustar = "ustar", #friction velocity
             H = "H", #sensible heat flux
             wind = "wind", #Wind speed at height zr (m s-1); only used if stab_correction = TRUE
             zr=1.77, #instrument height
             zh=0.15, #canopy height
             d = 0.67*0.15, #zero plane displacement height
             z0m = 0.15*0.15, #Roughness length (m),
             #estimate_z0m = TRUE,#Should z0m be estimated from the logarithmic wind profile? I
             stab_correction = TRUE)#, #Should stability correction be applied?
             #stab_formulation = c("Dyer_1970","Businger_1971"))#,  #Stability correction function used
             #constants = bigleaf.constants()) #k - von-Karman constant (-) Kelvin - conversion degree Celsius to Kelvin cp - specific heat of air for constant pressure (J K-1 kg-1) g - gravitational acceleration (m s-2)
d_plus_zm_k<-(0.67*0.15)+(0.15*0.15)
d_plus_zm_k-0.15

plot(windprofile_kiebitz_plus05, type="l")
lines(windprofile_kiebitz_plus06, col="red")

mean(windprofile_kiebitz, na.rm=T) #0
sd(windprofile_kiebitz, na.rm=T) #0

####Wind Beton####
df_beton<-data.frame("Tair"=dat.beton.flux.meteo$AirTC_Avg_beton,
                       "pressure"=dat.beton.flux.meteo$AirP_Avg,
                       "ustar"=dat.beton.flux.meteo$u.,
                       "H"=dat.beton.flux.meteo$H,
                       "wind"=dat.beton.flux.meteo$wind_speed)

windprofile_beton<-wind.profile(data=df_beton, 
                                  z=c(0.1), #Vector with heights for which wind speed is to be calculated. 
                                  Tair = "Tair", #Air Temp
                                  pressure = "pressure", #Air Pressure
                                  ustar = "ustar", #friction velocity
                                  H = "H", #sensible heat flux
                                  wind = "wind", #Wind speed at height zr (m s-1); only used if stab_correction = TRUE
                                  zr=1.84, #instrument height
                                  zh=0.1, #canopy height
                                  d = 0.67*0.1, #zero plane displacement height
                                  z0m = 0.15*0.1, #Roughness length (m),
                                  #estimate_z0m = TRUE,#Should z0m be estimated from the logarithmic wind profile? I
                                  stab_correction = TRUE)#, #Should stability correction be applied?
#stab_formulation = c("Dyer_1970","Businger_1971"))#,  #Stability correction function used
#constants = bigleaf.constants()) #k - von-Karman constant (-) Kelvin - conversion degree Celsius to Kelvin cp - specific heat of air for constant pressure (J K-1 kg-1) g - gravitational acceleration (m s-2)

d_plus_zm_b<-(0.67*0.1)+(0.15*0.1) #0.082
d_plus_zm_b-0.1
plot(windprofile_beton, type="l")
mean(windprofile_beton, na.rm=T) #0
sd(windprofile_beton, na.rm=T) #0
range(windprofile_beton, na.rm=T)

####LAI####
#install.packages("sen2r")
#install.packages(c("shinyFiles", "shinydashboard", "shinyWidgets"))
setwd("C:/00_Dana/Uni/Masterarbeit/LAI_Sentinel/S2B_MSIL2A_20210825T103619_N0301_R008_T32UMC_20210825T140331/S2B_MSIL2A_20210825T103619_N0301_R008_T32UMC_20210825T140331.SAFE/GRANULE/L2A_T32UMC_A023343_20210825T103907/IMG_DATA/R10m")

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
B8<-raster("T32UMC_20210825T103619_B08_10m.tif")
B4<-raster("T32UMC_20210825T103619_B04_10m.tif")
mapview(test)
#crop to shape of Muenster
gadm <- getData('GADM', country='DEU', level=2)
gadm <- gadm[gadm$NAME_2=="Münster",]
gadm_sf <- as(gadm,"sf")
extent(B8)
extent(gadm)

crs(B8) # +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs 
crs(gadm) #+proj=longlat +datum=WGS84 +no_defs 
ms<-spTransform(gadm, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"))
mapview(B8)+mapview(ms) 
#transform coordinate system
compareCRS(B8, ms)
#crop to size of MS
B4_crop<-crop(B4, ms)
B8_crop<-crop(B8, ms)
#check
mapview(B4_crop)
mapview(B8_crop)

#calculate NDVI Index
#(B8-B4)/(B8+B4)
NDVI<-(B8_crop-B4_crop)/(B8_crop+B4_crop)
mapview(NDVI)

#crop to study site
polys_ex<-extent(kiebitz_polys+beton_polys)
NDVI_crop<-crop(NDVI, polys_ex)

mapview(NDVI_crop)+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#estimate LAI
#semi-arid grassland in Inner Mongolia 
LAI_nexp<-0.0897+1.424*NDVI_crop #not exp
LAI_exp<-0.128*exp(NDVI_crop/0.311) #exp
mapview(LAI_exp)
mapview(LAI_nexp)

LAI<-0.0875*exp(4.372*NDVI_crop)
mapview(LAI)+
  mapview(kiebitz_polys[8], alpha.regions=0, col.region="black", lwd=1)+
  mapview(beton_polys[8], alpha.regions=0, col.region="black", lwd=1)

#calculate LAI for Kiebitz
LAI_kiebitz<-mask(LAI, kiebitz_polys[8])
mapview(LAI_kiebitz)
round(mean(values(LAI_kiebitz), na.rm=T),2)
#calculate LAI for Beton
LAI_beton<-mask(LAI, beton_polys[8])
mapview(LAI_beton)
round(mean(values(LAI_beton), na.rm=T),2)
