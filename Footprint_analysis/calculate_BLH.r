#calculate Boundary Layer Height
#Parametrizised

#source heat fluxes and meteorology script
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Meteorology/heat_fluxes_with_meteorology.r")

#-------------------calculate boundary layer height from paper-------------------------------------#
#Kljun, N.; Calanca, P.; Rotach, M. W.; Schmid, H. P. (2015): A simple two-dimensional parameterisation for Flux Footprint Prediction (FFP)

####calculate blh for stable conditions####
#L=monin obukvov length
#ustar as friction velocity
calculate_blh<-function(L=dat.beton.flux.meteo$L, ustar=dat.beton.flux.meteo$u.){
#stable/neutral --> formula
#coriolis Paramter f defined as f<-2*sigma*sinφ 
sigma<-7.2921 * 10^-5 #angular velocity of earth rotation
sin_lat<-sin(51.947046) #sinus of latitude
f<-2*sigma*sin_lat #calculate f


#calculate blh for stable conditions
blh_s<-L/3.8*(-1+(1+2.28*(ustar/(f*L)))^1/2) 
plot(blh_s, type="l")
#####calculate blh for neutral conditions when z/L~0####

#calculate z/L
zL<-1.84/dat.beton.flux.meteo$L
plot(zL, type="l")

#what means ~0? -> 0.05 okay?
plot(zL[zL>-0.05&zL<0.05], type="l")
#hn=cn(ustar/|f|)
cn<-0.3 
#calculate blh for neutral conditions
blh_n<-cn*(ustar/abs(f))
plot(blh_n, type="l")
#####for convective conditions blh is 1500####
blh_c<-1500
#source for convective: https://geography.swansea.ac.uk/nkljun/ffp/www/downloads/FFPonline_Readme.pdf

####assign values to neutral, stable and convective conditions####
#create dataframe
blh<-data.frame("blh"=blh_s, 
                "zL"=zL,
                "condition"="nearneutral_stable") #stable condtions
#label for convective condition
blh$condition[zL<1*-1]<-"convective"
#label for neutral condition 
blh$condition[blh$zL>-0.05&blh$zL<0.05]<-"neutral"
blh$condition[is.na(blh$zL)]<-"unknown"


#assign calculated values
blh$blh[blh$condition=="convective"]<-blh_c #convective, set to 1500 if z/L<-1
blh$blh[blh$condition=="neutral"]<-blh_n[blh$condition=="neutral"] #neutral, if z/L~0

blh$timestamp<-dat.beton.flux.meteo$TIMESTAMP
blh$timestamp<-as.POSIXct(blh$timestamp)
#blh$tod<-dat.beton.flux.meteo$tod
return(blh)
}
#call function
blh_beton<-calculate_blh()
blh_kiebitz<-calculate_blh(L=dat.kiebitz.flux.meteo$L, ustar=dat.kiebitz.flux.meteo$u.)


#get boundary layer height to same length as dat.beton.fluxes
#Beton
dat.beton.flux.meteo$timestamp<-dat.beton.flux.meteo$TIMESTAMP
BLH_beton<-full_join(x=dat.beton.flux.meteo[,c("H","timestamp")], y=blh_beton, by="timestamp")
#Kiebitz
dat.kiebitz.flux.meteo$timestamp<-dat.kiebitz.flux.meteo$TIMESTAMP
BLH_kiebitz<-full_join(x=dat.kiebitz.flux.meteo[,c("H","timestamp")], y=blh_kiebitz, by="timestamp")


#plot
library(ggplot2)
ggplot(dat=blh_beton, aes(x=timestamp, y=blh))+
  geom_line()+
  theme_bw()
ggplot(dat=blh_kiebitz, aes(x=timestamp, y=blh))+
  geom_line()+
  theme_bw()
