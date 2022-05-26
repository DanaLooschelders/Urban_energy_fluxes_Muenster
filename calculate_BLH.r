#calculate Boundary Layer Height
#Parametrizised

#source heat fluxes and meteorology script
source("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/heat_fluxes_with_meteorology.r")

#-------------------calculate boundary layer height from paper-------------------------------------#

####calculate blh for stable conditions####

#stable/neutral --> formula
#coriolis Paramter f defined as f<-2*sigma*sinφ 
sigma<-7.2921 * 10^-5 #angular velocity of earth rotation
sin_lat<-sin(51.947046) #sinus of latitude
f<-2*sigma*sin_lat #calculate f

L<-dat.beton.flux.meteo$L #monin obukvov length
ustar<-dat.beton.flux.meteo$u. #ustar as friction velocity

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
blh$tod<-dat.beton.flux.meteo$tod
#plot
library(ggplot2)
ggplot(dat=blh, aes(x=timestamp, y=blh))+
  geom_line()+
  theme_bw()


#get boundary layer height to same length as dat.beton.fluxes
dat.beton.flux.meteo$timestamp<-dat.beton.flux.meteo$TIMESTAMP

BLH_full<-full_join(x=dat.beton.flux.meteo[,c("H","timestamp")], y=blh, by="timestamp")

