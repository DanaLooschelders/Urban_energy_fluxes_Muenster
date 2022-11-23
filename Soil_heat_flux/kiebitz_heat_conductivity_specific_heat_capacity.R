#Kiebitz
# alpha = k/(p*Cp)= thermal diffusivity (from Oldroyd, 2013)
#Cv=p*Cp 
#k=alpha*Cv = thermal conductivity
alpha_kiebitz=1.8*10^-7 #m-2 s-1 
# k = thermal conductivity

#calculate specific heat capacity and density of soil particles and soil water content

##### p = density 
#the measured density (soil solids and water) was 1.87 g/cm^3
#take 2.58 Mg m^-3 as literature value for particle density of sandy loam soil 
  #with 66% Sand, 23% Silt, 11% Clay and 2.3% organic matter
  #from Ochsner, 2001
#bulk density is needed 
#bulk density of sand (Scheffer/Schachtschabel): 1.16-1.7
#bulk density of loam  (Scheffer/Schachtschabel): 1.2-2
#take mean VWC and calculate bulk density from that? 
rho_solids=1.87*1000 #convert Mg m^-3 to kg m^-3

#the density of water is 998 kg/m3
rho_water<- 998 #kg/m3

#####cp = specific heat
#specific heat of soil solids for sandy loam soil
#from Ochsner, 2001 
cp_solids<- 801 #J/(kg K) 

#specific heat of water: 4182 J/kg/K
cp_water<- 4182 # J/kg/K

#####v = volume fraction
#volums fraction of solids
v_solids<-1-dat.soil.merge$mean_VWC #m3/m3

#volume fraction of water
theta<-dat.soil.merge$mean_VWC #m3/m3
plot(dat.soil.merge$mean_VWC, type="l")
#calculate volumetric heat capacity from specific heat capacity and density and volume fractions of solids and water 
#from Ochsner 2001 (deVries method from 1963)
#C=rho_s*vs*cp_soilds+rho_w*cp_water*theta
Cv<-cp_soilds*rho_solids*v_solids+cp_water*rho_water*theta
mean(Cv, na.rm=T) #2334465 J m-3 K-3 
mean(Cv, na.rm=T)/1000000 #2.334465 MJ m-3 K-3
median(Cv, na.rm=T) #2311917 J m-3 K-3
median(Cv, na.rm=T)/1000000 #2.311917 MJ m-3 K-3
plot(Cv, type="l")
#calculate thermal conductivity W m-1 K-1
#k=alpha*Cv
k_kiebitz<-alpha_kiebitz*Cv
mean(k_kiebitz, na.rm=T) #0.4202036 W m-1 K-1 
median(k_kiebitz, na.rm=T) #0.4161451 W m-1 K-1
#plot
plot(k_kiebitz, type="l")
