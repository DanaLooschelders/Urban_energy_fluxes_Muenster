#Kiebitz
# alpha = k/(p*Cp)= thermal diffusivity (from Oldroyd, 2013)
#Cv=p*Cp 
#k=alpha*Cv = thermal conductivity
alpha_kiebitz=1.8*10^-7 #m s-1 
# k = thermal conductivity

#calculate specific heat capacity and density of soil particles and soil water content

##### p = density 
#the measured density (soil solids and water) was 1.87 g/cm^3
#take 2.58 Mg m^-3 as literature value for particle density of sandy loam soil 
  #with 66% Sand, 23% Silt, 11% Clay and 2.3% organic matter
  #from Ochsner, 2001
rho_solids=2.58/1000 #convert Mg m^-3 to kg m^-3

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

#calculate volumetric heat capacity from specific heat capacity and density and volume fractions of solids and water 
#from Ochsner 2001 (deVries method from 1963)
#C=rho_s*vs*cp_soilds+rho_w*cp_water*theta
Cv<-cp_soilds*rho_solids*v_solids+cp_water*rho_water*theta
mean(Cv, na.rm=T) #502714.8J m-3 K-3 
median(Cv, na.rm=T) #457710.6 J m-3 K-3
plot(Cv, type="l")
#calculate thermal conductivity W m-1 K-1
#k=alpha*Cv
k_kiebitz<-alpha_kiebitz*Cv
mean(k_kiebitz, na.rm=T) #0.09048866
median(k_kiebitz, na.rm=T) #0.08238791
#plot
plot(k_kiebitz, type="l")
