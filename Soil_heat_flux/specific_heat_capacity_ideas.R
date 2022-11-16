#Soil heat Flux

####Kiebitz####
#take specific heat capacity value for dry sandy loam soil and water (with mean VWC)
#water: ~4.18 MJ Mg–1 K–1
#dry sandy loam soil: 0.738 MJ Mg–1 K–1

#volumetric heat capacity

#Soil Science Society of America Journal Specific Heat Capacity of Soil Solids: Influences of Clay Content, Organic Matter, and Tightly Bound Water
#water<-dat.soil.merge$mean_VWC*4.18 
#soil<-1-dat.soil.merge$mean_VWC*0.738
#whole<-water+soil
#calculate mean and median
#mean(whole, na.rm=T) #1.414588
#median(whole, na.rm=T) #1.377473

# alpha = k/(p*Cp)= thermal diffusivity
alpha_kiebitz=1.8*10^-7 #m s-1
# k = thermal conductivity
# p = density
p_kiebitz=1.87 #g/cm^3
#1 g/cm3 = 1000 kg/m3

#water: 4.2 J/g°C 
cpw<-4.2*1000
p_kiebitz<-p_kiebitz/1000 #kg/m3
p_water<-1 #g/cm^3
p_water<-p_water/1000 #kg/m3
cp<-801 #J/(kg K) #only of solids
C<-cp*p_kiebitz*(1-dat.soil.merge$mean_VWC)+cpw*p_water*dat.soil.merge$mean_VWC
mean(C, na.rm=T)
# Cv = volumetric heat capacity Jm^-3 K^-1
#assume particle density of 2.65 Mgm3 (often assumed to be a constant)
#Cv= 2.01*10^-6 pb/2.65 + 4.19*10^-6*VWC
#Cv_kiebitz<-2.01*10^6*2.65/2.65 + 4.19*10^6*dat.soil.merge$mean_VWC

#cv=Cgmin*xmin+Cgwasser*VWC (Foken, deVries Verfahren)
Cp_kiebitz<-2.01*(1-dat.soil.merge$mean_VWC)+4.12*10^6*dat.soil.merge$mean_VWC
#C=ps*vs*cs+pw*cw*VWC
C_Kiebitz<-+998*4182*dat.soil.merge$mean_VWC
#calculate thermal conductivity W m-1 K
#k=alpha*Cv
k_kiebitz<-alpha_kiebitz*Cp_kiebitz
mean(k_kiebitz, na.rm=T)

#heat flux as in Bonan (2016) Ecological Climatology
#F=-k(∆t/∆z)
#F=heat flux [W/m^2]
#-k = thermal conductivity
#∆t/∆z = temperature gradient

