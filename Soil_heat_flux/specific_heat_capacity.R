#Soil heat Flux

####Kiebitz####
#take specific heat capacity value for dry sandy loam soil and water (with mean VWC)
#water: ~4.18 MJ Mg–1 K–1
#dry sandy loam soil: 0.738 MJ Mg–1 K–1
#Soil Science Society of America Journal Specific Heat Capacity of Soil Solids: Influences of Clay Content, Organic Matter, and Tightly Bound Water
water<-dat.soil.merge$mean_VWC*4.18 
soil<-1-dat.soil.merge$mean_VWC*0.738
whole<-water+soil
#calculate mean and median
mean(whole, na.rm=T)
median(whole, na.rm=T)


# alpha = k/(p*Cp)= thermal diffusivity
alpha_kiebitz=1.8*10^-7
# k = thermal conductivity
# p = density
density_kiebitz=
# Cp = specific heat capacity

#calculate thermal conductivity


#heat flux as in Bonan (2016) Ecological Climatology
#F=-k(∆t/∆z)
#F=heat flux [W/m^2]
#-k = thermal conductivity
#∆t/∆z = temperature gradient

