#beton
# alpha = k/(p*Cp)= thermal diffusivity (from Oldroyd, 2013)
alpha_beton<-7.13*10^-8 #m-2 s-1 
#Cv=p*Cp 
#k=alpha*Cv = thermal conductivity


##### p = density 
p_beton<-2.409*1000 # g/cm^3 to kg/m^3

#####cp = specific heat 
#take literature value for concrete with similar density 
#taken from Howlander, 2012
#concrete with density of 2339 and diffusivty of 9.29*10-7
#J/(kg K) 
cp_beton<-1023

Cv_beton=p_beton*cp_beton
#calculate thermal conductivity W m-1 K-1
#k=alpha*Cv

k_beton<-alpha_beton*Cv_beton #0.1757122
