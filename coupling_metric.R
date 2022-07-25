#calculate novel coupling metric

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
  #Leaf area index --> estimate? calculate with GLI?
  #temperature at canopy height? same as height z? Literature, supp. data?
  #horizontal windspeed at canopy height -> calculate with log function from wind speed at height z?
