#prep files for online footprint tool
#yyyy = Year
#mm = Month [1‐12]
#day = Day of month [1‐31]
#HH = Hour [0‐23] or [1‐24] – has to be in UTC, NOT local time
#MM = Minutes, for example [0 30]
#zm = Measurement height above ground [m]
#d = Displacement height [m]
#z0 = Roughness length [m] ‐ enter [‐999] if not known
#u_mean = Mean wind speed at zm [ms‐1] ‐ enter [‐999] if not known
#L = Obukhov length [m]
#sigma_v = Standard deviation of lateral velocity fluctuations after rotation [ms‐1]
#u_star = Friction velocity [ms‐1]
#wind_dir = Wind direction in degrees (of 360) for rotation of the footprint
setwd("C:/00_Dana/Uni/Masterarbeit/FFP_R")
template<-read.csv("FFPonline_template.csv")

#yyyy,mm,day,HH_UTC,MM,zm,d,z0,u_mean,L,sigma_v,u_star,wind_dir
footprint_calc<-data.frame("yyyy"=as.numeric(substr(dat.beton.flux.meteo$TIMESTAMP, start = 1, stop=4)),
                           "mm"=as.numeric(substr(dat.beton.flux.meteo$TIMESTAMP, start=6, stop=7)),
                           "day"=as.numeric(substr(dat.beton.flux.meteo$TIMESTAMP, start = 9, stop=10)),
                           "HH_UTC"=as.numeric(substr(dat.beton.flux.meteo$time, start=1, stop=2)),
                           "MM"=as.numeric(substr(dat.beton.flux.meteo$time, start=4, stop=5)),
                           "zm"=rep(1.84, length(dat.beton.flux.meteo$time)),
                           "d"=rep(0.67*0.01, length(dat.beton.flux.meteo$time)),
                           "z0"=rep(-999, length(dat.beton.flux.meteo$time)),
                           "u_mean"=dat.beton.flux.meteo$u_unrot,
                           "L"=dat.beton.flux.meteo$L,
                           "sigma_v"=sqrt(dat.beton.flux.meteo$v_var),
                           "u_star"=dat.beton.flux.meteo$u.,
                           "wind_dir"=dat.beton.flux.meteo$wind_dir)

footprint_calc<-footprint_calc[complete.cases(footprint_calc),]
setwd("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Footprint_analysis")
write.csv(footprint_calc, file = "vlaues_for_footprint.csv", row.names = F,na = "-999")
getwd()

#coords
#Lat 51.9470462
#Lon 7.6407622