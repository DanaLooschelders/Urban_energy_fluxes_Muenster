#Calculate the Footprint with the R code provided by N. Kljun
#load neccessayr libraries
library(plot3D)
library(fields)
library(BiocManager)
library(plot3D)
library(EBImage)
library(jpeg)
library(zoom)
library(raster)
library(mapview)
library(circular)
library(fields) #install depency for spatialfill
library(spatialfil)
#source functions seperately`
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/functions_from_packages/function_convKernel_from_spatialfill.R")
#source scripts that contain footprint functions
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/functions_from_packages/calc_footprint_FFP.R")
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/functions_from_packages/calc_footprint_FFP_climatology.R")
#source script for calculation of BLH
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Footprint_analysis/calculate_BLH.r")


#footprint climatology
#domain = Domain size as an array of (xmin xmax ymin ymax) [m].
#dx, dy = Cell size of domain [m]
#nx, ny = Two integer scalars defining the number of grid elements in x and y
#r = Percentage of source area, i.e. a value between 10% and 90%. Default is [10:10:80]. Set to "NaN" for no output of percentages
#rslayer = Calculate footprint even if zm within roughness sublayer: set rslayer = 1. Default is 0 (i.e. no footprint for within RS). z0 is needed for
#smooth_data = Apply convolution filter to smooth footprint climatology if smooth_data=1 (default)
#crop = Crop output area to size of the 80% footprint or the largest r given if crop=1
#pulse = Display progress of footprint calculations every pulse-th footprint (e.g., "100")
#fig = Plot an example figure of the resulting footprint (on the screen): set fig = 1. Default is 0 (i.e. no figure)
#get dataframe woth no mis
footprint<-data.frame("windspeed"=dat.beton.flux.meteo$wind_speed, 
                      "L"=dat.beton.flux.meteo$L, 
                      "h"=BLH_full$blh,
                      "sigmav"=sqrt(dat.beton.flux.meteo$v_var),
                      "ustar"=dat.beton.flux.meteo$u.,
                    "winddir"=dat.beton.flux.meteo$wind_dir,
                    "date"=dat.beton.flux.meteo$TIMESTAMP,
                    "u_unrot"=dat.beton.flux.meteo$u_unrot,
                    "v_unrot"=dat.beton.flux.meteo$v_unrot,
                    "u_rot"=dat.beton.flux.meteo$u_rot,
                    "v_rot"=dat.beton.flux.meteo$v_rot)
#mean(footprint$winddir)
#226/7
#191/5
footprint<-footprint[complete.cases(footprint),]
range(footprint$date)
#footprint<-footprint[1:10,]
FFP_para <- calc_footprint_FFP_climatology(zm=1.78, 
                                      z0=NaN, 
                                      umean=footprint$windspeed, 
                                      h=footprint$h, #boundary layer
                                      ol=footprint$L,
                                      dx=1, dy=1,#cell size of 1 m
                                      sigmav=footprint$sigmav, 
                                      ustar=footprint$ustar, 
                                      wind_dir=footprint$winddir,
                                      domain=c(-80,80,-80,80),
                                      r=seq(10,80,10), 
                                      smooth_data=1)
#an aggregated footprint, a so-called footprint climatology
range(footprint$L)

#Two-dimensional view of footprint
ffp_x <- c(FFP_para$x_2d) #x-grid of footprint climatology [m]
ffp_y <- c(FFP_para$y_2d) #y-grid of footprint climatology [m]
ffp_f <- c(FFP_para$fclim_2d) #Normalised footprint function values of footprint climatology [m-2]

#receptor is mounted above the origin (0,0) and positive x indicates upwind distance
#mean_wind_dir<-mean(circular(footprint$winddir, units = "degrees"))

mean_wind_dir<-atan2(mean(footprint$u_comp, na.rm=T),
      mean(footprint$v_comp, na.rm=T))*(180/pi)+180
#mean wind dir is 329.3314

