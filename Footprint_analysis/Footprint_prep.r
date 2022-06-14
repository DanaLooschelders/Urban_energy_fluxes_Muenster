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

####Beton####
footprint_beton<-data.frame("windspeed"=dat.beton.flux.meteo$wind_speed, 
                      "L"=dat.beton.flux.meteo$L, 
                      "h"=BLH_beton$blh,
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
footprint_beton<-footprint_beton[complete.cases(footprint_beton),]
range(footprint_beton$date)
#footprint<-footprint[1:10,]
FFP_beton <- calc_footprint_FFP_climatology(zm=1.78, 
                                      z0=NaN, 
                                      umean=footprint_beton$windspeed, 
                                      h=footprint_beton$h, #boundary layer
                                      ol=footprint_beton$L,
                                      dx=1, dy=1,#cell size of 1 m
                                      sigmav=footprint_beton$sigmav, 
                                      ustar=footprint_beton$ustar, 
                                      wind_dir=footprint_beton$winddir,
                                      domain=c(-90,90,-90,90),
                                      r=seq(10,90,10), 
                                      smooth_data=1)
#an aggregated footprint, a so-called footprint climatology
range(footprint_beton$L)

#Two-dimensional view of footprint
ffp_x_b <- c(FFP_beton$x_2d) #x-grid of footprint climatology [m]
ffp_y_b <- c(FFP_beton$y_2d) #y-grid of footprint climatology [m]
ffp_f_b <- c(FFP_beton$fclim_2d) #Normalised footprint function values of footprint climatology [m-2]

#receptor is mounted above the origin (0,0) and positive x indicates upwind distance

####Kiebitz####
footprint_kiebitz<-data.frame("windspeed"=dat.kiebitz.flux.meteo$wind_speed, 
                            "L"=dat.kiebitz.flux.meteo$L, 
                            "h"=BLH_kiebitz$blh,
                            "sigmav"=sqrt(dat.kiebitz.flux.meteo$v_var),
                            "ustar"=dat.kiebitz.flux.meteo$u.,
                            "winddir"=dat.kiebitz.flux.meteo$wind_dir,
                            "date"=dat.kiebitz.flux.meteo$TIMESTAMP,
                            "u_unrot"=dat.kiebitz.flux.meteo$u_unrot,
                            "v_unrot"=dat.kiebitz.flux.meteo$v_unrot,
                            "u_rot"=dat.kiebitz.flux.meteo$u_rot,
                            "v_rot"=dat.kiebitz.flux.meteo$v_rot)
#mean(footprint$winddir)
#226/7
#191/5
footprint_kiebitz<-footprint_kiebitz[complete.cases(footprint_kiebitz),]
range(footprint_kiebitz$date)
#footprint<-footprint[1:10,]
FFP_kiebitz <- calc_footprint_FFP_climatology(zm=1.77, 
                                            z0=NaN, 
                                            umean=footprint_kiebitz$windspeed, 
                                            h=footprint_kiebitz$h, #boundary layer
                                            ol=footprint_kiebitz$L,
                                            dx=1, dy=1,#cell size of 1 m
                                            sigmav=footprint_kiebitz$sigmav, 
                                            ustar=footprint_kiebitz$ustar, 
                                            wind_dir=footprint_kiebitz$winddir,
                                            domain=c(-80,80,-80,80),
                                            r=seq(10,90,10), 
                                            smooth_data=1)
#an aggregated footprint, a so-called footprint climatology
range(footprint_kiebitz$L)

#Two-dimensional view of footprint
ffp_x_k <- c(FFP_kiebitz$x_2d) #x-grid of footprint climatology [m]
ffp_y_k <- c(FFP_kiebitz$y_2d) #y-grid of footprint climatology [m]
ffp_f_k <- c(FFP_kiebitz$fclim_2d) #Normalised footprint function values of footprint climatology [m-2]

#receptor is mounted above the origin (0,0) and positive x indicates upwind distance

