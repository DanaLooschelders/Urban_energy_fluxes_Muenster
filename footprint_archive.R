#Calculate the Footprint with the R code provided by N. Kljun
#load neccessayr libraries
#jpeg, fields, zoom, EBImage
#install.packages("spatialfil")
#library(spatialfil) #doesn`t work 
library(fields) #install depency for spatialfill
#install spatialfil from archive
#install.packages("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/FFP_R/spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(spatialfil)
#source functions seperately`
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/function_convKernel_from_spatialfill.R")
#install.packages("plot3D")
library(plot3D)
#install.packages("fields")
library(fields)
library(BiocManager)
library(plot3D)
#install.packages("EBImages")
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install(version = "3.12")
#BiocManager::install("EBImage", force=T)
library(EBImage)
library(jpeg)
#install.packages("zoom")
library(zoom)
library(raster)
#install.packages("mapview")
library(mapview)
library(circular)
#source scripts that contain footprint functions
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/FFP_R/calc_footprint_FFP.R")
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/FFP_R/calc_footprint_FFP_climatology.R")

#source script for calculation of BLH
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/calculate_BLH.r")

#input for footprint
#zm = Measurement height above displacement height (i.e. z-d) [m] 
#Beton: 1.84-(0.6*0.1) = 1.78
#Kiebitz
#z0 = Roughness length [m] - enter [NaN] if not known
#umean = Mean wind speed at zm [ms-1] - enter [NaN] if not known
#h = Boundary layer height [m]
#ol = Obukhov length [m]
#sigmav = standard deviation of lateral velocity fluctuations [ms-1]
#ustar = friction velocity [ms-1]
#optimal inputs
#wind_dir = Wind direction in degrees (of 360) for rotation of the footprint
#r = Percentage of source area, i.e. a value between 10% and 90%. Default is [10:10:80]. S
#nx = Integer scalar defining the number of grid elements of the scaled footprint. Default is 1000, nx must be >=600.
#rslayer = Calculate footprint even if zm within roughness sublayer: set rslayer = 1. Default is 0 (i.e. no footprint for within RS). z0 is needed for estimation of the RS.
#crop = Crop output area to size of the 80% footprint or the largest r given if crop=1

#for Beton
FFP_single <- calc_footprint_FFP(zm=1.78,#Measurement height above displacement height
                                 z0=NaN,
                                 umean = mean(dat.beton.flux.meteo$wind_speed, na.rm=T),
                                 h=BLH_full$pbl_layer1[100], #boundary layer height?
                                 ol=dat.beton.flux.meteo$L[100], #Obokhov length
                                 sigmav=sqrt(dat.beton.flux.meteo$v_var[100]),
                                 ustar=dat.beton.flux.meteo$u.[100], #ustar
                                 wind_dir=dat.beton.flux.meteo$wind_dir[100], #winddir
                                 r=seq(10,80,10))
#crosswind integrated footprint
#--> describes the footprint functionâs upwind extent but not its width
plot(FFP_single$x_ci, FFP_single$f_ci, type="l")

#2D footprint
#Two-dimensional view of single footprint with contour lines of R%
ffp_x <- c(FFP_single$x_2d)
ffp_y <- c(FFP_single$y_2d)
ffp_f <- c(FFP_single$f_2d)
#nx and ny must be adjusted -> resolution (no of grid elements)
#adjust xlim and ylim for plot
quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,ny=1000, xlim=c(-50,200),ylim=c(-180,200))
for (i in 1:8) lines(FFP_single$xr[[i]],FFP_single$yr[[i]], type="l", col="red")

#Two-dimensional view of footprint climatology with contour lines of R%.
image.plot(FFP_single$x_2d[1,], FFP_single$y_2d[,1], FFP_single$fclim_2d)
for (i in 1:8) lines(FFP_single$xr[[i]], FFP_single$yr[[i]], type="l", col="red")
