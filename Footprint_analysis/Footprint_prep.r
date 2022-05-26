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
source("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/function_convKernel_from_spatialfill.R")
#source scripts that contain footprint functions
source("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/FFP_R/calc_footprint_FFP.R")
source("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/FFP_R/calc_footprint_FFP_climatology.R")
#source script for calculation of BLH
source("Z:/Klimatologie/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/calculate_BLH.r")


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
                    "u_comp"=dat.beton.flux.meteo$u_rot,
                    "v_comp"=dat.beton.flux.meteo$v_rot)
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
range(BLH_merge$pbl_layer1, na.rm=T)

#calculate the area of the largest contour line
quilt.plot(ffp_x,ffp_y,ffp_f,nx=1000,ny=1000, xlim=c(-50,200),ylim=c(-180,200))
lines(FFP_single$xr[[8]],  FFP_single$yr[[8]], type="l", col="red")
ellipse<-data.frame("x"=FFP_single$xr[[8]], "y"=FFP_single$yr[[8]])
# Center of ellipse from Stackoverflow
#https://stackoverflow.com/questions/60423973/how-to-calculate-the-area-of-a-ggplot-stat-ellipse-when-type-norm
ctr = MASS::cov.trob(ellipse)$center 
# I tried changing this to 'stats::cov.wt' instead of 'MASS::cov.trob' 
#from what is saw from (https://github.com/tidyverse/ggplot2/blob/master/R/stat-ellipse.R#L98)
# Calculate distance to center from each point on the ellipse
dist2center <- sqrt(rowSums((t(t(ellipse)-ctr))^2))
# Calculate area of ellipse from semi-major and semi-minor axes. 
#These are, respectively, the largest and smallest values of dist2center. 
pi*min(dist2center)*max(dist2center)
#1764.154 calculated height
#1713.055 random height
FFP_single_2$xr[[8]]==FFP_single$xr[[8]]
#3D footprint climatology surface (using the plot3D package)
surf3D(FFP_single$x_2d, FFP_single$y_2d,FFP_single$fclim_2d)

#plot Crosswind-integrated footprint
plot(FFP_para$x_2d,FFP_para$fclim_2d, type="l")

#plot
#receptor is mounted above the origin (0,0) and positive x indicates upwind distance
mean_wind_dir<-mean(circular(footprint$winddir, units = "degrees"))

atan2(mean(footprint$u_comp, na.rm=T),
      mean(footprint$v_comp, na.rm=T))*(180/pi)+180
#mean wind dir is 270
