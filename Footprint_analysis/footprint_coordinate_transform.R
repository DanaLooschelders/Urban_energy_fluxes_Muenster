library(raster)
#install.packages("spdep")
library(spdep)
#install.packages("SMoLR")
#library(SMoLR)
library(sf)
library(sp)
library(mapview)
library(zoo)
####coordinate transformation####
#The trick is to use a rectangular projection for your map, 
#which is fine for the size ranges of a typical footprint.
#This can be UTM, or also SWEREF.  
#In this projection, one unit is equivalent to one meter.
#Hence, you can set the footprint model to a grid resolution of 1 m 
#using dx and dy input parameters.
#The sensor location in the footprint grid is at (0/0). 
utm_x_east_beton<-406579.60    #UTM Easting 406579.60 (x coordinate)
utm_y_north_beton<-5756021.19  #UTM Northing 5756021.19 (y coordinate)

utm_x_east_kiebitz<-406673.05
utm_y_north_kiebitz<-5755952.03

#This then needs a simple coordinates transfer to the sensor location in 
#your map reference system (e.g. SWEREF), i.e. add the values of your 
#sensor location to all footprint grid values.
FFP_beton$x_2d_UTM<-FFP_beton$x_2d+utm_x_east_beton
FFP_beton$y_2d_UTM<-FFP_beton$y_2d+utm_y_north_beton

FFP_kiebitz$x_2d_UTM<-FFP_kiebitz$x_2d+utm_x_east_kiebitz
FFP_kiebitz$y_2d_UTM<-FFP_kiebitz$y_2d+utm_y_north_kiebitz
#If you have a map resolution of < 1m, the same system applies, 
#just fit the footprint grid to the map grid.
beton_dataframe<-data.frame("x"=c(FFP_beton$x_2d_UTM), "y"=c(FFP_beton$y_2d_UTM), 
                            "values"=c(FFP_beton$fclim_2d))
beton_matrix<-as.matrix(beton_dataframe)

kiebitz_dataframe<-data.frame("x"=c(FFP_kiebitz$x_2d_UTM), "y"=c(FFP_kiebitz$y_2d_UTM), 
                            "values"=c(FFP_kiebitz$fclim_2d))
kiebitz_matrix<-as.matrix(kiebitz_dataframe)

####rotate in mean wind direction####
#calculate mean wind dir
library(rWind)

#atan2(mean(footprint$u_comp, na.rm=T),
#      mean(footprint$v_comp, na.rm=T))*(180/pi)+180

#aus Klimawiki Codeschnipseln
fun.mean_winddir <- function(u,v) ((180/(pi))*atan2(mean(-v, na.rm = TRUE),mean(u, na.rm = TRUE)))+180 
fun.mean_winddir(footprint_beton$u_unrot, footprint_beton$v_unrot) #beton
#fun.mean_winddir(footprint$u_rot, footprint$v_rot)
#mean wind dir is 239.3 -> direction on x axis
#90 would be east --> rotate -90 degrees --> 280 degrees
239.3-270 #-30.7
#239-90 -> -149 

fun.mean_winddir(footprint_kiebitz$u_unrot, footprint_kiebitz$v_unrot) #beton
#228 
228-270 #-42

#source function from package
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/functions_from_packages/Coordinate_Rotation_function_from_SMoLR.R")
#for beton
beton_matrix_rot<-rotate_coord(x=beton_matrix[,1], y=beton_matrix[,2], 
             angle = 30.7, center = c(utm_x_east_beton, utm_y_north_beton), type="degrees",
             method="polar")

beton_matrix_unrot<-beton_matrix
beton_matrix_rot<-cbind(beton_matrix_rot, beton_matrix[,3])
#for kiebitz
kiebitz_matrix_rot<-rotate_coord(x=kiebitz_matrix[,1], y=kiebitz_matrix[,2], 
                               angle = 42, center = c(utm_x_east_kiebitz, utm_y_north_kiebitz), type="degrees",
                               method="polar")

kiebitz_matrix_unrot<-kiebitz_matrix
kiebitz_matrix_rot<-cbind(kiebitz_matrix_rot, kiebitz_matrix[,3])
#create raster
raster_rot_b<-rasterFromXYZ(beton_matrix_rot,res = c(1,1), 
                           crs = "+proj=utm +zone=32U+datum=WGS84",digits = 0.3)
raster_unrot_b<-rasterFromXYZ(beton_matrix_unrot,res = c(1,1), 
                          crs = "+proj=utm +zone=32U+datum=WGS84",digits = 0.3)
#for kiebitz
raster_rot_k<-rasterFromXYZ(kiebitz_matrix_rot,res = c(1,1), 
                          crs = "+proj=utm +zone=32U+datum=WGS84",digits = 0.3)
raster_unrot_k<-rasterFromXYZ(kiebitz_matrix_unrot,res = c(1,1), 
                            crs = "+proj=utm +zone=32U+datum=WGS84",digits = 0.3)
#view beton
plot(raster_rot_b)
mapview(raster_rot_b)

plot(raster_unrot_b)
mapview(raster_unrot_b)

#view kiebitz
plot(raster_rot_k)
mapview(raster_rot_k)

plot(raster_unrot_k)
mapview(raster_unrot_k)

#create spatial polygons for contourlines
#for beton
FFP_beton$xr_utm<-lapply(FFP_beton$xr, function(x) x+utm_x_east_beton)
FFP_beton$yr_utm<-lapply(FFP_beton$yr, function(x) x+utm_y_north_beton)

#for kiebitz
FFP_kiebitz$xr_utm<-lapply(FFP_kiebitz$xr, function(x) x+utm_x_east_kiebitz)
FFP_kiebitz$yr_utm<-lapply(FFP_kiebitz$yr, function(x) x+utm_y_north_kiebitz)

#create list with x and y coordinates
lines_list_beton<-mapply(cbind,FFP_beton$xr_utm, FFP_beton$yr_utm, SIMPLIFY=FALSE)#beton
lines_list_kiebitz<-mapply(cbind,FFP_kiebitz$xr_utm, FFP_kiebitz$yr_utm, SIMPLIFY=FALSE)#kiebitz


#interpolate NAs
#beton
for(i in 1:length(lines_list_beton)){
  lines_list_beton[[i]][,1]<-na.approx(lines_list_beton[[i]][,1])
  lines_list_beton[[i]][,2]<-na.approx(lines_list_beton[[i]][,2])
}

#kiebitz
for(i in 1:length(lines_list_kiebitz)){
  lines_list_kiebitz[[i]][,1]<-na.approx(lines_list_kiebitz[[i]][,1])
  lines_list_kiebitz[[i]][,2]<-na.approx(lines_list_kiebitz[[i]][,2])
}

#rotate coordinate system
#lines_list<-lapply(lines_list, function(x) rotate_coord(as.matrix(x)[,1], 
#                                                        as.matrix(x)[,2],
#                                                        angle = -30.7, 
#                                                        center = c(utm_x_east, utm_y_north), 
#                                                        type="degrees",
#                                                        method="polar"))
#create polygons
ps_beton <- lapply(lines_list_beton, Polygon) #beton
ps_kiebitz <- lapply(lines_list_kiebitz, Polygon) #kiebitz
# add id 
p1_beton <- lapply(seq_along(ps_beton), function(i) Polygons(list(ps_beton[[i]]), #beton
                                                 ID = seq(1:8)[i]))
# add id 
p1_kiebitz <- lapply(seq_along(ps_kiebitz), function(i) Polygons(list(ps_kiebitz[[i]]), #kiebitz
                                                       ID = seq(1:8)[i]))
# create SpatialPolygons object
#beton
beton_polys <- SpatialPolygons(p1_beton, 
                                    proj4string = CRS("+proj=utm +zone=32U+datum=WGS84"))
#kiebitz
kiebitz_polys <- SpatialPolygons(p1_kiebitz, 
                                    proj4string = CRS("+proj=utm +zone=32U+datum=WGS84") )

mapview(beton_polys)+mapview(raster_rot_b) #view beton
mapview(kiebitz_polys)+mapview(raster_rot_k) #view kiebitz

#plot beton
mapview(beton_polys)
plot(beton_polys, axes=T)
plot(raster_rot_b, axes=T)

#plot kiebitz
mapview(kiebitz_polys)
plot(kiebitz_polys, axes=T)
plot(raster_rot_k, axes=T)
