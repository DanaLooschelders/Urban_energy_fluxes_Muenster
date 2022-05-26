library(raster)
#install.packages("spdep")
library(spdep)
#install.packages("SMoLR")
#library(SMoLR)
library(sf)
library(sp)

####coordinate transformation####
#The trick is to use a rectangular projection for your map, 
#which is fine for the size ranges of a typical footprint.
#This can be UTM, or also SWEREF.  
#In this projection, one unit is equivalent to one meter.
#Hence, you can set the footprint model to a grid resolution of 1 m 
#using dx and dy input parameters.
#The sensor location in the footprint grid is at (0/0). 
utm_x_east<-406579.60    #UTM Easting 406579.60 (x coordinate)
utm_y_north<-5756021.19  #UTM Northing 5756021.19 (y coordinate)
#This then needs a simple coordinates transfer to the sensor location in 
#your map reference system (e.g. SWEREF), i.e. add the values of your 
#sensor location to all footprint grid values.
FFP_para$x_2d_UTM<-FFP_para$x_2d+utm_x_east
FFP_para$y_2d_UTM<-FFP_para$y_2d+utm_y_north
#If you have a map resolution of < 1m, the same system applies, 
#just fit the footprint grid to the map grid.
beton_dataframe<-data.frame("x"=c(FFP_para$x_2d_UTM), "y"=c(FFP_para$y_2d_UTM), 
                            "values"=c(FFP_para$fclim_2d))
beton_matrix<-as.matrix(beton_dataframe)
####rotate in mean wind direction####
#calculate mean wind dir
atan2(mean(footprint$u_comp, na.rm=T),
      mean(footprint$v_comp, na.rm=T))*(180/pi)+180
#mean wind dir is 270 -> direction on x axis
#90 would we east --> rotate -80 degrees --> 280 degrees
#source function from package
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/Coordinate_Rotation_function_from_SMoLR.R")
beton_matrix_rot<-rotate_coord(x=beton_matrix[,1], y=beton_matrix[,2], 
             angle = 280, center = c(utm_x_east, utm_y_north), type="degrees",
             method="polar")

beton_matrix[,1:2]<-beton_matrix_rot
#create raster
test_raster<-rasterFromXYZ(beton_matrix,res = c(1,1), 
                           crs = "+proj=utm +zone=32U+datum=WGS84",digits = 0.3)

#view
plot(test_raster)
mapview(test_raster)
#create spatial polygons for contourlines
FFP_para$xr_utm<-lapply(FFP_para$xr, function(x) x+utm_x_east)
FFP_para$yr_utm<-lapply(FFP_para$yr, function(x) x+utm_y_north)
#create list with x and y coordinates
lines_list<-mapply(cbind,FFP_para$xr_utm, FFP_para$yr_utm, SIMPLIFY=FALSE)

#rotate coordinate system
#lines_list<-lapply(lines_list, function(x) rotate_coord(as.matrix(x)[,1], 
#                                                            as.matrix(x)[,2],
#                                            angle = 280, 
#                                            center = c(utm_x_east, utm_y_north), 
#                                            type="degrees",
#                                            method="polar"))
#remove NAs
for(i in 1:length(lines_list)){
  lines_list[[i]]<-lines_list[[i]][complete.cases(lines_list[[i]]),]
}

#create polygons
ps <- lapply(lines_list, Polygon)
# add id 
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                 ID = seq(1:8)[i]))
# create SpatialPolygons object
my_spatial_polys <- SpatialPolygons(p1, 
                                    proj4string = CRS("+proj=utm +zone=32U+datum=WGS84") )
mapview(my_spatial_polys)+mapview(test_raster)

mapview(my_spatial_polys)
plot(my_spatial_polys, axes=T)
plot(test_raster, axes=T)