#source footprint preparation
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Footprint_analysis/Footprint_prep.r")
#source coordinate rotation script
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Footprint_analysis/footprint_coordinate_transform.R")
library(RColorBrewer)

#create colors for quilt plot
#colspalette<- two.colors(50, middle="grey50" ) #create colorscale between two colors
fun_cols<-colorRampPalette(c("#e8f7f8", "#34a5ab")) #create colorscale
colspalette<-fun_cols(7) 

####Beton####
####calculate area of the largest contour line####
quilt.plot(ffp_x_b,ffp_y_b,ffp_f_b,nx=100,ny=100, xlim=c(-122,70),ylim=c(-140,60))
lines(FFP_beton$xr[[8]],  FFP_beton$yr[[8]], type="l", col="red")
ellipse<-data.frame("x"=FFP_beton$xr[[8]], "y"=FFP_beton$yr[[8]])

# Center of ellipse from Stackoverflow
#https://stackoverflow.com/questions/60423973/how-to-calculate-the-area-of-a-ggplot-stat-ellipse-when-type-norm
ctr = MASS::cov.trob(ellipse)$center 
# I tried changing this to 'stats::cov.wt' instead of 'MASS::cov.trob' 
#from what is saw from (https://github.com/tidyverse/ggplot2/blob/master/R/stat-ellipse.R#L98)
# Calculate distance to center from each point on the ellipse
dist2center <- sqrt(rowSums((t(t(ellipse)-ctr))^2))
# Calculate area of ellipse from semi-major and semi-minor axes. 
#These are, respectively, the largest and smallest values of dist2center. 
area_beton<-pi*min(dist2center)*max(dist2center)
#17407.35

####Quilt Plot####

#original
quilt.plot(ffp_x_b,ffp_y_b,ffp_f_b,nx=100,ny=100, xlim=c(-122,70),ylim=c(-140,60))
for (i in 1:8) lines(FFP_beton$xr[[i]],FFP_beton$yr[[i]], type="l", col="red")

#modified plot to pdf
pdf(file = "Footprint_Beton_whole.pdf", paper = "a4r")
quilt.plot(ffp_x_b,#vector of x coordinates
           ffp_y_b,#vector of y coordinates
           ffp_f_b, #vector of variable to be plotted
           col=colspalette,#color function
           #nlevel = nr of color levels
           legend.lab="Footprint Climatology [m^2]",
           legend.mar=4,
           legend.cex=0.8,
           legend.line=4,
           nx=160,ny=160, #nr of grid boxed in x/y
           xlim=c(-122,70),ylim=c(-140,60)) #boundaries for plot
for (i in 1:8) lines(FFP_beton$xr[[i]],#x-array for contour line of r
                     FFP_beton$yr[[i]], #y-array for contour line of r
                     type="l", col="#616161", lwd=0.5)
mtext("Distance [m]", side = 1, line = 2.3)#add axis labels
mtext("Distance [m]", side = 2, line = 2.3)#add axis labels
mtext(paste("Beton Footprint", substr(range(footprint_beton$date)[1],
                                      start=1, stop=11),
            "until", substr(range(footprint_beton$date)[2],
                            start=1, stop=11)), side = 3, line = 1.5)#add title
mtext("contour lines depicting percentage of footprint", 
      side=3, line=0.5,cex = 0.9)
dev.off()

#quilt plot with utm coordiantes
quilt.plot(c(FFP_beton$x_2d_UTM),#vector of x coordinates
           c(FFP_beton$y_2d_UTM),#vector of y coordinates
           c(FFP_beton$fclim_2d),# #vector of variable to be plotted
           col=colspalette,#color function
           #nlevel = nr of color levels
           legend.lab="Footprint Climatology [m^2]",
           legend.mar=4,
           legend.cex=0.8,
           legend.line=4,
           nx=160,ny=160, #nr of grid boxed in x/y
           xlim=c(406460,406645),ylim=c(5755910,5756070)) #boundaries for plot
for (i in 1:8) lines(FFP_beton$xr_utm[[i]],#x-array for contour line of r
                     FFP_beton$yr_utm[[i]], #y-array for contour line of r
                     type="l", col="#616161", lwd=0.5)
mtext("Easting [UTM]", side = 1, line = 2.3)#add axis labels
mtext("Northing [UTM]", side = 2, line = 2.3)#add axis labels
mtext(paste("Beton Footprint", substr(range(footprint_beton$date)[1],
                                      start=1, stop=11),
            "until", substr(range(footprint_beton$date)[2],
                            start=1, stop=11)), side = 3, line = 1.5)#add title
mtext("contour lines depicting percentage of footprint", 
      side=3, line=0.5,cex = 0.9)


#####3D footprint 
#3D footprint climatology surface (using the plot3D package)
surf3D(FFP_beton$x_2d, FFP_beton$y_2d,FFP_beton$fclim_2d)

####Crosswind-integrated footprint
#plot Crosswind-integrated footprint
plot(FFP_beton$x_2d,FFP_beton$fclim_2d, type="l")

#plot mit fields
#Two-dimensional view of footprint climatology with contour lines of R%.

image.plot(FFP_beton$x_2d[1,], FFP_beton$y_2d[,1], FFP_beton$fclim_2d, 
           xlim=c(-70,30), ylim=c(-50, 40))
for (i in 1:8) lines(FFP_beton$xr[[i]], FFP_beton$yr[[i]], type="l", col="red")

####Kiebitz####
#calculate area of the largest contour line
quilt.plot(ffp_x_k,ffp_y_k,ffp_f_k,nx=100,ny=100, xlim=c(-80,40),ylim=c(-80,40))
lines(FFP_kiebitz$xr[[8]],  FFP_kiebitz$yr[[8]], type="l", col="red")
ellipse<-data.frame("x"=FFP_kiebitz$xr[[8]], "y"=FFP_kiebitz$yr[[8]])

# Center of ellipse from Stackoverflow
#https://stackoverflow.com/questions/60423973/how-to-calculate-the-area-of-a-ggplot-stat-ellipse-when-type-norm
ctr = MASS::cov.trob(ellipse)$center 
# I tried changing this to 'stats::cov.wt' instead of 'MASS::cov.trob' 
#from what is saw from (https://github.com/tidyverse/ggplot2/blob/master/R/stat-ellipse.R#L98)
# Calculate distance to center from each point on the ellipse
dist2center <- sqrt(rowSums((t(t(ellipse)-ctr))^2))
# Calculate area of ellipse from semi-major and semi-minor axes. 
#These are, respectively, the largest and smallest values of dist2center. 
area_kiebitz<-pi*min(dist2center)*max(dist2center)
#9326.346

####Quilt Plot
#original
quilt.plot(ffp_x_k,ffp_y_k,ffp_f_k,nx=100,ny=100, xlim=c(-80,40),ylim=c(-80,40))
for (i in 1:8) lines(FFP_kiebitz$xr[[i]],FFP_kiebitz$yr[[i]], type="l", col="red")

#modified plot to pdf
pdf(file = "Footprint_Kiebitz_whole.pdf", paper = "a4r")
quilt.plot(ffp_x_k,#vector of x coordinates
           ffp_y_k,#vector of y coordinates
           ffp_f_k, #vector of variable to be plotted
           col=colspalette,#color function
           #nlevel = nr of color levels
           legend.lab="Footprint Climatology [m^2]",
           legend.mar=4,
           legend.cex=0.8,
           legend.line=4,
           nx=160,ny=160, #nr of grid boxed in x/y
           xlim=c(-70,30),ylim=c(-50,40)) #boundaries for plot
for (i in 1:8) lines(FFP_kiebitz$xr[[i]],#x-array for contour line of r
                     FFP_kiebitz$yr[[i]], #y-array for contour line of r
                     type="l", col="#616161", lwd=0.5)
mtext("Distance [m]", side = 1, line = 2.3)#add axis labels
mtext("Distance [m]", side = 2, line = 2.3)#add axis labels
mtext(paste("Beton Footprint", substr(range(footprint_kiebitz$date)[1],
                                      start=1, stop=11),
            "until", substr(range(footprint_kiebitz$date)[2],
                            start=1, stop=11)), side = 3, line = 1.5)#add title
mtext("contour lines depicting percentage of footprint", 
      side=3, line=0.5,cex = 0.9)
dev.off()

#quilt plot with utm coordiantes
quilt.plot(c(FFP_kiebitz$x_2d_UTM),#vector of x coordinates
           c(FFP_kiebitz$y_2d_UTM),#vector of y coordinates
           c(FFP_kiebitz$fclim_2d),# #vector of variable to be plotted
           col=colspalette,#color function
           #nlevel = nr of color levels
           legend.lab="Footprint Climatology [m^2]",
           legend.mar=4,
           legend.cex=0.8,
           legend.line=4,
           nx=160,ny=160, #nr of grid boxed in x/y
           xlim=c(406630,406690),ylim=c(5755910,5755970)) #boundaries for plot
for (i in 1:8) lines(FFP_kiebitz$xr_utm[[i]],#x-array for contour line of r
                     FFP_kiebitz$yr_utm[[i]], #y-array for contour line of r
                     type="l", col="#616161", lwd=0.5)
mtext("Easting [UTM]", side = 1, line = 2.3)#add axis labels
mtext("Northing [UTM]", side = 2, line = 2.3)#add axis labels
mtext(paste("Beton Footprint", substr(range(footprint_kiebitz$date)[1],
                                      start=1, stop=11),
            "until", substr(range(footprint_kiebitz$date)[2],
                            start=1, stop=11)), side = 3, line = 1.5)#add title
mtext("contour lines depicting percentage of footprint", 
      side=3, line=0.5,cex = 0.9)


#####3D footprint
#3D footprint climatology surface (using the plot3D package)
surf3D(FFP_kiebitz$x_2d, FFP_kiebitz$y_2d,FFP_kiebitz$fclim_2d)

####Crosswind-integrated footprint
#plot Crosswind-integrated footprint
plot(FFP_kiebitz$x_2d,FFP_kiebitz$fclim_2d, type="l")

#plot mit fields
#Two-dimensional view of footprint climatology with contour lines of R%.

image.plot(FFP_kiebitz$x_2d[1,], FFP_kiebitz$y_2d[,1], FFP_kiebitz$fclim_2d, 
           xlim=c(-80,31), ylim=c(-80, 40))
for (i in 1:8) lines(FFP_kiebitz$xr[[i]], FFP_kiebitz$yr[[i]], type="l", col="red")

####plot with tmap
library(tmap)
library(tmaptools)
library(stars)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(OpenStreetMap)
#tmap
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Footprint")
tmap_options(check.and.fix = TRUE)
#add location point
points <- data.frame(name = c("EC02", "EC04"),
                     y = c(51.9470462,51.94644),
                     x = c(7.6407622, 7.64214 ),
                     stringsAsFactors = F)

points <- st_as_sf(points, coords = c("x", "y"), crs = 4326)

map<-tm_basemap(leaflet::providers$OpenStreetMap.DE)+
  #tm_shape(raster_rot_b,
               # raster.downsample = FALSE)+
#tm_raster(title = "Footprint Climatology")+ #palette = cols,
  tm_shape(beton_polys)+
  tm_borders()+
  tm_shape(kiebitz_polys)+
  tm_borders()+
  tm_shape(points) + tm_dots()+
  #tm_compass()+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8)

l_map<-tmap_leaflet(map)
l_map

mapshot(l_map, file = "leaflet_beton_footprint.png")

tmap_leaflet(map)

tmap_save(map, "Footprint_map_Beton.png")

#save with basemap
c_osm <- tmaptools::read_osm(bb(NLD_muni))#, ext = 1.05


map_wb <- tm_shape(c_osm)+
  tm_rgb() 
  #tm_shape(raster_rot_b,
  # raster.downsample = FALSE)+
  #tm_raster(title = "Footprint Climatology")+ #palette = cols,
  tm_shape(beton_polys)+
  tm_borders()+
  #tm_compass()+
  tm_scale_bar(bg.color="white")+
  tm_grid(n.x=4,n.y=4,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")+
  tm_layout(legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8)
map_wb

####ggmap####
library(ggplot2)
library(ggmap)
ggmap::get_stamenmap()%>%ggmap()
?ggmap

qmplot(lat=x_2d_UTM, lon=y_2d_UTM, data=FFP_kiebitz)
qmap(location="muenster")
?register_google
?qmplot
(FFP_kiebitz$x_2d_UTM),#vector of x coordinates
c(FFP_kiebitz$y_2d_UTM),#vector of y coordinates
c(FFP_kiebitz$fclim_2d)