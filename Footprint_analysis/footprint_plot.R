#source footprint preparation
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Footprint_analysis/Footprint_prep.r")
#source coordinate rotation script
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Footprint_analysis/footprint_coordinate_transform.R")
library(RColorBrewer)

####calculate area of the largest contour line####
quilt.plot(ffp_x,ffp_y,ffp_f,nx=100,ny=100, xlim=c(-50,100),ylim=c(-180,200))
lines(FFP_para$xr[[8]],  FFP_para$yr[[8]], type="l", col="red")
ellipse<-data.frame("x"=FFP_para$xr[[8]], "y"=FFP_para$yr[[8]])

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
#4712.839

####Quilt Plot####
#create colors for quilt plot
#colspalette<- two.colors(50, middle="grey50" ) #create colorscale between two colors
fun_cols<-colorRampPalette(c("#e8f7f8", "#34a5ab")) #create colorscale
colspalette<-fun_cols(7)  

#original
quilt.plot(ffp_x,ffp_y,ffp_f,nx=100,ny=100, xlim=c(-70,40),ylim=c(-70,40))
for (i in 1:8) lines(FFP_para$xr[[i]],FFP_para$yr[[i]], type="l", col="red")

#modified plot to pdf
pdf(file = "Footprint_Beton_whole.pdf", paper = "a4r")
quilt.plot(ffp_x,#vector of x coordinates
           ffp_y,#vector of y coordinates
           ffp_f, #vector of variable to be plotted
           col=colspalette,#color function
           #nlevel = nr of color levels
           legend.lab="Footprint Climatology [m^2]",
           legend.mar=4,
           legend.cex=0.8,
           legend.line=4,
           nx=160,ny=160, #nr of grid boxed in x/y
           xlim=c(-70,30),ylim=c(-50,40)) #boundaries for plot
for (i in 1:8) lines(FFP_para$xr[[i]],#x-array for contour line of r
                     FFP_para$yr[[i]], #y-array for contour line of r
                     type="l", col="#616161", lwd=0.5)
mtext("Distance [m]", side = 1, line = 2.3)#add axis labels
mtext("Distance [m]", side = 2, line = 2.3)#add axis labels
mtext(paste("Beton Footprint", substr(range(footprint$date)[1],
                                      start=1, stop=11),
            "until", substr(range(footprint$date)[2],
                            start=1, stop=11)), side = 3, line = 1.5)#add title
mtext("contour lines depicting percentage of footprint", 
      side=3, line=0.5,cex = 0.9)
dev.off()

#quilt plot with utm coordiantes
quilt.plot(c(FFP_para$x_2d_UTM),#vector of x coordinates
           c(FFP_para$y_2d_UTM),#vector of y coordinates
           c(FFP_para$fclim_2d),# #vector of variable to be plotted
           col=colspalette,#color function
           #nlevel = nr of color levels
           legend.lab="Footprint Climatology [m^2]",
           legend.mar=4,
           legend.cex=0.8,
           legend.line=4,
           nx=160,ny=160, #nr of grid boxed in x/y
           xlim=c(406510,406610),ylim=c(5755970,5756050)) #boundaries for plot
for (i in 1:8) lines(FFP_para$xr_utm[[i]],#x-array for contour line of r
                     FFP_para$yr_utm[[i]], #y-array for contour line of r
                     type="l", col="#616161", lwd=0.5)
mtext("Easting [UTM]", side = 1, line = 2.3)#add axis labels
mtext("Northing [UTM]", side = 2, line = 2.3)#add axis labels
mtext(paste("Beton Footprint", substr(range(footprint$date)[1],
                                      start=1, stop=11),
            "until", substr(range(footprint$date)[2],
                            start=1, stop=11)), side = 3, line = 1.5)#add title
mtext("contour lines depicting percentage of footprint", 
      side=3, line=0.5,cex = 0.9)


#####3D footprint ####
#3D footprint climatology surface (using the plot3D package)
surf3D(FFP_para$x_2d, FFP_para$y_2d,FFP_para$fclim_2d)

####Crosswind-integrated footprint####
#plot Crosswind-integrated footprint
plot(FFP_para$x_2d,FFP_para$fclim_2d, type="l")

#plot mit fields
#Two-dimensional view of footprint climatology with contour lines of R%.

image.plot(FFP_para$x_2d[1,], FFP_para$y_2d[,1], FFP_para$fclim_2d, 
           xlim=c(-70,30), ylim=c(-50, 40))
for (i in 1:8) lines(FFP_para$xr[[i]], FFP_para$yr[[i]], type="l", col="red")

