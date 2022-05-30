setwd("C:/00_Dana/Uni/Masterarbeit/FFP_R/20220530090749803")
#footprint_classification_summary.csv: 
        #relative contribution of each land cover class to the
        #measured flux depending on footprint source area 
class_sum<-read.csv(file="20220530090749803_footprint_classification_summary.csv",skip=1 )
#*_footprint_classification_weighted_summary.csv
        #relative contribution of each land cover class to the
        #measured flux depending on footprint source area (applying footprint‐weighting).
class_weigh_sum<-read.csv(file="20220530090749803_footprint_classification_weighted_summary.csv", skip=1)
#*_footprint_raster_x2d.txt
        #Raster with x‐data of 2‐dimensional footprint. Distance [m] from the flux tower, i.e. the tower
        #is located at (0/0). The raster is a m x n two‐dimensional array (matrix) where m corresponds
        #to latitudinal distance and n to longitudinal distance. The x‐coordinate of the lower left corner
        #of the data raster / map corresponds to the very first number of the file.
ffp_fclim2d<-read.table(file="20220530090749803_footprint_raster_fclim2d.txt", sep=" ", 
                       dec=".", header=F)
#remove all NA columns
ffp_fclim2d <- ffp_fclim2d[ , colSums(is.na(ffp_fclim2d)) < nrow(ffp_fclim2d)]

#*_footprint_raster_y2d.txt 
        #Raster with y‐data of 2‐dimensional footprint. Distance [m] from the flux tower
        #is located at (0/0). The raster is a m x n two‐dimensional array (matrix) where m corresponds
        #to latitudinal distance and n to longitudinal distance. The y‐coordinate of the lower left corner
        #of the data raster / map corresponds to the very first number of the file.
ffp_ras_y<-read.table(file="20220530090749803_footprint_raster_y2d.txt", sep=" ", 
                      dec=".", header=F)
#remove all NA columns
ffp_ras_y <- ffp_ras_y[ , colSums(is.na(ffp_ras_y)) < nrow(ffp_ras_y)]
#*_footprint_raster_fclim2d.txt
        #Raster with the footprint function values [m‐2] of the 2‐dimensional footprint.
        #The raster is a max n two‐dimensional array (matrix) where m corresponds to lat distance and n to
        #long distance. The first number of the file corresponds to the footprint value of the
        #lower left corner.
ffp_ras_x<-read.csv2(file="20220530090749803_footprint_raster_x2d_csv.csv", header=F)
#*_log.txt
        #Log file containing your simulation name, tower coordinates, and simulation details

