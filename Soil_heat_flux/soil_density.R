#Soil density analysis
####Kiebitz####
#weight: 5 measurements [g]
weight_k<-c(280, 249, 293, 307, 296)
weight_k<-weight_k-106 #tara
median_weight<-median(weight_k) #187 g
volume_k<-100 #[cm^3]

density_k<-median_weight/volume_k #1.87 g/cm^3

####Beton####
#estimated: 36.5 (DL), 36.5(OK), 37(CS) [cm]
#measured every two cm [cm]
height_b<-c(37.7,37.1,37.6,37.7,36.4,37.0,
                36.2,36.6,37.0,37.0,35.7,35.2,35.6,35.5,35.7,36.0,36.5,
                37.2,37.0,36.5,36.2,35.7,35.1,34.0,33.3,
                32.5, 33.4, 33.7, 33.0, 33.5, 34.4, 34.8, 34.5, 33.6, 33.0, 34.2,
                34.0, 35.4, 36.2, 36.5, 36.4, 36.6, 36.7, 37.4, 37.8, 38.0, 38.4,
                38.3, 38.3, 38.2, 38.0, 38.0)
mean_height<-mean(height_b) #35.96 [cm]

diameter<-33
radius<-diameter/2
weight_b<-74100 #[g]

volume_b<-pi*radius^2*mean_height #30762.79 cm^3

density_b<-weight_b/volume_b #2.409 g/cm^3

