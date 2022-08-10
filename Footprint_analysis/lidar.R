library(lidR)

#install.packages("lidRviewer")
setwd("D:/Hafen_07082022/lidars/terra_las")

#read las files in R
lidar<-readLAS("cloud557f384449aa5023.las")

plot(lidar[0:350000000])
#create digital terrain model

