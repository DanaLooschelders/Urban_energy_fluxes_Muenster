####data exploration####
#access metadata for grass
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/11_FODS_Columns/FO-column-grass/FO-column-grass/final")
files<-list.files(pattern="nc")
file<-nc_open(filename=files[2])
print(file)["units:"]
#get start time
substr(ncatt_get(file, "time")$units, start=15, stop=33)
nchar("seconds since 2021-07-29 15:00:24")
nc_close(filename=files[2])
#access metadata for concrete

head(FO_concrete_list[[2]]$LAF)

table(FO_concrete_list[[2]]$unheated_PVC)
length(FO_concrete_list[[2]]$unheated_PVC)

head(FO_concrete_list[[2]]$cal_temp)
dim(FO_concrete_list[[2]]$cal_temp)

#time var:  time  Size:149 units: seconds since 2021-07-29 15:00:24
#dt: 24s (-> 60*60/24 = 150)
#dLAF: 0.254
#unheated_PVC: wrap_1cm;wrap_2cm;wrap_5cm;wrap_8cm
#length(FO_concrete_list[[1]]$LAF)
#60*60/24
#x y z
head(FO_concrete_list[[1]]$x)
tail(FO_concrete_list[[1]]$x)

head(FO_concrete_list[[1]]$y)
tail(FO_concrete_list[[1]]$y)

head(FO_concrete_list[[1]]$z)
tail(FO_concrete_list[[1]]$z)
length(FO_concrete_list[[1]]$z)

plot(FO_concrete_list[[1]]$unheated_PVC)

#test plot concrete
dat<-data.frame(t(FO_concrete_list[[3]]$cal_temp))
colnames(dat)<-FO_concrete_list[[3]]$z
dat$time<-seq(from=1, 60*60, by=24)

dat_long<-gather(data = dat, key, value, -time)
dat_long$key<-as.numeric(dat_long$key)
ggplot(dat_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c()+
  theme_bw()

#test plot grass
dat<-data.frame(t(FO_grass_list[[3]]$cal_temp))

colnames(dat)<-FO_grass_list[[3]]$z
dat$time<-seq(from=1, 60*60, by=24)

dat_long<-gather(data = dat, key, value, -time)
dat_long$key<-as.numeric(dat_long$key)
ggplot(dat_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c()+
  theme_bw()
