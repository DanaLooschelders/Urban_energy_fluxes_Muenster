library(pracma)
#check Steinfurter Str.
#set wd
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/")
daten_dir <- "Z:/klima/Projekte/2021_CalmCity/2_Daten/8_SteinfurterStrasse/meteo_TOA5/"
#list files from EC02 SS
files_SS_meteo <- list.files(paste0(daten_dir), pattern = ".dat")

for(i in c(1:length(files_SS_meteo))) {
  filename.dat <- files_SS_meteo[i]
  dat.meteo.tmp <- read.csv(paste(daten_dir, filename.dat, sep="/"), header=FALSE,
                            skip=4, stringsAsFactors = F, na.strings=c("NAN", "NA"),
                            col.names=colnames(read.csv(paste(daten_dir, 
                                                              filename.dat, sep="/"), 
                                                        header=T, skip=1, nrows=5)))[ ,1:25] #read only the first 14 columns (the last two contain only the time offset)
  if(filename.dat==files_SS_meteo[1]) {
    dat.SS.meteo <- dat.meteo.tmp
  } else {
    dat.SS.meteo <- rbind(dat.SS.meteo, dat.meteo.tmp)
  }
}

dat.SS.meteo$TIMESTAMP<-as.POSIXct(dat.SS.meteo$TIMESTAMP)
#summarize all variables to mean
dat.SS.agg<-dat.SS.meteo[3:4177,] %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="30 min")) %>%
  summarize_all(~mean(.,na.rm=F))
dat.SS.agg$TIMESTAMP<-as.POSIXct(dat.SS.agg$TIMESTAMP)
#set Steinfurter Str to same time as CalmCity data
dat.SS.agg$TIMESTAMP<-dat.SS.agg$TIMESTAMP+60*60 #substract one hour to match
#cut to same length as concrete/grass towers
dat.SS.cut<-dat.SS.agg[dat.SS.agg$TIMESTAMP>=range(grass.flux.meteo$TIMESTAMP)[1]&
                         dat.SS.agg$TIMESTAMP<=range(grass.flux.meteo$TIMESTAMP)[2],]
#QAQC:
dat.SS.cut$SUp_Avg[dat.SS.cut$SUp_Avg<0]<-0

#calculate albedo
dat.SS.cut$hour<-as.numeric(hour(dat.SS.cut$TIMESTAMP))
dat.SS.cut$albedo<-dat.SS.cut$SDn_Avg/dat.SS.cut$SUp_Avg
mean(dat.SS.cut$albedo[dat.SS.cut$hour>=8&dat.SS.cut$hour<=20], na.rm=T)

#calculate difference with concrete
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg, col="concrete"))+
  geom_line(data=dat.SS.cut, aes(x=TIMESTAMP, y=SUp_Avg, col="Steinfurter"))+
  theme_bw()+
  ggtitle(label="Incoming SW")
ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Energy_Balance",
       filename=paste("SWUp_concrete_grass_Steinf.png"),
       width=297, height=210, units = "mm")
#check difference 
diff_SUp_all<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff_gc"=grass.flux.meteo$SUp_Avg-concrete.flux.meteo$SUp_Avg,
                     "diff_sc"=dat.SS.cut$SUp_Avg-concrete.flux.meteo$SUp_Avg,
                     "diff_sg"=dat.SS.cut$SUp_Avg-grass.flux.meteo$SUp_Avg)
#lag +2 -> + 1h: lag(grass.flux.meteo$SUp_Avg,n=2)-concrete.flux.meteo$SUp_Avg)
#lag -2 -> -1h: grass.flux.meteo$SUp_Avg-lag(concrete.flux.meteo$SUp_Avg, n=2))
#any(grass.flux.meteo$TIMESTAMP!=concrete.flux.meteo$TIMESTAMP) #make sure timestamps fit
ggplot(dat.SS.cut[740:800,])+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg, col="Steinf"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15))+
  geom_line(data=concrete.flux.meteo[740:800,], aes(x=TIMESTAMP, y=SUp_Avg, col="concrete"))+
  geom_line(data=grass.flux.meteo[740:800,], aes(x=TIMESTAMP, y=SUp_Avg, col="grass"))+
  theme_bw()

#test with one hour lag
#plot difference
ggplot(data=diff_SUp_all)+
  geom_line(aes(x=TIMESTAMP, y=diff_gc))+
  theme_bw()+
  ylab(label="Difference SUp [W m^-2]")+
  ggtitle(label="Difference grass - concrete")

ggplot(data=diff_SUp_all[200:280,])+
  geom_line(aes(x=TIMESTAMP, y=diff_sc))+
  theme_bw()+
  ylab(label="Difference SUp [W m^-2]")+
  ggtitle(label="Difference steinfurter - concrete")

ggplot(data=diff_SUp_all)+
  geom_line(aes(x=TIMESTAMP, y=diff_sg))+
  theme_bw()+
  ylab(label="Difference SUp [W m^-2]")+
  ggtitle(label="Difference steinfurter - grass")

diff_SUp_all$hour<-hour(diff_SUp_all$TIMESTAMP)
diff_SUp_all_melted<-melt(diff_SUp_all, id.vars=c("hour", "TIMESTAMP"))
#plot mean day of difference
ggplot(data=diff_SUp_all)+
  geom_boxplot(aes(x=hour, y=diff_gc, group=hour))+
  theme_bw()+
  ggtitle(label="difference SUp grass - concrete")
mean(abs(diff_SUp_all$diff_gc), na.rm=T) #7.339763

ggplot(data=diff_SUp_all)+
  geom_boxplot(aes(x=hour, y=diff_sc, group=hour))+
  theme_bw()+
  ggtitle(label="difference SUp Steinfurter - concrete")
mean(abs(diff_SUp_all$diff_sc), na.rm=T) #29.87899

ggplot(data=diff_SUp_all)+
  geom_boxplot(aes(x=hour, y=diff_sg, group=hour))+
  theme_bw()+
  ggtitle(label="difference SUp Steinfurter - grass")
mean(abs(diff_SUp_all$diff_sg), na.rm=T) #30.93221

#scattering? 
#timestamp kiebitz? -> no
#angle difference? -> impossible

#calculate surface temp
grass.flux.meteo$SurfaceTemp<-nthroot(grass.flux.meteo$LDnCo_Avg/(5.67*10^-8), 4)-272
plot(grass.flux.meteo$SurfaceTemp, type="l")
concrete.flux.meteo$SurfaceTemp<-nthroot(concrete.flux.meteo$LDnCo_Avg/(5.67*10^-8), 4)-272
dat.SS.cut$SurfaceTemp<-nthroot(dat.SS.cut$LDnCo_Avg/(5.67*10^-8), 4)-272
#calculate Rad balance for Steinfurter Str
dat.SS.cut$TotRNet_Avg_2<-dat.SS.cut$SDn_Avg-dat.SS.cut$SUp_Avg+
  dat.SS.cut$LDnCo_Avg-dat.SS.cut$LUpCo_Avg
#rename AirTemp
colnames(dat.SS.cut)[5]<-"AirTC_Avg"

grass.flux.meteo$index<-"grass"
concrete.flux.meteo$index<-"concrete"
dat.SS.cut$index<-"rural"

grass.flux.meteo$hour<-hour(grass.flux.meteo$TIMESTAMP)
concrete.flux.meteo$hour<-hour(concrete.flux.meteo$TIMESTAMP)
dat.SS.cut$hour<-hour(dat.SS.cut$TIMESTAMP)

grass_sub<-grass.flux.meteo[, c("TotRNet_Avg_2", "hour", "index","AirTC_Avg", "SurfaceTemp" )]
concrete_sub<-concrete.flux.meteo[, c( "TotRNet_Avg_2", "hour", "index", "AirTC_Avg", "SurfaceTemp")]
dat.SS.cut<-dat.SS.cut[,  c("TotRNet_Avg_2", "hour", "index", "AirTC_Avg", "SurfaceTemp")]
all_sub<-rbind(grass_sub, concrete_sub, dat.SS.cut)

#Median day of fluxes
ggplot(dat=all_sub)+
  ylab(bquote('Fluxes [W' ~m^-2* ']'))+
  stat_summary(aes(x=hour, y=AirTC_Avg, linetype="AirTC_Avg", color="AirTC_Avg"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=SurfaceTemp, linetype="SurfaceTemp", color="SurfaceTemp"), 
               size=0.8, fun.y=median, geom="line")+
  theme_bw()+
  labs(color  = " ", linetype = " ")+
  scale_color_manual(values=c( "#1f78b4", "#33a02c"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  scale_linetype_manual(values=c(1,2))+
  facet_grid(cols=vars(index))
#totnet rad 

all_sub_air<-all_sub[,1:4]
all_sub_air$ID<-"AirTemp"
colnames(all_sub_air)[4]<-"Temp"
all_sub_surf<-all_sub[,c(1:3,5)]
all_sub_surf$ID<-"SurfaceTemp"
colnames(all_sub_surf)[4]<-"Temp"
all_sub_melt<-rbind(all_sub_air, all_sub_surf)
#air temp
ggplot(dat=all_sub_melt)+
  stat_summary(aes(x=hour, y=AirTC_Avg, col=index, linetype=index), 
               size=0.8, fun.y=median, geom="line")+
  theme_bw()+
  scale_color_manual(values=c( "#1f78b4", "#33a02c", "black"))+
  scale_linetype_manual(values=c(1,2,3))
#surface temp
ggplot(dat=all_sub)+
  stat_summary(aes(x=hour, y=SurfaceTemp, col=index, linetype=index), 
               size=0.8, fun.y=median, geom="line")+
  theme_bw()+
  scale_color_manual(" ", values=c( "#1f78b4", "#33a02c", "black"))+
  scale_linetype_manual(" ", values=c(1,2,3))
#both
ggplot(dat=all_sub_melt)+
  stat_summary(aes(x=hour, y=Temp, col=index, linetype=index), 
               size=0.8, fun.y=median, geom="line")+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 30))+
  scale_color_manual(" ", values=c( "#1f78b4", "#33a02c", "black"))+
  scale_linetype_manual(" ", values=c(1,2,3))+
  facet_grid(cols=vars(ID))
