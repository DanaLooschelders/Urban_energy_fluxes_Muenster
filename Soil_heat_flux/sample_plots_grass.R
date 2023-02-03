setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
FO_grass_aG<-read.csv("FO_grass_20cm_aG_10cm.csv")

####plot varaince####
x <- cbind(lapply(FO_grass_aG[,1:59], FUN = var, na.rm = T))
vardf <- data.frame('col' = as.numeric(substr(rownames(x),start=2, stop=30)), 'variation' = unlist(x))

ggplot(vardf)+
  geom_line(aes(x=as.factor(round(col, 3)), y= variation, group=1))+
  xlab("Height")+
  ylab("Temperature variation [°C]")+
  geom_vline(aes(xintercept = as.factor(0.472)), col="brown", linewidth=0.5)+
  geom_vline(aes(xintercept = as.factor(0.533)), col="green", linewidth=0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90)) 

####plot temperature profile over time####
#transpose data
FO_grass_long<-data.frame(t(FO_grass_aG[,1:59]))
#set colnames
colnames(FO_grass_long)<-FO_grass_aG$grass_time
#set height as ID
FO_grass_long$ID<-as.factor(round(as.numeric(substr(rownames(FO_grass_long), start=2, stop=30)), 3))
#melt into long format
FO_grass_long = reshape2::melt(FO_grass_long, id.vars = "ID")
#convert type to numeric
FO_grass_long$value<-as.numeric(FO_grass_long$value)
#plot
ggplot(data=FO_grass_long)+
  geom_boxplot(aes(x=ID, y=as.numeric(value), group=ID))+
  theme_bw()+
  geom_vline(aes(xintercept = as.factor(0.472)), col="brown", linewidth=0.9)+
  geom_vline(aes(xintercept = as.factor(0.533)), col="green", linewidth=0.9)+
  theme(axis.text.x = element_text(angle = 90))       

####plot soil heat flux over time####
for(i in 1:length(flux_whole1[[2]])){
  if(i==1){
    dat_shf_grass_whole<-rbind(flux_whole1[[2]][[i]], flux_whole2[[2]][[i]], flux_whole3[[2]][[i]], flux_whole4[[2]][[i]])
  }else{
  dat_shf_grass_temp<-rbind(flux_whole1[[2]][[i]], flux_whole2[[2]][[i]], flux_whole3[[2]][[i]], flux_whole4[[2]][[i]])
  dat_shf_grass_whole<-rbind(dat_shf_grass_temp, dat_shf_grass_whole)
  }
}
dat_shf_grass_whole$depth<-round(dat_shf_grass_whole$depth,3)
#plot
ggplot(data=dat_shf_grass_whole)+
  geom_boxplot(aes(x=as.factor(depth), y=as.numeric(abs(shf)), group=as.factor(depth)))+
  theme_bw()+
  geom_vline(aes(xintercept = as.factor(0.472)), col="brown", linewidth=0.9)+
  #geom_vline(aes(xintercept = as.factor(0.533)), col="green", linewidth=0.9)+
  ylab(label="shf [W m^-2]")+
  xlab(label="height [m]")+
  theme(axis.text.x = element_text(angle = 90))  
