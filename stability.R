#stability

ggplot(dat=dat.beton.flux.meteo[dat.beton.flux.meteo$X.z.d..L<=1&
                                  dat.beton.flux.meteo$X.z.d..L>=-1,])+
  geom_line(aes(x=TIMESTAMP, y=X.z.d..L))+
  theme_bw()

stability_1<-dat.beton.flux.meteo[, c("TIMESTAMP", "hour_num", "X.z.d..L")]
stability_1$index<-"beton"
stability_2<-dat.kiebitz.flux.meteo[, c("TIMESTAMP", "hour_num", "X.z.d..L")]
stability_2$index<-"grass"

stability<-rbind(stability_1, stability_2)
#get into long format
means_long <- pivot_longer(stability, -c(TIMESTAMP, index, hour_num), values_to = "mean", names_to = "variable")
sd_long <- pivot_longer(stability, -c(TIMESTAMP, index, hour_num), values_to = "sd", names_to = "variable")

df_join <- means_long %>% 
  left_join(sd_long)


#plot
ggplot(data = df_join[df_join$mean<=1&df_join$mean>=-1,], 
       aes(x = hour_num, group = index)) + 
  geom_line(aes(y = mean, color = index), stat="summary", fun="mean", size = 1) + 
  stat_summary(aes(x=hour_num, y=mean, group=index, fill=index),geom="ribbon",
               fun.data = "mean_sdl", fun.args=c(na.rm=T, mult=1), alpha= 0.5)+
  theme_bw()

?stat_summary
