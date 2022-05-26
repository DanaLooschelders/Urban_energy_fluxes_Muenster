library(plotrix)
library(circular)
library(openair)
#wind functions
# Umrechnung von Grad in Bogenmaß
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}


#calculate the wind statistics for beton and kiebitz
#calculate the mean wind direction

#beton
mean(circular(dat.beton.flux.meteo$wind_dir, units = "degrees"), na.rm=T)
atan2(mean(dat.beton.flux.meteo$u_rot, na.rm=T),
      mean(dat.beton.flux.meteo$v_rot, na.rm=T))*(180/pi)+180
#kiebitz
#calculate the mean wind speed
#beton
#kiebitz

#wind rose
#create breaks
grenzen<-c(0,rad2deg(seq(0,2*pi,by=pi/8)+pi/16))
grenzen[length(grenzen)]<-360

#cut data to breaks
beton.wind.dir<-cut(dat.beton.flux.meteo$wind_dir, breaks=grenzen, 
                labels=c("N1", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO",
                         "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N2"))

kiebitz.wind.dir<-cut(dat.kiebitz.flux.meteo$wind_dir, breaks=grenzen, 
                    labels=c("N1", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO",
                             "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N2"))
#rename
levels(beton.wind.dir)<- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO", 
                       "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW","N")

levels(kiebitz.wind.dir)<- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO", 
                           "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW","N")
#absolute frequencies
absfreq_beton<-table(beton.wind.dir)
absfreq_kiebitz<-table(kiebitz.wind.dir)

#relative frequencies
relfreq_beton<-absfreq_beton/sum(absfreq_beton)
relfreq_kiebitz<-absfreq_kiebitz/sum(absfreq_kiebitz)

#degrees for plot
Grad<-c(rad2deg(seq(0,2*pi-pi/8,by=pi/8)))

#labels
label <- c("N", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO", 
                "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

#plot wind beton rel freq rose
pdf(file="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/windrose_beton_relfreq.pdf")
polar.plot(as.numeric(relfreq_beton),polar.pos=Grad,
           main="Beton windrose rel. Freq.",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=deg2rad(Grad),labels=label,
           line.col="blue",
           radial.lim=c(0,max(relfreq_beton)),
           boxed.radial=TRUE)
dev.off()

#plot kiebitz rel freq windrose
pdf(file="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/windrose_kiebitz_relfreq.pdf")
polar.plot(as.numeric(relfreq_kiebitz),polar.pos=Grad,
           main="Beton windrose rel. Freq.",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=deg2rad(Grad),labels=label,
           line.col="blue",
           radial.lim=c(0,max(relfreq_kiebitz)),
           boxed.radial=TRUE)
dev.off()
# Hinweis: aufgrund eines Bugs im Package plotrix (V 3.8-2) muss u.U. label.pos=deg2rad(Grad) gesetzt werden


#Beton Windrose mit absoluten Häufigkeiten:
pdf(file="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/windrose_beton_absfreq.pdf")
polar.plot(as.numeric(absfreq_beton),polar.pos=Grad,
           main="Windrose absolute Häufigkeiten",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=deg2rad(Grad),labels=label, line.col="blue",
           radial.lim=c(0,max(absfreq_beton)),poly.col="blue",
           boxed.radial=TRUE)
dev.off()


#Kiebitz Windrose mit absoluten Häufigkeiten:
pdf(file="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/windrose_kiebitz_absfreq.pdf")
polar.plot(as.numeric(absfreq_beton),polar.pos=Grad,
           main="Windrose absolute Häufigkeiten",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=deg2rad(Grad),labels=label, line.col="blue",
           radial.lim=c(0,max(absfreq_beton)),poly.col="blue",
           boxed.radial=TRUE)
dev.off()
#both absolute windroses in one plot
multiwindrose<-rbind(as.numeric(absfreq_beton),as.numeric(absfreq_kiebitz))

pdf(file="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/windrose_beton_kiebitz_absfreq.pdf")
polar.plot(multiwindrose,polar.pos=Grad,main="Windrose Kiebitz & Beton\nabs freq",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=deg2rad(Grad),labels=label, line.col=c(2,4),
           radial.lim=c(0,max(absfreq_kiebitz)),
           boxed.radial=FALSE)
legend(500,450,c("Abs. freq. Kiebitz", "Abs. freq. Beton"),
       col=c(2,4),lty=1, cex=.5)
dev.off()

#both relative windroses in one plot
multiwindrose<-rbind(as.numeric(relfreq_beton),as.numeric(relfreq_kiebitz))

pdf(file="Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/windrose_beton_kiebitz_absfreq.pdf")
polar.plot(multiwindrose,polar.pos=Grad,main="Windrose Kiebitz & Beton\nrel freq",
           rp.type="p", start=90, clockwise=TRUE, 
           label.pos=deg2rad(Grad),labels=label, line.col=c(2,4),
           radial.lim=c(0,max(relfreq_kiebitz)),
           boxed.radial=FALSE)
legend(500,450,c("Rel. freq. Kiebitz", "Rel. freq. Beton"),
       col=c(2,4),lty=1, cex=.5)
dev.off()
