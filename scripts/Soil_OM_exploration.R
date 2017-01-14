#organic matter by plot and diversity
library(tidyverse)
library(ggplot2)
library(forcats)
library(mgcv)
soilOM<-read.csv("C:/Users/rbevans2/Desktop/FIELD DATA 2016!!/combined_soil_OMloi.csv")


fit<-aov(OMloi~diversity, data = soilOM)
summary(fit)

ggplot(soilOM, aes(x = diversity, y = OMloi))+ 
  geom_point(size = 2)

boxplot(OMloi~diversity, data = soilOM)
# there is a significant relationship between
# diversity and OMloi

fit2<-aov(OMloi~block, data=soilOM)
summary(fit2)
#there is not a significant relationship between 
#OMloi and block

fit3<-lm(OMloi~diversity, data=soilOM)
summary(fit3)
boxplot(OMloi~diversity, data=soilOM)


fit4<-lm(OMloi~level, data=soilOM)
summary(fit4)
#there is a significant relationship between OMloi and level

boxplot(OMloi~level, data=soilOM)
ggplot(fit4) +
  aes(x=level, y=OMloi) +
  geom_point(stat="identity") +
  geom_smooth()

library(sp)
library(rgdal)
Soil_coords<-read.csv(Soil_coords)

#omit NA values
Soil_coords<-na.omit(Soil_coords)

#function
x<-Soil_coords$dlong
y<-Soil_coords$dlat

xy<-data.frame(ID = 1:2, X=-x, Y=y)
coordinates(xy)<-c("X", "Y")
proj4string(xy)<-CRS("+proj=longlat +datum=WGS84")

res<-spTransform(xy, CRS("+proj=utm +zone=14 
  ellps=WGS84"))


library(mgcv)
library(sp)

OMgam<-gam(OMloi~s(UTM_east, UTM_north), 
    data=combined_soil_OMloi)

gam.check(OMgam)

plot(OMgam, se=FALSE)

library(broom)
fit5<-lm(OMloi~UTM_north + UTM_east + diversity, data = soilOM)
fit5.test<-augment(fit5)

ggplot(soilOM, aes(x=UTM_north, y=OMloi))+
  geom_point(color=(soilOM$diversity))+
  geom_smooth(method = "lm")+
  xlab("Degrees_North")+
  ylab("Organic_Matter_Content (%LOI)")

ombydeg <- ggplot(soilOM, aes(x=UTM_north, y=OMloi)) + geom_point()
ombydeg
# now we need a dataframe with 3 variables in it, SprTmax, BARESOIL and ROCK
# b/c our model uses these three variables. 
# get range of SprTmax to create the sequence
degrange <- range(soilOM$UTM_north)
OMrange<-range(soilOM$OMloi)
# choose the median of the other two variables
nd <- data.frame(UTM_north = seq(degrange[1], degrange[2], length=20),
                 diversity = median(soilOM$diversity),
                 OMloi = median(soilOM$OMloi))
predom <- augment(fit5, newdata=nd)

ombydeg + geom_line(data=predom, mapping = aes(x=UTM_north, y=.fitted))

# We need two different amounts of BARESOIL
# we could create another dataframe and stick them together, or use expand.grid()
nd <- expand.grid(UTM_north = seq(degrange[1], degrange[2], length=20),
                  diversity = quantile(soilOM$diversity, p=c(0.1, 0.9)))
predom <- augment(fit5, newdata=nd)

ombydeg + geom_line(data=predom, mapping = aes(x=UTM_north, y=.fitted, color=(diversity)), lwd=2)
#So more baresoil = fewer species at all levels of max spring temperature
         