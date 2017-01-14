##convert decimal degrees to  

library(sp)
library(rgdal)
Soil_coords<-read.csv(Soil_coords)

#omit NA values
Soil_coords<-na.omit(Soil_coords)

#function
x<-Soil_coords$dlong
y<-Soil_coords$dlat

xy<-data.frame(ID = 1:2, X=x, Y=y)
coordinates(xy)<-c("X", "Y")
proj4string(xy)<-CRS("+proj=longlat +datum=WGS84")
  
res<-spTransform(xy, CRS("+proj=utm +zone=14 
  ellps=WGS84"))

res
