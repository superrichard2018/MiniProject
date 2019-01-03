## first we are going to call the libraries
install.packages('spdep')
install.packages('gridExtra')
install.packages("corrplot")
library(broom)
library(tmap)
library(geojsonio)
library(sf)
library(raster)
library(tidyverse)
library(spData)
library(spatstat)
library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(gridExtra)
###################################
#library(rgdal)
#englandOA <- readOGR("englandshp/england.shp")
#newEW<- st_as_sf(englandOA)
#head(newEW)

#qtm(newEW)


##### not a good idea data is going to be misreadable
#cran
#englandOA2<-st_transform(englandOA,27700)

#####################
#LondonB <- readOGR("LondonB/LB.shp")

#head(LondonB)
#qtm(LondonB)

########## I knew it to simple

####### new
#LSOA <- readOGR("LSOALondon/LSOALondon2011.shp")
#LSOA1 <- st_as_sf(LSOA)
#head(LSOA1)

#qtm(LSOA1)
### LETS TRANSFROM THIS TOOOOOO SIMPLE FEATURES
#LSOA1 <- st_as_sf(LSOA)
#head(LSOA1)
#tail(LSOA1)

## ate the end the shape of local authorities is going to help
##so i ve decided to go futher with this shapefile

#########my supershapefile########
LA <- readOGR("localA/local2.shp")
head(LA)
LA1 <- st_as_sf(LA)
qtm(LA1)
head(LA1)


#NOW RRECALLL IT the data should be a data frame
library(readxl)
superdata <- read_excel("LOAtransportandhealth4.xlsx")
View(superdata)

## check if the data is a data frame 
class(superdata)

#### now let's merge this thing that has been so painful
LA2 <- merge (LA1,superdata, by.x ="lad16cd", by.y ="LAcode" )
qtm(LA2)
### check it
head(LA2)
###prooofs 

summary (LA2$bicycle)
#summary (LA2$General_Health)
summary (LA2$very_good_health)
####let's proof this function

newvar<-0
recode<-function(variable,high,medium,low){
  newvar[variable<=high]<-"High"
  newvar[variable<=medium]<-"Medium"
  newvar[variable<=low]<-"Low"
  return(newvar)
}


####lets do our chi square test
attach(LA2)
LA2$bicycle_recode <- recode(bicycle, 17755,2529.5,817.2)


LA2$General_Health_recode <- recode(very_good_health,258250,94489,43712)

chisq <- chisq.test(LA2$bicycle_recode, LA2$very_good_health_recode) 

chisq$observed
chisq$expected
chisq$statistic
chisq$p.value
###### new methodology ### adam's methology didnt work for me##### at the end did work:::)))
#head(LA2)
#table(LA2$bicycle,LA2$General_Health)

#chi <- chisq.test(table(LA2$bicycle,LA2$very_good_health))
#chi$statistic
#chi$p.value
################################################
####################################################

varlist <- data.frame(cbind(lapply(LA2, class)))
varlist$id <- seq(1,nrow(varlist))

qplot(bicycle, data = LA2, geom = "histogram")

qplot(very_good_health, data = LA2, gemo = "histogram")

### the data looks terrible bro
#summary (log10(LA2$bicycle))
#summary (log10(LA2$very_good_health))

###please normalize the data better use log10

pb <- qplot (x=bicycle, data = LA2)
pb1 <- qplot(x=bicycle_log, data=LA2)
#pb2 <-  qplot(x=sqrt(bicycle), data=LA2)

grid.arrange(pb,pb1, ncol = 1)

h1 <- qplot(x=very_good_health, data=LA2)
h2 <- qplot(x=very_good_health_log, data=LA2)
#h3 <- qplot(x=sqrt(very_good_health), data=LA2)

#class(pb1)


grid.arrange(h1,h2, ncol=1)

#####lets check a relationship

### couldnt find the way......sorry

###Data without log10 
qplot(very_good_health_log,bicycle_log, data=LA2, geom="point")+stat_smooth(method="lm",se=FALSE,size=1)


##### the plottttttt

##############################

#####next thing

library(broom)

LAmodel <- lm(very_good_health_log ~ bicycle_log, data = LA2 )

LAmodel_res <- tidy(LAmodel)

summary(LAmodel)

plot(LAmodel)

############################# Interpret those horrible plots

####################next thing

LA2$LAmodel_resids <- LAmodel$residuals

tmap_mode("view")



qtm(LA2,fill="LAmodel_resids", fill.palette="blues",borders.lwd= 1)
qtm(LA2,fill="LAmodel_resids")
#############################################
#############################################3
####################################################
####################################################
##################################################

library(spdep)

LA3 <- as(LA2,"Spatial")
coordsW <- coordinates(LA3)
plot(coordsW)

######################################

LA4_nb <- poly2nb(LA3,queen=T)

plot(LA4_nb,coordinates(coordsW), col="blue")

plot(LA3,add=T)

######################################################
##### super duper##### 
##### now the next thing #####

LA5.lw <- nb2listw(LA4_nb, zero.policy=T)

moran.test(LA3@data$LAmodel_resids, LA5.lw, zero.policy = T)

moran.plot(LA3@data$LAmodel_resids, LA5.lw, zero.policy = T)

##############################################
##################################################

#### looking for more variables

library(corrplot)


corvan <- qplot (x=car_or_van, data = LA2)
corvan1 <- qplot(x=car_or_van_log, data=LA2)

grid.arrange(corvan,corvan1, ncol=1)

of <- qplot (x=on_foot, data = LA2)
of1 <- qplot(x=on_foot_log, data=LA2)

grid.arrange(of,of1, ncol=1)


LA2df <- st_set_geometry(LA2,NULL)
LA2df$bicycle_log <- as.numeric(LA2df$bicycle_log) 
cor.data <- with(LA2df, data.frame(very_good_health_log,bicycle_log,car_or_van_log,on_foot_log)) 
class(very_good_health_log)
class(bicycle_log)
class(car_or_van_log)
class(on_foot_log)


#df$colname <- as.numeric(ds$colname)


cor(cor.data)
plot(cor.data)


cor.mat <- cor(cor.data, use="complete")


corrplot.mixed(cor.mat)

LAmodel2 <- lm(very_good_health_log ~ bicycle_log + car_or_van_log + on_foot_log, data = LA2 )

summary(LAmodel2)

LA2$LAmodel2_res <- LAmodel2$residuals

plot(LAmodel2)

qtm(LA2, fill = "LAmodel2_res")
#############################################33

LA77 <- as(LA2,"Spatial")



LA5.lw <- nb2listw(LA4_nb, zero.policy=T)

moran.test(LA77@data$LAmodel2_res, LA5.lw, zero.policy = T)
moran.plot(LA77@data$LAmodel2_res, LA5.lw, zero.policy = T)

#moran.test(LA77@data$LAmodel2_resids, LA5.lw, zero.policy = T)

##moran.plot(LA77@data$LAmodel_resids, LA5.lw, zero.policy = T)
install.packages("spgwr")
library(spgwr)

library(spgwr)

#GWRbandwidth <- gwr.sel(AvgGCSE201 ~ UnauthAbse + Employment + CarsPerHH2, data = LondonWards, coords=cbind(x,y),adapt=T) 
 #bw1<- gwr.sel(very_good_health_log ~ bicycle_log + car_or_van_log + on_foot_log, data = LA2, coords=cbind(x,y), adapt=T)
 bw<- gwr.sel(very_good_health_log ~ bicycle_log + car_or_van_log + on_foot_log, data = LA77, coords=cbind(x,y), adapt=T)
 
 gwr.model=gwr(very_good_health_log ~ bicycle_log +car_or_van_log + on_foot_log, data = LA77, coords=cbind(x,y), adapt = bw, hatmatrix =TRUE, se.fit = TRUE) 
 
 gwr.model
 
 results <- as.data.frame(gwr.model$SDF)
 head(results)
 
 LA77@data$coefbicycle_log <- results$bicycle_log
 LA77@data$coefcar_or_van_log <- results$car_or_van_log
 LA77@data$coefon_foot_log <- results$on_foot_log
 
 #tm_shape(LA77)+
  # tm_polygons(col="coefbicycle_log", palette="RdBu")
 
 qtm(LA77,fill="coefbicycle_log",fill.palette="RdBu")
 qtm(LA77,fill="coefcar_or_van_log",fill.palette="RdBu")
 qtm(LA77,fill="coefon_foot_log",fill.palette="RdBu")
 
 
 sigTest = abs(gwr.model$SDF$bicycle_log) -2 * gwr.model$SDF$bicycle_log 
 
LA77$GWRbicycle_logSig<-sigTest

 
qtm(LA77,fill="GWRbicycle_logSig")


 
 
 #tm_shape(LA77)+
  # tm_polygons(col="coefcar_or_van_log", palette="RdBu")
 
 #tm_shape(LA77)+
  # tm_polygons(col="coefon_foot_log", palette="RdBu")
 
 
 
 
 
 #####Looks interesting.
 #### WHATTT I GOT AN AIRY PROJECTION WHATS THAT?

### Im going to play safe try to change to NBG
#LSOA2<-st_transform(LSOA,27700) #COULDNT

## NEW VARIABLE LSOA1
