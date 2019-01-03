install.packages("spatstat")
library(tmap)
library(geojsonio)
library(sf)
library(raster)
library(tidyverse)
library(spData)
library(spatstat)
library(sp)
library(raster)

## here is the data:

hunt <- geojson_read("https://www.dropbox.com/s/wa2ip35tcmt93g3/Team7.geojson?raw=1", what = "sp")
### look at the structure
str(hunt)
head(hunt)

### find that can extract the shape .....read the shape without 
## this reads the data as spatial polygon
##wwe have four things.
##1.-route(hunt which is geojson file) LIne
##2.-tube stataions that's a KML file which is a section o points could be a polygon, (Week1 when you ask info from
## google maps it will return a geojsonio file).
##3.-Hunt addresses which is a csv file , adam provides a geocoding script, that asks google maps, ir reads script 
##and returns the geojsonio file.
####
##4.-London ward information that is a shapefile a series of polygons and each polygon has attributes.
##Moodle week 6 and the london wards has the city of London in the correct way
 
## the first thing to do is to pick a common formats,,,  u can use Spatial Polygons or SF simple features
###
##(QTM)quick tematic map
###Practical 3 read 
###
#####

##
# where did they go
tmap_mode("view")
tm_shape(hunt) +
  tm_lines (col = "green", lwd = 4)
##Ill try to measure my route with this CRAN
##st_as_sf ------st means is the generic name to refer to simple features  
hunt1 <- st_as_sf(hunt)
st_crs(hunt1)
#we are asking what projection we are using
head(hunt1)
### we also can ask to the head of our data 
### simple features dont work well with rasters
## last question pick the original
st_length(hunt1)
## the great thing about simple features the functions pick
###practical week 3
#### BNG 27700
####WGs84 4326

hunt2 <- st_transform(hunt1, 27700)
st_crs


#### sooooo we converted sp to sf .....we measure the distance and we change the coordinate system

################################################################################################

##install.packages("rgdal")
library(rgdal)
## read as simple fatures
tubestations <- st_read("https://www.doogal.co.uk/LondonStationsKML.ashx","London stations with zone information")
### this st_read read the file in simple features
stations1<-  st_transform(tubestations, 27700) 
head(stations1)
tm_shape(stations1) +
  tm_dots(col="blue")
##next thing is the buffer 
##??st_buffer
hunt3 <- st_buffer(hunt2, dist = 100)
class(hunt2)
tm_shape(hunt3) +
  tm_polygons(col = "red")

###super buffer is done
#### now we should plot the tube stations 
#########################################################################################
library(tidyverse)
huntaddresses <-  read.csv("https://www.dropbox.com/s/2cbu2ux9ddy9c0l/huntaddresses.csv?raw=1")
head(huntaddresses)

### I was almost on the finish line
### Harry Potter's platform is giving a headache. here is a plausible solution
###data.frame[row_number, column_number] = new_value
#huntadresseshunt1 <- huntadresses[29,1] <- Platform3/4
#carSpeeds$Speed[3] <- NA
#iris$Species[iris$Species == 'virginica'] <- 'setosa'
#huntaddresses$Location[29,1] <- 'Platform_9.75_Kings_Cross'
huntaddresses[29,3] <- 51.531427
huntaddresses[29,4] <- -0.1261330
#huntaddresses[29,5] <- -'Euston Rd, Kings Cross, London N1C 4QP, UK'

huntaddresses1 <-  st_as_sf(huntaddresses, coords = c("lon","lat"))
head

#huntaddresses1[29,]<-huntaddresses1[28,]
### c is a funtion which converts alist into a vector 
### transform into a national bNG
st_crs(huntaddresses1)
huntadresses2 <- st_set_crs(huntaddresses1, 4326)
## if it doesnt have a projection I can use this function   st_set_crs
st_crs(huntadresses2)
huntaddresses3 <- st_transform(huntadresses2, 27700)
head(huntaddresses3)
tm_shape(huntaddresses3) +
  tm_dots(col = "green")

##########################################################################################
#####here comes the good part the intersection and that stuff########
###############################################################
#### so I have the hunt3 which is the buffer and huntaddresses3 which is the tube stations##########
#######

findtubestations = st_intersection(hunt3,stations1)
head(findtubestations)

##dim locations versus dimensions
###dim(huntaddresses3)

head (findtubestations)
tm_shape(hunt3)+
  tm_polygons(col='green')+
  tm_shape(findtubestations)+
  tm_dots(col="blue")

###################################### this buffer is for question number 3


hunt4 <- st_buffer(hunt2, dist = 300)
plot(hunt4)
tm_shape(hunt4) +
  tm_polygons(col = "red")

#####new intersection #### this intersection is for the question number 3 

findhunts = st_intersection(hunt4, huntaddresses3)
head (findhunts)
tm_shape(hunt4)+
  tm_polygons(col = "blue")+
  tm_shape(findhunts)+
  tm_dots(col="red")

###  clip the data to a single object
########################### Let's go to the new question number 4
#now we load the london wards

library(rgdal)
LondonWards <- readOGR("London Wards/LondonWards.shp")
LondonWards1 <- st_as_sf(LondonWards)
head(LondonWards1)
LondonWards2<-st_transform(LondonWards1,27700)

### calculate intersections between wards
londonWardsIntersect<- LondonWards2[hunt2,]
qtm(LondonWards2)
qtm(londonWardsIntersect)
print (londonWardsIntersect)


## let's add a new intersection for a nwe window

newsuperintersection <- LondonWards2[huntaddresses3,]
qtm(newsuperintersection)


### to be more clear plot the route and the wards

tm_shape (londonWardsIntersect)+
  tm_polygons(col = NA, alpha = 0.9)+
  tm_shape(hunt) +
  tm_lines (col = "green", lwd = 4)
  


## filtering #filter the data and get the values 
#filter male expectancy

londonWardsIntersect$MaleLE0509
londonWardsIntersect$FemaleLE05
###get the maximun value
londonwardsmax<-filter(londonWardsIntersect,MaleLE0509==max(londonWardsIntersect$MaleLE0509))
###get the minimun value
londonwardsmin<-filter(londonWardsIntersect,MaleLE0509==min(londonWardsIntersect$MaleLE0509))
print (londonwardsmax)
print (londonwardsmin)
## get the mean of Life expentancy male
londonwardsaveragemale<-group_by(londonWardsIntersect)%>%
                    summarise(mean_life = mean (MaleLE0509, na.rm = TRUE))     
print(londonwardsaveragemale)
londonwardsaveragefemale<-group_by(londonWardsIntersect)%>%
                    summarise(mean_life = mean (FemaleLE05, na.rm = TRUE))     
print(londonwardsaveragefemale)

##??filter
##write a file with the info
#write.csv(londonwardsmax, "lmax.csv")
##please chek the file
#lomax <- read.csv(lmax.csv)
### I'll try to get the K value
### but first a window
### well hav eto say this the window command does not work with spacial features, therefore, we have to change it to SP
#LondonWadsIn_sp <- sf:::as_Spatial(londonWardsIntersect)

hunt5 <- hunt4


buffer_sp <- sf:::as_Spatial(hunt5)
window2 <- as.owin(buffer_sp)
plot(window2)

superhunt <- huntaddresses3
print(superhunt)
#### hey change the points from sf to sp
score <- sf:::as_Spatial(superhunt)


#### now we can run the window
window <- as.owin(LondonWards)
plot(window)




#### now we should create a ppp object
huntpoints <- score
huntpoints.ppp <- ppp(x=huntpoints@coords[,1],y=huntpoints@coords[,2],window=window)
plot (huntpoints.ppp,pch=16,cex=1,main="huntpoints")
###plot the density of the points 500
plot(density(huntpoints.ppp, sigma = 500))

###plot the density of the points 1000
plot(density(huntpoints.ppp, sigma = 1000))

#Quadrant Analysis 
plot(huntpoints.ppp,pch=16,cex=1, main="huntppoints")
#now count the points in that fall in a 10 x 10 grid overlaid across the window
plot(quadratcount(huntpoints.ppp, nx = 10, ny = 10),add=T,col="red")

#K test
K <- Kest(huntpoints.ppp, correction="border")
plot(K)

library(raster)
library(fpc)
library(plyr)
library(OpenStreetMap)



#first extract the points from the spatial points data frame
newhuntpoints <- data.frame(huntpoints@coords[,1:2])
#now run the dbscan analysis
db <- fpc::dbscan(newhuntpoints, eps = 600, MinPts = 2)
#now plot the results
plot(db, newhuntpoints, main = "DBSCAN Output", frame = F)
plot(LondonWadsIn_sp, add=T)
















