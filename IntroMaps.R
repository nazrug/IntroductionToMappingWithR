#Outline
#Important packages
library(dplyr)
library(ggplot2)
#spatial packages
library(sp)
library(rgdal)
library(maptools)

dir.prj<-getwd()

###########################################################################
######################    POINTS    ######################################
###########################################################################
#How to read in points
ALL_POINTS<-read.csv("AZ_TREE.csv",header=T)

#This is FIA Tree data for the state of AZ, organized by species
str(ALL_POINTS)

#Subset Data - We are Only interested in Ponderosa Pine
unique(ALL_POINTS$common_name)
ALL_POINTS<-subset(ALL_POINTS,common_name == 'ponderosa pine')

#Read in points using the function Spatial Points in the sp package
ALL_POINTS2<-sp::SpatialPoints(dplyr::select(ALL_POINTS,LON,LAT))

#Inspect data
str(ALL_POINTS2)
slotNames(ALL_POINTS2)
head(ALL_POINTS2@coords)
ALL_POINTS2@bbox#sw,ne
ALL_POINTS2@proj4string

#Read in points with associated data
colnames(ALL_POINTS)
#Two Arguments <- Coordinates, Then Associated Data
ALL_POINTS3<-SpatialPointsDataFrame(dplyr::select(ALL_POINTS,LON,LAT),dplyr::select(ALL_POINTS,FORGRP_NAME,TOTAL_BIOMASS))
slotNames(ALL_POINTS3)
head(ALL_POINTS3@data)

#What is the Coordinate Reference System (CRS)
ALL_POINTS3@proj4string

#Spatial objects will either be imported with a CRS, or you will have to assign one
##Helpful pdf <- https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
#WGS84
EPSG <- rgdal::make_EPSG()
head(EPSG)
dim(EPSG)# 5078 rows or 5078 different projections

#Narrow down choice using the filter command from dplyr and grep arguments
WGS <- dplyr::filter(EPSG, grepl('WGS84',prj4),grepl('longlat',prj4))
dim(WGS) #down to 22. Rad!
WGS #we want code = 4326, 8th row

#Assign projection to our SpatialPointsDataFrame
proj4string(ALL_POINTS3) <- CRS(WGS[8,3])
ALL_POINTS3@proj4string

#Once you have your points projected, you can transform them into different projections using the function spTransform
ALL_POINTS_NAD83<-spTransform(ALL_POINTS3,CRS(EPSG[198,3]))
ALL_POINTS_NAD83@proj4string

#Plot points to see that they are in the correct position
plot(ALL_POINTS3)

#plot using ggplot
##ggplot does not accepts Spatial Objects
ggplot()+geom_point(data=ALL_POINTS3,aes(x=LON,y=LAT),color='red')
#Use original dataframe
ggplot()+geom_point(data=ALL_POINTS,aes(x=LON,y=LAT),color='red')

###########################################################################
###################    POLYGONS-SHAPEFILES   ##############################
###########################################################################
###########################################################################
#Option 1 <- Use maps already loaded in R

#Use maps package to load up map of AZ
library(maps)#library(Rworldmaps),library(dismo)

map()
map('france')
map('usa')
map('state',region = c("arizona","new mexico","utah","colorado"))
map('county','arizona')
#map points over plot
plot(ALL_POINTS3,add=TRUE)

############################################################
#Option 2 - pull a map from the internet
library(ggmap)#library(Rgooglemaps),library(OpenStreetMap)

#get_map -> Need at least 3 arguments: locations, zoom, and maptype

#Two options with location
##center
LOC1 <- c(lon=mean(ALL_POINTS$LON),lat=mean(ALL_POINTS$LAT)) 

#bbox
LOC2 <- c(-116,32.8,-108,35.5) #SW,NE corner

#get_map!
map2<-get_map(location=LOC2,zoom=7,maptype='hybrid')

#Plot map2
plot(map2)#doesn't work
map2

map3<-ggmap(map2)#WGS84
map3

?get_map

#plot points over geom_point
map3+geom_point(data=ALL_POINTS,aes(LON,LAT),size=2)

#plot points by forest group
map3+geom_point(data=ALL_POINTS,aes(LON,LAT,color=FORGRP_NAME))

#plot points by forest group (different colors) and size changes by amount PP present
map3+geom_point(data=ALL_POINTS,aes(LON,LAT,size=TOTAL_BIOMASS,fill=FORGRP_NAME),
                alpha=.7,pch=21,color='black')+
                scale_fill_brewer(palette='Dark2',name='Forest Group')+
                scale_size(range = c(1, 8))

####################################################################
#Option  3 - Read in your own shapefile from ArcGis or otherwise
##readOGR - function for reading in shapefiles.
##readOGR(dsn = directory of the shapefile, layer = name of layer without an extension)
##located at http://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
WWF_TE<-rgdal::readOGR(dsn=file.path(dir.prj,'WWF_TE/official'),layer='wwf_terr_ecos')

#Inspect data
str(WWF_TE)
slotNames(WWF_TE)#instead of Coords <- polygons and plotOrder
plot(WWF_TE)

#This file is huge <- Let's "crop" it
WWF_TE@proj4string

library(raster) #contains the crop function we will need

#Crop in by state boundary of AZ
#Create firm AZ spatial polygon using AZ from maps package
AZ <- map("state",region='arizona', fill = TRUE)#fill = TRUE is essential
str(AZ)#not a Spatial Polygon
AZ <- map2SpatialPolygons(AZ, IDs='arizona', proj4string=CRS(WGS[8,3]))
slotNames(AZ)

#crop
WWF_TE_AZ<-crop(WWF_TE,AZ)

#plot
plot(WWF_TE_AZ)

#ggplot
ggplot(WWF_TE_AZ)#doesn't work

#transform
head(WWF_TE_AZ@data)
WWF_TE_AZ@data$id <- rownames(WWF_TE_AZ@data)
#create a data.frame from spatial object
WWF_TE_DF <- fortify(WWF_TE_AZ,region="id")#will create a data frame from the polygons slot, but doesn't include of the data
str(WWF_TE_DF)
WWF_TE_DF <- plyr::join(WWF_TE_DF,WWF_TE_AZ@data)
str(WWF_TE_DF)

#Plot ecoregions using ggplot and the geom_polygon function
ggplot()+geom_polygon(data=WWF_TE_DF,aes(x=long,y=lat,group=group),fill='NA',colour="black")+theme_bw()

#Vary Ecoregion by group
p<-ggplot()+geom_polygon(data=WWF_TE_DF,aes(x=long,y=lat,group=group,fill=ECO_NAME),
                          colour="black")+theme_bw()+
                          guides(fill=guide_legend(title="Ecosystem Name"))
plot(p)

p+geom_point(data=ALL_POINTS,aes(x=LON,y=LAT))
#########################################################
####Other Interesting with Points and Polygons

#Frequency of plots by county

#Switch county map in a Spatial Polygon Object
AZ_Counties <- map('county','arizona',fill=TRUE)
IDs <- sapply(strsplit(AZ_Counties$names, ","), function(x) x[2])
AZ_Counties <- map2SpatialPolygons(AZ_Counties, IDs=IDs, proj4string=CRS(WGS[8,3]))

#Find length of each plot by county
?aggregate
AZ_agg <- aggregate(x = ALL_POINTS3['FORGRP_NAME'], by = AZ_Counties, FUN = length)
head(AZ_agg)
AZ_Counties$count<-AZ_agg$FORGRP_NAME
AZ_Counties@data

#switch to DF for ggplot
AZ_Counties$id<-rownames(AZ_Counties@data)
AZ_CountiesDF <- fortify(AZ_Counties,region="id")
AZ_CountiesDF <- plyr::join(AZ_CountiesDF,AZ_Counties@data)

ggplot()+geom_polygon(data=AZ_CountiesDF,aes(x=long,y=lat,group=group),fill=NA,colour="black")+theme_bw()

p<-ggplot()+geom_polygon(data=AZ_CountiesDF,aes(x=long,y=lat,group=group,fill=count),colour="black")+
  scale_fill_gradient(low = "darkseagreen1", high = "forestgreen",
                      breaks=c(5,400,800),
                      labels=c("Low","Medium","High"))
plot(p)

p+geom_point(data=ALL_POINTS,aes(x=LON,y=LAT))

#Density of Ponderosa Pine Plots in Arizona
ggmap(map2)+
stat_density2d(data=ALL_POINTS,
  aes(x = LON, y = LAT, fill = ..level.., alpha = ..level..,show_guide=FALSE),
  geom = "polygon",
  bins = 20)+
  scale_fill_gradient(low = "grey5", high = "green") +                           #change display colors
  theme(legend.position="none")+                                                    #no legend
  ggtitle("Plot Density for Ponderosa Pine") 

##################################################################################
############################   RASTER   ##########################################
##################################################################################
#Get DEM offline using getData in raster package
#DEM <- getData('alt', country='USA') #- not working!

#Read in raster
DEM <- raster('dem_AZ2')#direct to folder

#Format of raster
DEM

#CRS of raster
newproj<-AZ@proj4string
DEM<-projectRaster(DEM,crs=newproj)#changes projection. If we needed to assign a projection use 'projection' function

#plot raster
plot(DEM)
plot(AZ,add=T)

#crop vs. mask raster
#crop uses bounding box
DEM_AZ<-crop(DEM,extent(AZ))
plot(DEM_AZ)

#while mask will actually use the exact boundary lines
DEM_AZ<-mask(DEM_AZ,AZ)
plot(DEM_AZ)

#ggplot raster
DEM.P<-rasterToPoints(DEM_AZ)
DEM.P<-as.data.frame(DEM.P)
head(DEM.P)

#Use geom_raster
ggplot() +
  geom_raster(data=DEM.P, aes(y=y, x=x,fill=dem_az2))

#change colors
p<-ggplot() +
  geom_raster(data=DEM.P, aes(y=y, x=x,fill=dem_az2)) +
  scale_fill_gradientn(colours = terrain.colors(20))+
  coord_equal() 
plot(p)

p+geom_point(data=ALL_POINTS,aes(x=LON,y=LAT))

#Extract point data - extract function
Elevation<-extract(DEM_AZ,ALL_POINTS3)
ALL_POINTS<-cbind(ALL_POINTS,Elevation)
head(ALL_POINTS)

#Terrain function 
#opt = c("slope", "aspect", "tpi", "tri", "roughness", "flowdir"))
SLOPE_AZ<-terrain(DEM_AZ, opt='slope', unit='radians',neighbors=8)
plot(SLOPE_AZ)
ASPECT_AZ<-terrain(DEM_AZ, opt='aspect', unit='radians',neighbors=8)
plot(ASPECT_AZ)
FLOWDIR_AZ<-terrain(DEM_AZ, opt='flowdir', unit='degrees',neighbors=8)
plot(FLOWDIR_AZ)

?hillShade
HILLSHADE_AZ<-hillShade(SLOPE_AZ, ASPECT_AZ, angle=50, direction=150)
#noon, flagstaff az - 50,150
#angle: 10 - 60
#azimuth 75 - 285

#no hill shade
plot(DEM_AZ,col=rainbow(25))
#hill shade
plot(HILLSHADE_AZ, col=grey(0:100/100),legend=F)
plot(DEM_AZ,col=rainbow(25, alpha=0.35), add=TRUE)

#save output
?writeFormats
writeRaster(HILLSHADE_AZ,'HILLSHADE_AZ.tif',overwrite=TRUE)

#How to plot this same map in ggplot
#first transform rasters
DEM.P<-as.data.frame(rasterToPoints(DEM_AZ))
head(DEM.P)
HILLSHADE.P<-as.data.frame(rasterToPoints(HILLSHADE_AZ))
head(HILLSHADE.P)

ggplot() +
  geom_raster(data=DEM.P,aes(x,y,fill=dem_az2)) +
  scale_fill_gradientn(name="Elevation",colours = rainbow(20)) +
  guides(fill = guide_colorbar()) +
  geom_tile(data=HILLSHADE.P,aes(x,y,alpha=layer), fill = "grey20") +
  scale_alpha(range = c(0, 0.5))+ 
  theme_bw()+
  coord_equal()

#Other Raster Functions - Climate Data
#tmin <- getData("worldclim", var = "tmin", res = 10)#global climate layers (climate grids) with a spatial resolution of about 1 square kilometer                

#http://charcoal.cnre.vt.edu/climate/current/ - Moscow Forest Science Lab USFS
#30 years normal for North America about 1 square kilometers
map <- raster('mapSW.tif') #mean annual precip
mat_tenths <- raster('mat_tenthsSW.tif') #mean annual temp
gsp <- raster('gspSW.tif') #growing season precip

#Inspect Data
plot(mat_tenths)
mat_tenths
minValue(mat_tenths)
maxValue(mat_tenths)
cellStats(mat_tenths,stat='mean')

#change units of mat
mat <- mat_tenths * 10

#create new climate layer from two other layers using overlay function
#pratio <- gsp/map
pratio <- overlay(gsp,map,fun=function(x,y) {x/y})
pratio

#stack or brick climate data
#similar - memory storage is different. Raster brick creates one temporary file, while raster stack pulls from existing files
ClimateStack<-stack(map,mat,gsp,pratio)
ClimateStack
plot(ClimateStack)

#Change names
pratio@data@names <- 'pratio'
mat@data@names <- 'mat'
ClimateStack<-stack(map,mat,gsp,pratio)
plot(ClimateStack)

#assign a projection to all rasters in stack
projection(ClimateStack) <- newproj #WGS84

#Mask, then crop, all rasters in stack
ClimateStack_AZ<-mask(ClimateStack,AZ)
plot(ClimateStack_AZ)
ClimateStack_AZ<-crop(ClimateStack_AZ,extent(AZ))#clean up empty space
plot(ClimateStack_AZ)

#Interesting exploratory plots
pairs(ClimateStack_AZ)
rasterVis::bwplot(ClimateStack_AZ)

#Unstack your raster stack to get back at individual plots
ClimList<-unstack(ClimateStack_AZ)
ClimList
pratio_AZ<-ClimList[[4]]

#Interesting individual plots
hist(pratio_AZ)
rasterVis::levelplot(pratio_AZ)

#Other useful functions found here: http://rpubs.com/etiennebr/visualraster
#resampling, aggregating,disaggregate

#Other geoprocessing packages
library(RSAGA) 
library(spgrass6)
library(sextante)

#QUESTIONS?