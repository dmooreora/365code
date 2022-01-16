install.packages("rgdal")
install.packages("rgeos")
install.packages("maptools")
library(rgdal)
library(rgeos)
library(ggplot2)



file.exists('../GIS/gis_osm_natural_a_free_1.shp')
map <- readOGR(dsn="../GIS",layer="gis_osm_natural_a_free_1",verbose=FALSE)
map_wgs84 <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
#str(map_wgs84)
#summary(map_wgs84)
write.csv(map_wgs84, "../GIS/gis_osm_natural_a_free_2.csv", row.names=TRUE)
summary(map_wgs84)
plot(map_wgs84, axes=TRUE)

file.exists('C:/Dev/downloads/Addr_Points_010421/Addr_Points_010421.shp')
map <- readOGR(dsn="C:/Dev/downloads/Addr_Points_010421",layer="Addr_Points_010421",verbose=FALSE)
map_wgs84 <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
#str(map_wgs84)
#summary(map_wgs84)
write.csv(map_wgs84, "C:/Dev/downloads/Addr_Points_010421.csv", row.names=TRUE)
summary(map_wgs84)
plot(map_wgs84, axes=TRUE)

file.exists('C:/Dev/Datasets/GIS/gis_osm_places_a_free_1.shp')
map <- readOGR(dsn="C:/Dev/Datasets/GIS",layer="gis_osm_places_a_free_1",verbose=FALSE)
map_wgs84 <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
#str(map_wgs84)
#summary(map_wgs84)
write.csv(map_wgs84, "C:/Dev/Datasets/gis_osm_places_a_free_2.csv", row.names=TRUE)
summary(map_wgs84)
plot(map_wgs84, axes=TRUE)

file.exists('C:/Dev/Datasets/GIS/gis_osm_buildings_a_free_1.shp')
map <- readOGR(dsn="C:/Dev/Datasets/GIS",layer="gis_osm_buildings_a_free_1",verbose=FALSE)
map_wgs84 <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
#str(map_wgs84)
#summary(map_wgs84)
write.csv(map_wgs84, "C:/Dev/Datasets/gis_osm_buildings_a_free_2.csv", row.names=TRUE)
summary(map_wgs84)
plot(map_wgs84, axes=TRUE)

file.exists('C:/Dev/Datasets/GIS/nc.shp')
map <- readOGR(dsn="C:/Dev/Datasets/GIS",layer="nc",verbose=FALSE)
map_wgs84 <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
#str(map_wgs84)
#summary(map_wgs84)
write.csv(map_wgs84, "C:/Dev/Datasets/nc2.csv", row.names=TRUE)
summary(map_wgs84)
plot(map_wgs84, axes=TRUE)



write.csv(map_wgs84, "C:/Dev/Datasets/Guildford_Addressing.csv", row.names=TRUE)
plot(map_wgs84, axes=TRUE)

map2 <- readOGR(dsn="C:/Dev/Datasets/GCVEC_StatePlane",layer="GCVEC_StatePlaneGrid200", verbose=FALSE)
map_wgs84 <- spTransform(map2, CRS("+proj=longlat +datum=WGS84"))
write.csv(map_wgs84, "C:/Dev/Datasets/GCVEC_StatePlaneGrid200.csv",row.names=TRUE)
summary(map_wgs84)
plot(map_wgs84, axes=TRUE)

map3 <- readOGR(dsn="C:/Dev/Datasets/Contour_100ft",layer="Contour_100ft", verbose=FALSE)
map_wgs84 <- spTransform(map3, CRS("+proj=longlat +datum=WGS84"))
write.csv(map_wgs84, "C:/Dev/Datasets/Contour_100ft.csv",row.names=TRUE)
str(map_wgs84)
plot(map_wgs84, axes=TRUE)

map4 <- readOGR(dsn="c:/Dev/Datasets/")


