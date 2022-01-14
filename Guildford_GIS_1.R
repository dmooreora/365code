install.packages("rgdal")
install.packages("rgeos")
install.packages("maptools")
library(rgdal)
library(rgeos)

file.exists('C:/Dev/Datasets/Guilford_Addressing_shp/Addr_Points_010421.shp')
map <- readOGR(dsn="C:/Dev/Datasets/Guilford_Addressing_shp",layer="Addr_Points_010421",verbose=FALSE)
map_wgs84 <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
#str(map_wgs84)
#summary(map_wgs84)

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

library(MASS)
fold <- table(survey$Fold)


par(mar = c(1, 1, 1, 1))
pie(fold)

library(maps)
states_map <- map_data("state")

ggplot(states_map, aes(x = long, y = lat, group = group)) +
	geom_polygon(fill = "white", colour = "black")

ggplot(states_map, aes(x = long, y = lat, group = group)) +
	geom_path() + coord_map("mercator")

world_map <- map_data("world")
world_map

east_asia <- map_data("world", region = c("Japan","China","North Korea", "South Korea"))

ggplot(east_asia, aes(x = long, y = lat, group = group, fill = region)) +
	geom_polygon(colour = "black") +
	scale_fill_brewer(palette = "Set2")

nz1 <- map_data("world", region = "New Zealand") %>%
	filter(long > 0 & lat > -48) 

ggplot(nz1, aes(x = long, y = lat, group = group)) +
	geom_path()

nz2 <- map_data("nz")
ggplot(nz2, aes(x = long, y = lat, group = group)) + 
	geom_path()

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimes

library(maps)
states_map <- map_data("state")

crime_map <- merge(states_map, crimes, by.x = "region", by.y = "state")

crime_map

ggplot(crime_map, aes(x = long, y = lat, group = group, fill = Assault)) +
	geom_polygon(colour = "black") +
	coord_map("polyconic")



crime_p <- ggplot(crimes, aes(map_id = state, fill = Assault)) +
	geom_map(map = states_map, colour = "black") +
	expand_limits(x = states_map$long, y = states_map$lat) +
	coord_map("polyconic")

crime_p + scale_fill_gradient2(low = "#559999", mid = "grey90", high = "#BB650B", 
							   midpoint = median(crimes$Assault))

crime_p + scale_fill_viridis_c()

qa <- quantile(crimes$Assault, c(0, 0.2, 0.4, 0.6, 0.8, 1.0))

crimes$Assault_q <- cut(crimes$Assault, qa, labels = c("0-20%","20-40%","40-60%","60-80%","80-100%"), include.lowest=TRUE)

pal <- colorRampPalette(c("#559999","grey80","#BB650B"))(5)

ggplot(crimes, aes(map_id = state, fill = Assault_q)) +
	geom_map(map = states_map, colour = "black") +
	scale_fill_manual(values = pal) +
	expand_limits(x = states_map$long, y = states_map$lat) + 
	coord_map("polyconic") +
	labs(fill = "Assault Rate\nPrecentile")
theme_void

