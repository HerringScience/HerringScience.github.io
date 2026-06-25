

### spawning grounds

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "oce")

loadfunctions( "acousticHerring")


can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Tagging 
CP <- as(extent(-69, -64, 42, 46), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


load("germanBox.RData")
load("scotsBox.RData")
load("sealBox.RData")



ggplot(scotsBox,aes(x=X, y=Y)) +geom_polygon(fill = "white", colour = "black", linetype = 3) + geom_polygon(data=germanBox,aes(x=X, y=Y), fill = "white", colour = "black", linetype = 3)  + geom_polygon(data=sealBox,aes(x=X, y=Y), fill = "white", colour = "black", linetype = 3)  + geom_polygon(data=out,aes(x=long, y=lat, group=group))+ coord_map() + xlab("")  + ylab("") + scaleBar(lon = -64.7, lat = 43.3, distanceLon = 25, distanceLat = 5, distanceLegend = -4, dist.unit = "km")+ annotate("text", x = -66.47, y = 43.63, label = "German Bank")+ annotate("text", x = -65.8, y = 43.33, label = "Seal Island")+ annotate("text", x = -65.3, y = 45.28, label = "Scots Bay")


# Without Seal and add labels for provinces and water bodies
ggplot(scotsBox,aes(x=X, y=Y)) +geom_polygon(fill = "white", colour = "black", linetype = 3) + geom_polygon(data=germanBox,aes(x=X, y=Y), fill = "white", colour = "black", linetype = 3)   + geom_polygon(data=out,aes(x=long, y=lat, group=group))+ coord_map() + xlab("")  + ylab("") + scaleBar(lon = -64.7, lat = 43.3, distanceLon = 25, distanceLat = 5, distanceLegend = -4, dist.unit = "km")+ annotate("text", x = -66.43, y = 43.63, label = "German Bank", size = 3.5)+ annotate("text", x = -65.25, y = 45.3, label = "Scots Bay", size = 3.5)+ annotate("text", x = -66, y = 44.9, label = "Bay of Fundy", size = 5)+ annotate("text", x = -65.1, y = 44.5, label = "Nova Scotia", size = 5, colour = "white") + annotate("text", x = -66.55, y = 45.6, label = "New Brunswick", size = 5, colour = "white")

