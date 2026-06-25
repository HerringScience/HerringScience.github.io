
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx")

loadfunctions( "acousticHerring")

# Mapping
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
CP <- as(extent(-68, -63, 43, 46), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("catchBoxes.csv")



#### This is where you can put the lat and lon locations in:
X = c(-66.41)

Y = c(43.48667)

z = data.frame(X,Y)


# Mapping Figure
ggplot(z, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box, fill = Box), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(pch=21, size = 2, fill = "White") + ggtitle("Tag Releases") + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68"))
