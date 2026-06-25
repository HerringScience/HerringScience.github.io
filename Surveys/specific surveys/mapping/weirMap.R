

# Create a weir map:

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf")

loadfunctions( "acousticHerring")

can<-getData('GADM', country="CAN", level=1) # provinces
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
unique(can1$NAME_1)

NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine", "New Hampshire", "Vermont","Massachusetts"),]

# Proper coordinates for weir locations 
CP <- as(extent(-67.5, -62, 42, 46), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

polysT = read.csv("timGrounds.csv")

weirs = read.csv("weirMap.csv")

ggplot(weirs,aes(x = LONGITUDE, y = LATITUDE)) + geom_polygon(data=polysT,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey77',col='black') + geom_point(colour = "red") + labs(x=NULL, y=NULL) + coord_map() + ggtitle("All Weir Locations")
