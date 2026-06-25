

# Script to create grounds map for taggers to help with identifying the proper location

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "ggsn")

loadfunctions( "acousticHerring")

setwd("C:/Users/herri/OneDrive/Documents/Jenna/workspace/")

can<-getData('GADM', country="CAN", level=1) # provinces
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
unique(can1$NAME_1)

NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine", "New Hampshire", "Vermont","Massachusetts"),]

CP <- as(extent(-69, -63, 42, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)
polysT = read.csv("timGrounds.csv")

# look at tagging events by grounds
ggplot(polysT, aes(x=X, y=Y)) + geom_polygon(aes(group=Box, fill = Box), colour = "White", lwd =1) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='antiquewhite3',col='black') + labs(x=NULL, y=NULL) + coord_map()  + ggtitle("Tagging Grounds") + scaleBar(lon = -64, lat = 43.5, distanceLon = 25, distanceLat = 5, distanceLegend = -4, dist.unit = "km")


