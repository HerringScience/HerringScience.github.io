
# Plot sample locations 

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx")

loadfunctions( "acousticHerring")

# Mapping
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
CP <- as(extent(-68, -63, 43, 46), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

x = c(-64.9333, -66.4)
y = c(45.2, 43.48333333)
date = c( "2017-08-12","2017-07-26")
sample = data.frame(x,y,date)

ggplot(sample, aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(aes(fill = date), pch=21, size = 3, col = "black") + ggtitle("Dr. Ruzzante's Sample Locations 2017")+ coord_map() + labs(x=NULL, y=NULL)