RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

loadfunctions( "polygons")
loadfunctions( "acousticTuna")

# weird -  you have to have data points (i.e. surveylines) to be able to run the scale bar

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]


# Proper coordinates for Both
CP <- as(extent(-67, -63, 42, 46), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("surveyBoxes.csv")
# German
germanBox=boxes[which(boxes$Box == "GermanBank"), ]
# Scot's
scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]

# Prince 5
x = -66.8500
y = 44.9300  
prince5 = data.frame(x,y)      

ggplot(prince5, aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') + labs(x=NULL, y=NULL) + scaleBar(lon = -64, lat = 43.5, distanceLon = 25, distanceLat = 5, distanceLegend = -4, dist.unit = "km") + coord_map() + geom_point(colour = "red", size = 4, shape = 3) + geom_polygon(data=germanBox,aes(x=X, y=Y)) + geom_polygon(data=scotsBox,aes(x=X, y=Y)) + annotate("text", x = -66.47, y = 43.63, label = "German Bank")+ annotate("text", x = -65.3, y = 45.28, label = "Scots Bay")


