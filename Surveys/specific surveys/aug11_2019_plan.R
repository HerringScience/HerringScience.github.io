

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 43.8), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("surveyBoxes.csv")
seal=boxes[which(boxes$Box == "Seal_2018B"), ]
CTD = boxes[which(boxes$Box == "GBocean"), ] 


# 9 boats
track3 = read.csv("Aug11_Seal_9.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
track4 = read.csv("Aug11_OA.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

# 7 boats
track5 = read.csv("Aug11_Seal_7.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
track6 = read.csv("Aug11_OB.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

GB = read.csv("Aug11_GB.csv")
SI = read.csv("Aug11_Seal_6.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

# working on the seal box.
ggplot(track5, aes(x=X, y=Y)) + geom_polygon(data = out, aes(x=long, y=lat, group=group))+ geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=seal,aes(x=X, y=Y, group=PID),fill='white',col='black') + coord_map() + labs(x=NULL, y=NULL) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + geom_segment(data = track4, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)

# remove polygons
ggplot(GB, aes(x=X, y=Y)) + geom_polygon(data = out, aes(x=long, y=lat, group=group)) +  geom_polygon(data = CTD, aes(x=X, y=Y)) +  geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + geom_segment(data = SI, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)
