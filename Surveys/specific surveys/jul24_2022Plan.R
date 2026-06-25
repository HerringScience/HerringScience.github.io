

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")
unique(boxes$Box)

# Scots Bay plankton and CTD box
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]

# Scots Bay
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

trackSB = read.csv("jul24_2022Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)



ggplot(trackSB, aes(x=X, y=Y))+ geom_polygon(data=polyEastern,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("") + ggtitle("Scots Bay #5 Plan July 24, 2022")+ geom_polygon(data=SBplankton,aes(x=X, y=Y, group=PID),fill='NA',col='black') + geom_polygon(data=SBCTD,aes(x=X, y=Y, group=PID),fill='NA',col='black')


