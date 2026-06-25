

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")
unique(boxes$Box)

GBCTD=boxes[which(boxes$Box == "GBocean"), ]


# German Bank      

SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")

# Seal Island      


SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
            

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


trackGB = read.csv("oct31_2021PlanB.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

trackGB = read.csv("oct31_2021PlanA.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

# Plot the transects: German Bank/Seal Island
ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=GBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")

ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")+ ggtitle("Oct 31, 2021 Plan A")

