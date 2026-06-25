

# CHP Box


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
fish=boxes[which(boxes$Box == "Seal_Owen"), ]
tow=boxes[which(boxes$Box == "Tow"), ]
german=boxes[which(boxes$Box == "GermanBank"), ]
SI_2003=boxes[which(boxes$Box == "SI_2003"), ]
Chad=boxes[which(boxes$Box == "ChadSeal"), ]
scots=boxes[which(boxes$Box == "ScotsBay"), ]



# Add CHP box
SUA = read.csv("CHP_polygon.csv")
CHP = as.PolySet(SUA, projection="LL")


SUA = read.csv("BB.csv")
CHP = as.PolySet(SUA, projection="LL")

track3 = read.csv("Aug11_Seal_9.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
points = read.csv("Points.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
points1 = read.csv("Points1.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

ggplot(data=points, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))


# just boxes
ggplot(points, aes(x=X, y=Y)) + geom_polygon(data = out, aes(x=long, y=lat, group=group))+ geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='white',col='black', size = 1.2)+ geom_polygon(data=seal,aes(x=X, y=Y, group=PID),fill='white',col='black', size = 1.2)+ geom_polygon(data=Chad,aes(x=X, y=Y, group=PID),fill='white',col='black', size = 1.2)+ coord_map() + labs(x=NULL, y=NULL) + annotate("text", x = -66.36, y = 43.4, label = "German", size = 5) + annotate("text", x = -66.15, y = 43.4, label = "Seal", size = 5) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "blue")+ annotate("text", x = -66, y = 43.6, label = "Chad's Box", size = 5)


# just boxes
ggplot(points, aes(x=X, y=Y)) + geom_polygon(data = out, aes(x=long, y=lat, group=group))+ geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='white',col='black', size = 1.2)+ geom_polygon(data=seal,aes(x=X, y=Y, group=PID),fill='white',col='black', size = 1.2) + coord_map() + labs(x=NULL, y=NULL) + annotate("text", x = -66.36, y = 43.4, label = "German", size = 5) + annotate("text", x = -66.15, y = 43.4, label = "Seal", size = 5) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "blue")+ annotate("text", x = -66, y = 43.6, label = "Chad's Box", size = 5)





# marcs request
ggplot(points1, aes(x=X, y=Y)) + geom_polygon(data = out, aes(x=long, y=lat, group=group))+ geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='white',col='black', size = 1.2)+ geom_polygon(data=seal,aes(x=X, y=Y, group=PID),fill='white',col='black', size = 1.2)+ coord_map() + labs(x=NULL, y=NULL) + geom_segment(data= points, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "blue") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "red",linetype = 2)




