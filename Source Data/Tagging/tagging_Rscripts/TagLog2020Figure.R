

# Figure for the tagging log 2020:

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx")

loadfunctions( "acousticHerring")

# Mapping

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

CP <- as(extent(-67.5, -62, 43, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Load .csv with the polygons for the figures;
# Grounds
polysT = read.csv("timGrounds.csv")

#survey boxes
survey = read.csv("ActiveSurveyBoxes.csv")

  #polysA = read.csv("allanGrounds.csv")

# NAFO Subunits
polysNAFO = read.csv("NAFO_subunits.csv")
ids = c("4Xq", "4Xo", "4Xs", "4Xr", "5Yb", "5Yc")
polys_NAFO = polysNAFO[which((polysNAFO$Area %in% ids)), ] 

# just map no points
ggplot(polysT, aes(x=X, y=Y, group = Box)) + geom_polygon(aes(group=Box),colour = "black", fill = "white") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='white',col='black')+ labs(x=NULL, y=NULL) + coord_map()+theme_bw() + geom_polygon(data=polys_NAFO, aes(x=X, y=Y,group=Area, fill=Area) )

ggplot(out, aes(x=long, y=lat, group = group)) + geom_polygon(colour = "black", fill = "white") + geom_polygon(data=polys_NAFO, aes(x=X, y=Y,group=Area, fill=Area) )


# Aug 18 catches



y1 = c(43.4500, 43.4500, 43.4500, 43.4333)	
x1 = c(-66.4000, -66.4333, -66.4167, -66.4333)
	
y1 = c(43.4167)	
x1 = c(-66.4333)

	
z = data.frame(y1,x1)	



ggplot(polys_NAFO, aes(x=X, y=Y, group = Area)) + geom_polygon(aes(fill = Area), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),col='black')+ geom_polygon(data=survey,aes(x=X, y=Y, group = Box), fill = "white") + geom_point(data=z, aes(x=x1, y=y1))


ggplot(polys_NAFO, aes(x=X, y=Y)) + geom_polygon(data=survey,aes(x=X, y=Y, group = Box), fill = "white") + geom_point(data=z, aes(x=x1, y=y1))




head(polys_NAFO)
 geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='white',col='black')+ labs(x=NULL, y=NULL) + coord_map()+theme_bw()




+geom_text(data=cnames, aes(X, Y, label = Box), size=3.2, angle = 15) 






# find centre point of polygons
cnames <- aggregate(cbind(X, Y) ~ Box, data=boxes, FUN=mean)

# Make adjustments so that the text is in a better place:

# Fix Seal Island
cnames[10,2] = -65.9

#  GBCatch Box
cnames[2,3] = 43.1

#  Gannet
cnames[1,3] = 43.67

#  GBSurvey Box
cnames[3,2] = -66.32
cnames[3,3] = 43.45

#  Grand Manan
cnames[4,3] = 44.56

#  Long Island
cnames[5,3] = 44.7
cnames[5,2] = -66.05

#  NB Coastal
cnames[7,2] = -66.62

#  Trinity
cnames[15,2] = -66.2
cnames[15,3] = 44.12


head(polysNAFO)



# For the CCFFER conference

CP <- as(extent(-67.5, -64.5, 43, 45.45), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

test=boxes[which(boxes$Box != "Shelburne"), ]
test2=test[which(test$Box != "The Patch"), ]
test3=test2[which(test2$Box != "Trinity"), ]

cnames1 = cnames[which(cnames$Box != "The Patch"), ]
cnames2 = cnames1[which(cnames1$Box != "Trinity"), ]
cnames3 = cnames2[which(cnames2$Box != "Shelburne"), ]

cnames3$type = c("Feeding","Feeding", "Spawning/Feeding", "Feeding", "Feeding", "Feeding", "Feeding", "Feeding", "Spawning", "Spawning/Feeding", "Feeding", "Feeding")

ggplot(test3, aes(x=X, y=Y, group = Box)) + geom_polygon(aes(group=Box),colour = "black", fill = "white") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black')+ labs(x=NULL, y=NULL) + coord_map() +geom_label(data=cnames3, aes(X, Y, label = Box, colour  = type), size=4)
