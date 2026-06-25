
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Tagging 
CP <- as(extent(-69, -62, 42, 46), "SpatialPolygons")

# Proper coordinates for GB plankton tow
CP <- as(extent(-67, -65, 43, 43.6), "SpatialPolygons")

    # Coordinates for German Bank and Spec
      CP <- as(extent(-67, -65, 43, 44), "SpatialPolygons")


# Scots
CP <- as(extent(-66, -63, 44, 46), "SpatialPolygons") 

# Scotia Shelf
CP <- as(extent(-65, -60, 43, 46), "SpatialPolygons") 




proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

polysT = read.csv("timGrounds.csv")
boxes = read.csv("surveyBoxes.csv")

# determining the official boxes for the survey areas
boxes = read.csv("boxestest.csv")
spec = read.csv("specB.csv")
spec2021 = read.csv("specB_2021.csv")
    #2019 points for spec box. 


	


	
x = c(-66.1167, -66.1833, -66.2500, -65.7333, -66.1667) 
y = c(43.7333, 43.4167, 43.4167, 43.5000, 43.5000) 

z = data.frame(x,y)




      # points = read.csv("Points.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

head(boxes)

boxesS=boxes[which(boxes$Box != "ScotsBay"), ]

ggplot(boxes,aes(x=X, y=Y))   + geom_polygon(aes(group=PID, colour = Box),fill= NA,lwd=1)+ geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()+ labs(x=NULL, y=NULL)

ggplot(boxesS,aes(x=X, y=Y))   + geom_polygon(aes(group=PID, colour = Box), fill = NA,lwd=1)+ geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()+ labs(x=NULL, y=NULL)+ geom_segment(data = spec, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "blue") + geom_point(data = z, aes(x = x, y = y), size = 6, colour = "green") +ggtitle (" Spectacle Buoy Suggested 2021")
 
  
  geom_segment(data = spec2021, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "green") 







ggplot(boxesS,aes(x=X, y=Y))   + geom_segment(data = spec, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "blue") + geom_point(data = spec2021, aes(x = X, y = Y), size = 6, colour = "green") 


ggplot(z,aes(x=x, y=y))   + geom_point()




(aes(group=PID, colour = Box), fill = NA,lwd=1)+ geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()+ labs(x=NULL, y=NULL)+ geom_segment(data = spec, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "blue") + geom_point(data = spec2021, aes(x = X, y = Y), size = 6, colour = "green") 



head(boxes)
unique(boxes$Box)
german=boxes[which(boxes$Box == "GermanBank"), ]


head(polysT)

#  Plot Seely's Head, Winner and Blacks

	


x1 =c(-66.4852)
y1 = c(43.88995)	
type = c("Seeleys", "Winner", "Blacks", "Flower Pot", "Spruce is Cove")
plank1 = data.frame(x1, y1, type)

x1 =c(-66.1811)
y1 = c(43.2015)	
plank1 = data.frame(x1, y1)


ggplot(plank1,aes(x=x1, y=y1))  + geom_polygon(data=polysT,aes(x=X, y=Y, group=Box, fill = Box), colour  = "white") + geom_point(size =3) + geom_point(size =3) + labs(x=NULL, y=NULL) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()

#+ geom_polygon(data=german,aes(x=X, y=Y, group=Box), fill = "white", col = "black")

head(polysT)


weird = read.csv("MinasBasinWeirs.csv")

germanBox=boxes[which(boxes$Box == "GermanBank"), ]
germanBox$PID = 1
germanBox$POS = 1:4

scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
scotsBox$PID = 1
scotsBox$POS = 1:4

sealBox=boxes[which(boxes$Box == "Seal_2019"), ]
sealBox$PID = 1
sealBox$POS = 1:4


# plankton tow





x1 = c(-66.29406667,-66.07841667,-66.13926667,-66.12276667, -66.17665, -66.08818333, -66.08601667, -66.09261667, -66.09168333, -66.0892)
y1 = c(	43.28953333, 43.2669, 43.30968333, 43.28651667, 43.34158333, 43.50988333, 43.5013, 43.5003, 43.5092, 43.51)
Type = c("tagging","tagging","tagging","tagging","tagging","tow","tow","tow","tow","CTD")
plank1 = data.frame(x1, y1, Type)

# Find Location
x1 = -64.98
y1 = 45.202
plank1 = data.frame(x1, y1)

ggplot(plank1,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(color = "red", size = 3) + ggtitle("Tagging location") + coord_map() + labs(x=NULL, y=NULL)

# German/Seal
ggplot(plank1,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_polygon(data=germanBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_polygon(data=sealBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_point(aes(colour  = Type)) + labs(x=NULL, y=NULL) 

# point
ggplot(plank1,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_polygon(data=germanBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_polygon(data=sealBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_point() + labs(x=NULL, y=NULL) 


+ coord_map() + ggtitle("GB2017-03")



# The Patch
x2 = -62.3288 
y2 = 44.268
plank1 = as.data.frame(c(x2,y2))

# Fishing Location
x2 = -64.95
y2 = 45.26666667

# tag location
x1 = -66.6405
y1 = 44.6121

# fishing location




x1 = c(-66.15, -66.15166667, -66.155, -66.16666667, -66.155)
y1 = c(43.31666667, 43.32016667, 43.31266667, 43.33333333, 43.32333333)
pos = c(1:1)
pos = as.factor(pos)
plank1 = data.frame(x1,y1, pos)

ggplot(plank1,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), colour = "black", fill="white",linetype = 3) + geom_point(aes(group = pos), color = "red", size = 3) + coord_map() 

ggplot(plank1,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  +  geom_point(color = "red", size = 3)+ ggtitle("Tag #449974 Application Site - July 2, 2018") + coord_map() + labs(x=NULL, y=NULL) 

# Tagging location
x1 = -66.4178
y1 = 43.43233
plank = as.data.frame(c(x1, y1))

# weirs in the Minas Basin
ggplot(weird,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_point(color = "red", size = 3)+ ggtitle("2017 Herring Weir Locations") + coord_map() + coord_map() + labs(x=NULL, y=NULL)

# Line of receivers
x = c(-64.93350, -64.78854)
y = c(45.32650, 45.16000)
plank = data.frame(x,y)

ggplot(plank,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=plank,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)

library(geosphere)
r = distm (c(-64.93350, 45.32650), c(-64.78854, 45.16000), fun = distHaversine)
r/1000

# recievers
ggplot(plank,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_line(color="red", size=1) + ggtitle("Proposed Position of Acoustic Recievers") + coord_map() + labs(x=NULL, y=NULL) + annotate("text", x = -65.5, y = 45.75, label = "New Brunswick", colour = "white") + annotate("text", x = -65.5, y = 45.1, label = "Bay of Fundy")

x2 = 
  y2 = 
  plank1 = as.data.frame(c(x2,y2))

gglocator()

