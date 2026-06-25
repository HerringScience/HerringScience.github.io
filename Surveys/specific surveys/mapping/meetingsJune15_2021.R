
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Tagging 
  CP <- as(extent(-69, -62, 42, 46), "SpatialPolygons")
# Proper coordinates for GB plankton tow
  CP <- as(extent(-67, -65, 43, 43.6), "SpatialPolygons")
# Coordinates for German Bank and Spec
  CP <- as(extent(-67, -65, 43, 44), "SpatialPolygons")
# Coordinates for German Bank and Trinity
  CP <- as(extent(-67, -65, 43, 46), "SpatialPolygons")
# Scots
  CP <- as(extent(-66, -63, 44, 46), "SpatialPolygons") 
# Scotia Shelf
  CP <- as(extent(-65, -60, 43, 46), "SpatialPolygons") 

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Load Grounds/Boxes
polysT = read.csv("timGrounds.csv")
unique(polysT$Box)
boxes = read.csv("surveyBoxes.csv")

trinity = polysT[which(polysT$Box == "Trinity"), ]
lurcher = polysT[which(polysT$Box == "Lurcher"), ]
gannet = polysT[which(polysT$Box == "Gannet Dry Ledge"), ]

# determining the official boxes for the survey areas
boxes = read.csv("boxestest.csv")
spec = read.csv("specB.csv")
spec2021 = read.csv("specB_2021.csv")
    #2019 points for spec box. 


	


#1	
x = c(-66.1167, -66.1833, -66.2500, -65.7333, -66.1667) 
y = c(43.7333, 43.4167, 43.4167, 43.5000, 43.5000) 


#Scots Bay
x = c(-65.1205, -64.9475, -64.6000, -64.6000) 
y = c(45.3768, 45.0953, 45.2050, 45.3800) 
PID = c(1,1,1,1)
POS = c(1,2,3,4)  

45.3768	-65.1205
45.0953	-64.9475
45.3800	-64.6000
45.2050	-64.6000


z = data.frame(x,y, PID, POS)



boxes = read.csv("surveyBoxes.csv")
unique(boxes$Box)


# Scots Bay
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")






      # points = read.csv("Points.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

head(boxes)

boxesS=boxes[which(boxes$Box != "ScotsBay"), ]

ggplot(boxes,aes(x=X, y=Y))   + geom_polygon(aes(group=PID, colour = Box),fill= NA,lwd=1)+ geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()+ labs(x=NULL, y=NULL)


#2021


ggplot(polySB_main,aes(x=X, y=Y))   + geom_polygon(aes(group=PID, colour = Box), fill = NA,lwd=1) + geom_polygon(data = z, aes(x = x, y = y), fill = "green") + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()+ labs(x=NULL, y=NULL)+ geom_segment(data = spec, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "blue") +ggtitle (" Spectacle Buoy Suggested 2021")



#Scots Bay June 14, 2021


# Scots
CP <- as(extent(-66, -64, 45, 46), "SpatialPolygons") 

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)



ggplot(polySB_main,aes(x=x, y=y)) + geom_polygon(data = z, aes(x = x, y = y), fill = "green") + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()+ labs(x=NULL, y=NULL)+ geom_polygon(data=polyEastern,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + ggtitle("Area 22 with our Survey Boxes")



+ geom_polygon(aes(group=PID), fill = "white",lwd=1)






# ggplot(trackSB, aes(x=X, y=Y))+ geom_polygon(data=polyEastern,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=SBplankton,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=SBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("") + ggtitle("Scots Bay Survey Plan July 7, 2019")
 
  
  geom_segment(data = spec2021, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1.2, colour = "green") 


  
  
  
  
  # Gill netters proposal
  
  # Coordinates for German Bank and Trinity
  CP <- as(extent(-67, -65, 43, 45), "SpatialPolygons")
  
  proj4string(CP) <- CRS(proj4string(NBNS))
  out <- gIntersection(NBNS, CP, byid=TRUE)
  
  # German Bank      
  
  SUA = read.csv("polygon_GB.csv")
  polyGB = as.PolySet(SUA, projection="LL")
  
  # Seal Island      
  
  SUA = read.csv("polygon_SI.csv")
  polySI = as.PolySet(SUA, projection="LL")
  	
### Z
  
  x = c(-66.2500, -65.7333, -65.5, -66.1167) 
  y = c(43.4167, 43.5000, 44.6, 43.7333) 
  PID = c(1,1,1,1)
  POS = c(1,2,3,4)  

  z = data.frame(x,y, PID, POS)

###T
  
  x = c(-66.5000, -66.5000, -66.1333, -66.1333) 
  y = c(44.1667, 43.7500, 43.7500, 44.1667) 
  PID = c(1,1,1,1)
  POS = c(1,2,3,4)  
  
  t = data.frame(x,y, PID, POS)

  
  spec = read.csv("specB2021.csv")
  
  
  ggplot(polyGB,aes(x=x, y=y))+   geom_polygon(data = trinity, aes(x = X, y = Y), colour = "red", fill = "grey", lwd = 0.5)+     geom_polygon(data = gannet, aes(x = X, y = Y), colour = "red", fill = "grey", lwd = 0.5)+   geom_polygon(data = lurcher, aes(x = X, y = Y), colour = "red", fill = "grey", lwd = 0.5)+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='grey',col='red', lwd = 0.5)+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='grey',col='red', lwd=0.5) + geom_polygon(data = z, aes(x = x, y = y), colour = "green",fill =NA, lwd = 1.5)  +   geom_polygon(data = t, aes(x = x, y = y), colour = "green", fill = NA, lwd = 1.5)+ geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()+ labs(x=NULL, y=NULL) + ggtitle("Gillnetters Proposal June 2021") + geom_segment(data=spec, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1)
  
  ggplot(polyGB,aes(x=x, y=y))+   geom_polygon(data = trinity, aes(x = X, y = Y), colour = "red", fill = "grey", lwd = 0.5)+     geom_polygon(data = gannet, aes(x = X, y = Y), colour = "red", fill = "grey", lwd = 0.5)+   geom_polygon(data = lurcher, aes(x = X, y = Y), colour = "red", fill = "grey", lwd = 0.5)+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='grey',col='red', lwd = 0.5)+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='grey',col='red', lwd=0.5) + geom_polygon(data = t, aes(x = x, y = y), colour = "green", fill = NA, lwd = 1.5)+ geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map()+ labs(x=NULL, y=NULL) + ggtitle("Gillnetters Proposal June 2021") + geom_segment(data=spec, aes(x = X, y = Y, xend = Xend, yend = Yend),  colour = "green", lwd=1.5)
  
  
  