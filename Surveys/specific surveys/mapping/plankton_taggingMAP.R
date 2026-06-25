

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

setwd("C:/Users/herri/Desktop/Ems' Work/")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

SUA = read.csv("polygon_SB_Aug04_2019.csv")
polySB_main = as.PolySet(SUA, projection="LL")

# plankton tow

# CTD/plankton position
x2 = c(-64.9407, -64.95, -64.9509, -64.8353, -64.9492, -64.7235, -64.9172, -65.2132)
y2 = c(45.2642, 45.1632, 45.1587, 45.1768, 45.26863, 45.2713, 45.2834, 45.0516)
type = c("CTD","Tagging","Tagging_CTD","Tagging", "Tagging", "Tagging", "CTD", "CTD")
Vessel = c("SL", "LM", "SL", "LM","LM","LM", "SL","LM")


type = as.factor(type)
plank1 = data.frame(x2,y2, type, Vessel)

boxes = read.csv("timGrounds.csv")
CTD=boxes[which(boxes$Box == "SBocean"), ]

ggplot(plank1,aes(x=x2, y=y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+  geom_polygon(data=CTD,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_point(aes(colour = type, shape = Vessel, size =3)) + coord_map() + labs(x=NULL, y=NULL) 



#Mapping 2 - CTD
CTD1 = data.frame(x2,y2)
boxes = read.csv("timGrounds.csv")
CTD2=boxes

CP <- as(extent(-67.6, -61.8, 43, 45.8), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)
factors=read.csv("CTD_Event.csv")
  factors$Lon= as.factor(factors$Lon)
  factors$Lat = as.factor(factors$Lat)
  factors$ground = as.factor(factors$ground)

 y2=c(factors$Lat)
 x2=c(factors$Lon)
 CTD1 = data.frame(y2,x2)

ggplot(CTD1,aes(x=x2, y=y2))+ 
geom_polygon(data=boxes,aes(x=X, y=Y, group=boxes), fill = "white", colour = "black") + 
geom_point(size = 3, colour ="red")+
geom_polygon(data=out,aes(x=long, y=lat, group=group))
coord_map() 



