
# Scots Bay
# To account for the bad data region in ME TO2, the End distance in the region exported echoview file was changed to end dist - 8.75km which is the length of the bad data region which overlaps with the BP T02.

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Aug 26, 2017 Scots Bay
regions = read.table("RegionAug26.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapAug26.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -34.874, TS50 = -34.981)

map = mapDat(x = mapping)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug26_2017"), ]
# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# Remove BP  and C100
trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]
trans_survey2=trans_survey1[which(trans_survey1$Vessel != "C1"), ]
unique(trans_survey2$Vessel)

northern=trans_survey[which(trans_survey$Vessel == "C1"), ]
eastern=trans_survey[which(trans_survey$Vessel == "BP"), ]

resultsa = biomassCalc(x = trans_survey2, areaKm = 629.01)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 94.05)
unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 131.19)
unique(resultsc$total_biomass)

# Load polygons

SUA = read.csv("polygon_SB_aug26_2017.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthernAug262017.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEasternAug36.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


calcArea(polyGB)
#892.91


# Main

# Determine the ordering of polygon points




# Northern box
# Proper coordinates for just northern box
CP <- as(extent(-65.25, -64.75, 45.2, 45.4), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

mapNorth = map[which(map$Vessel == "C1"), ]
ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

SUA = read.csv("polygon_SBNorthernAug262017.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

ggplot(mapNorth,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))+  geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID))  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -64.82, y = 45.1, label = "Main box = 673.24 km2" )+ annotate("text", x = -65.15, y = 45.32, label = "Northern box = 71.83 km2" ) + coord_map() + labs(x=NULL, y=NULL) 





### Eastern Box 
CP <- as(extent(-65, -64.6, 45.2, 45.3), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

mapEast = map[which(map$Vessel == "BP"), ]
ggplot(polySB_eastern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

SUA = read.csv("polygon_SBEasternAug36.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

ggplot(mapEast,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))+  geom_polygon(data=polySB_eastern,aes(x=X, y=Y, group=PID))  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -64.82, y = 45.1, label = "Main box = 673.24 km2" )+ annotate("text", x = -65.15, y = 45.32, label = "Northern box = 71.83 km2" ) + coord_map() + labs(x=NULL, y=NULL) 




#### Main Box 
CP <- as(extent(-65.2, -64.65, 45, 45.4), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

mapMain = map[which(map$Vessel != "BP"), ]
mapMain2 = mapMain[which(mapMain$Vessel != "C1"), ]

ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

SUA = read.csv("polygon_SB_aug26_2017.csv")
polySB_main = as.PolySet(SUA, projection="LL")

ggplot(mapMain2,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))+  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -64.82, y = 45.1, label = "Main box = 673.24 km2" )+ annotate("text", x = -65.15, y = 45.32, label = "Northern box = 71.83 km2" ) + coord_map() + labs(x=NULL, y=NULL) 

# 
calcArea(polySB_northern) 
calcArea(polySB_eastern) 
calcArea(polySB_main) 



#PRC ABC
ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL)

# Figure for report
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), linetype = 2, fill = "white", colour = "blue")+  geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID), colour = "blue", linetype = 2, fill = "white")  +   geom_polygon(data=polySB_eastern,aes(x=X, y=Y, group=PID), linetype = 2, fill = "white", colour = "blue") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -64.82, y = 45.1, label = "Main box = 673.24 km2") + coord_map() + labs(x=NULL, y=NULL) 


calcArea(polySB_main)
# - 1.61km2 (Ile Haute)"


#   
gglocator()
()

calcArea(polySB_main) 
calcArea(GBpoly) 

ggplot(test, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    


# plankton tow
ggplot(plank,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(color = "red", size = 3) + ggtitle("Plankton Tow Location") + coord_map() + labs(x=NULL, y=NULL)


y1 = 45.09333333
x1 = -65.26166667
plank = as.data.frame(c(x1,y1))

