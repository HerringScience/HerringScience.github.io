
# Scots Bay
# To account for the bad data region in ME TO2, the End distance in the region exported echoview file was changed to end dist - 8.75km which is the length of the bad data region which overlaps with the BP T02.

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
    # Northern box 
    CP <- as(extent(-65.25, -64.75, 45.25, 45.4), "SpatialPolygons")
        # Look closer at main box
        CP <- as(extent(-65.3, -64.55, 45, 45.3), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Aug 12, 2017 Scots Bay
regions = read.table("RegionAug12.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapAug12.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -34.681, TS50 = -34.788)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug12_2017"), ]
# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

map = mapDat(x = mapping)



# Remove C1  and for Aug12, 2017
trans_survey1=trans_survey[which(trans_survey$Vessel != "C1"), ]
northern=trans_survey[which(trans_survey$Vessel == "C1"), ]

resultsa = biomassCalc(x = trans_survey1, areaKm = 643.38)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 86.36)
unique(resultsb$total_biomass)

# Load polygons

# Main
SUA = read.csv("polygon_SB_aug12.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthernAug12.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

# Determine the ordering of polygon points
ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))


# Scot's Bay
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))+ geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + coord_map() + labs(x=NULL, y=NULL) 

# Only main box
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + coord_map() + labs(x=NULL, y=NULL) 

# Only northern
SUA = read.csv("polygon_SBNorthernAug12.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

ggplot(northernMAP,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID))+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + coord_map() + labs(x=NULL, y=NULL) 


ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

northernMAP=map[which(map$Vessel == "C1"), ]



# plankton tow
ggplot(plank,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(color = "red", size = 3) + geom_point(data=plank1,aes(x=x2, y=y2), color = "blue", size = 3, pch = 3)+ ggtitle("Plankton Tow and CTD Locations") + coord_map() + labs(x=NULL, y=NULL)



x1 = -65.2081
y1 = 45.1734
plank = as.data.frame(c(x1,y1))

x2 = -65.2066
y2 = 45.1742
plank1 = as.data.frame(c(x2,y2))

calcArea(polySB_northern) 
calcArea(polySB_main) 

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL)






# Figure for report
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))+  geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -64.82, y = 45.1, label = "Main box = 673.24 km2" )+ annotate("text", x = -65.15, y = 45.32, label = "Northern box = 71.83 km2" ) + coord_map() + labs(x=NULL, y=NULL) 


calcArea(polySB_main)
# - 1.61km2 (Ile Haute)"


#   
gglocator()
()

calcArea(polySB_main) 
calcArea(GBpoly) 

ggplot(test, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    


