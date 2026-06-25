
# German Bank
# To account for the bad data region in ME TO2, the End distance in the region exported echoview file was changed to end dist - 8.75km which is the length of the bad data region which overlaps with the BP T02.

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Sep 17, 2017 German Bank
regions = read.table("RegionSep17.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapSep17.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS50 = -34.694, TS38 = -34.586)
#x$TS<-ifelse((x$Frequency>38), -34.694, -34.586)



# Load school height data

pelagic = read.table("Pelagic_Sep17.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

demersal = read.table("Demersal_Sep17.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

pelagic1 = transects(x = pelagic) 
demersal1 =  transects(x = demersal) 

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Sep17_2017"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm = 841.28)
unique(resultsa$total_biomass)

map = mapDat(x = mapping)


# Load polygons
# German Bank
SUA = read.csv("polygon_GB2.csv")
polyGB = as.PolySet(SUA, projection="LL")
calcArea(polyGB)
#892.91


# Main

# Determine the ordering of polygon points
ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

# plankton tow
ggplot(plank,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(color = "red", size = 3) + geom_point(data=plank1,aes(x=x2, y=y2), color = "blue", size = 3, pch = 3)+ ggtitle("Plankton Tow and CTD Locations") + coord_map() + labs(x=NULL, y=NULL)


x1 = -66.3776
y1 = 43.5679
plank = as.data.frame(c(x1,y1))

x2 = -66.3804
y2 = 43.5590
plank1 = as.data.frame(c(x2,y2))

calcArea(polySB_northern) 
calcArea(polySB_main) 

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL)






# Figure for report
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID))+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + coord_map() + labs(x=NULL, y=NULL) 



#   
gglocator()
()

calcArea(polySB_main) 
calcArea(GBpoly) 

ggplot(test, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()   

# plot two surveys

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(size = PRC_ABC), shape = 21, colour = "black",fill = "white", stroke = 1.4) + labs(x=NULL, y=NULL) +geom_point(data=map1,aes(x=Xend, y=Yend, size = PRC_ABC), colour = "red", alpha = 0.5, stroke = 1.5) + coord_map()   

