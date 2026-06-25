
# Scots Bay
# 5.75km was taken off the DV T01 to account for side track off the line

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

# Sep 23, 2017 Scots Bay
regions = read.table("RegionSept23.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapSept23.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -34.622, TS50 = -34.730)
#Target Strength (38kHz) =	-34.622 Target Strength (50kHz) =	-34.730


map = mapDat(x = mapping)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Sep23_2017"), ]
# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


resultsa = biomassCalc(x = trans_survey, areaKm = 343.36)
unique(resultsa$total_biomass)

# Load polygons

SUA = read.csv("polygonSB_Shalf.csv")
polySB_main = as.PolySet(SUA, projection="LL")
calcArea(polySB_main)
336.66
# Main

ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))



ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))+  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + coord_map() + labs(x=NULL, y=NULL) 

# 
calcArea(polySB_main) 



#PRC ABC
ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL)


# plankton tow
ggplot(plank,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(color = "red", size = 3) + ggtitle("Plankton Tow Location") + coord_map() + labs(x=NULL, y=NULL)


y1 = 45.0583
x1 = -65.235
plank = as.data.frame(c(x1,y1))

