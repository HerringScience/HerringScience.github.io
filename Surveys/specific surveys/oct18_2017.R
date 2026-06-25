
# German Bank

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

# Oct 18, 2017 German Bank
regions = read.table("RegionOct18.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapOct18.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -35.5, TS50 = -35.609)
# Standard TS used -35.609, -35.5

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct18_2017"), ]
# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm = 829.72)
unique(resultsa$total_biomass)

map = mapDat(x = mapping)



# Load polygons
# German Bank
SUA = read.csv("polygon_GB2.csv")
polyGB = as.PolySet(SUA, projection="LL")
calcArea(polyGB)

# Figure for report
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID))+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + coord_map() + labs(x=NULL, y=NULL) 

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    

#plankton tow
ggplot(plank2,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_polygon(data=germanBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_point(color = "red", size = 3)+ ggtitle("Tag #444728") + coord_map() + labs(x=NULL, y=NULL) + geom_point(data = plank1, aes(x=x2, y=y2), pch = 3) + coord_map() + ggtitle("GB2017-06")

x2 = -66.3573
y2 = 43.55666
plank1 = as.data.frame(c(x2,y2))

x1 = -66.35 
y1 = 43.5666
plank2 = as.data.frame(c(x1,y1))
