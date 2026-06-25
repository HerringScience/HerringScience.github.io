
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

# Oct 06, 2017 German Bank
regions = read.table("RegionOct6_17.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapOct6_17.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -34.949, TS50 = -35.057)
# Target Strength (38kHz) =	-34.949, Target Strength (50kHz) =	-35.057

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct06_2017"), ]
# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm = 822.33)
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


