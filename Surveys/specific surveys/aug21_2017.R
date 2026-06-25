

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

# Aug 21, 2017 Scots Bay
regions = read.table("regionAug21German.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapAug21German.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -34.716 , TS50 = -34.823 )


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug21_2017"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm =819.58)
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



x1 = -66.3148
y1 = 43.5641
plank = as.data.frame(c(x1,y1))

x2 = -66.3148
y2 = 43.55292
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


