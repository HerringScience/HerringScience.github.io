These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

tagging = read.csv("tagging2018.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
head(tagging)
tagging$Date = as.Date(tagging$Date, "%Y-%m-%d")
tagging$month =month(tagging$Date)
tagging$Cost = as.factor(tagging$Cost)
tagging$Tagger = as.factor(tagging$Tagger)


# Number of tags applied per month
test<-aggregate(Total_Tagged_Calc~month, tagging, FUN=sum)

# Number of tags applied per survey
test<-aggregate(Total_Tagged_Calc~Associated.Survey, tagging, FUN=sum)


test1=aggregate(Cost~Associated.Survey, data=tagging, sum, na.rm=TRUE)
test2=aggregate(Total_Tagged_Calc~Associated.Survey, data=tagging, sum, na.rm=TRUE)
combo =merge(test1, test2, by = "Associated.Survey")
combo=combo[which(combo$Associated.Survey != "None"), ]

ggplot(combo, aes(x=Associated.Survey, y=Cost)) + geom_point(colour = "red", size = 3)  + labs(title = "Number of Tags Applied Per Survey", y = "number of tags")
combo$Cost = as.factor(combo$Cost)


ggplot(data=combo,aes(Associated.Survey, Total_Tagged_Calc, colour=Cost)) + geom_point(size = 2) + ggtitle("Tagging costs and number of tags applied")

ggplot(data=tagging, aes(x = Date, y = Total_Tagged_Calc, colour = Tagger)) + geom_point() + ggtitle("Number of fish tagged per tagging event")

# Attempted and failed tagging nights
2018-07-29, Nicole Seamone, Grand Manan, $300
2018-08-26, Lisa Houston, German Bank 2, $200
2018-08-27, Lisa Houston, German Bank 2, $200
2018-09-13, Lisa Houstin, Seal Island 1, $200
2018-09-16, Lisa Houstin, Seal Island 1, $200

# Mapping
CP <- as(extent(-67, -63, 43, 46), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("surveyBoxes.csv")

scots = boxes[which(boxes$Box == "ScotsBay"), ]
german=boxes[which(boxes$Box == "GermanBank"), ]


ggplot(tagging,aes(x=Lon_, y=Lat_)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=scots,aes(x=X, y=Y, group=PID),fill='white',col='black') + coord_map() + labs(x=NULL, y=NULL) + geom_point(aes(size = Total_Tagged_Calc, colour = Tagger)) + ggtitle("Tagging Locations 2018 - Number of Tags")
  
ggplot(tagging,aes(x=Lon_, y=Lat_)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=scots,aes(x=X, y=Y, group=PID),fill='white',col='black') + coord_map() + labs(x=NULL, y=NULL) + geom_point(aes(size = Cost, colour = Tagger)) + ggtitle("Tagging Locations 2018 -  Cost")  


head(tagging)

