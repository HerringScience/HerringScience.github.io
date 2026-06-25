

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 43.8), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("surveyBoxes.csv")

seal=boxes[which(boxes$Box == "Seal_2018"), ]
fish=boxes[which(boxes$Box == "Seal_Owen"), ]
tow=boxes[which(boxes$Box == "Tow"), ]


german=boxes[which(boxes$Box == "GermanBank"), ]

ggplot(seal, aes(x=X, y=Y), group = PID) + geom_polygon(fill = "red", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black')+ geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='blue',col='black')+ geom_polygon(data=fish,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=tow,aes(x=X, y=Y, group=PID),fill='white',col='black')


# Survey plan
track1 = read.csv("sept16_plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
track = read.csv("sept16B_plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

# Seal Island
ggplot(track,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data = seal, aes(x=X, y=Y), fill = "white", colour = "black")  + geom_polygon(data = german, aes(x=X, y=Y), fill = "white", colour = "black") + coord_map() + labs(x=NULL, y=NULL) + geom_polygon(data = fish, aes(x=X, y=Y), fill = "grey75", colour = "black")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_segment(data=track1, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1.2)




# Sep 16, 2018 Seal island
regions = read.table("Region_Seal1_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
mapping = read.table("Map_Seal1_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)
trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

# Load polygons
# German Bank
SUA = read.csv("polygon_Seal1.csv")
polySI = as.PolySet(SUA, projection="LL")

x = surveyTrack(x=trans, polyNameA  = polySI)

ggplot(data=polySI, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

calcArea(polySI) 

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Sep16_2018"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm =377.94)
unique(resultsa$total_biomass)

map = mapDat(x = mapping)




# plankton tow

# CTD/plankton position
x2 = c(-66.35, -66.35965, -66.3618)
y2 = c(43.55, 43.53333333, 43.5491)
type = c(1,1,2)
type = as.factor(type)
plank1 = data.frame(x2,y2, type)

ggplot(plank1,aes(x=x2, y=y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_point(aes(colour = type), size = 3) + coord_map() + labs(x=NULL, y=NULL) + geom_line(aes(group = type), size = 1)


# PRC

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL) +coord_map()


# Run results
tableA = resultTableA(x = trans_survey)
tableB = resultTableB(x = trans_survey)
tableC = resultTableC(x = resultsa)

# German
write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)






