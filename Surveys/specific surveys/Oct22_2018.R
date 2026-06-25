

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
SI_2003=boxes[which(boxes$Box == "SI_2003"), ]



ggplot(seal, aes(x=X, y=Y), group = PID) + geom_polygon(fill = "red", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black')+ geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='blue',col='black')+ geom_polygon(data=fish,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=tow,aes(x=X, y=Y, group=PID),fill='white',col='black')


# Survey plan
track1 = read.csv("Oct21_plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
track2 = read.csv("Oct21_plan_.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)


# Tracks - Seal Island 2003
ggplot(track1,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=german,aes(x=X, y=Y, group=PID),fill='white',col='black') + coord_map() + labs(x=NULL, y=NULL)+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_segment(data=track1, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1.2) + geom_segment(data=track2, aes(x=X, y=Y, xend = Xend, yend = Yend, colour = Vessel), size = 1.2) + geom_polygon(data=SI_2003,aes(x=X, y=Y, group=PID),fill='red',col='black')

x1 = c(-65.48, -65.46)
y1 = c(43.08, 43.07)

# Oct 22, 2018
regions = read.table("Region_Oct22_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
mapping = read.table("Map_Oct22_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)
trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

# Load polygons
# German Bank


SUA = read.csv("polygon_GB8.csv")
polyGB = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_Seal.csv")
polySI = as.PolySet(SUA, projection="LL")

x = surveyTrack(x=trans, polyNameA  = polySI, polyNameB = polyGB)

ggplot(data=polySI, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

calcArea(polySI) 

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct22_2018"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

trans_survey1 <- subset(trans_survey, ( X < -66.23) )
dim(trans_survey1)

trans_survey2 <- subset(trans_survey, ( X > -66.23) )
dim(trans_survey2)
dim(trans_survey)

resultsa = biomassCalc(x = trans_survey1, areaKm =612.22)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = trans_survey2, areaKm =260.93)
unique(resultsb$total_biomass)


# PRC

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL) +coord_map()


# Run results
tableA = resultTableA(x = trans_survey1)
tableB = resultTableB(x = trans_survey1)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = trans_survey2)
tableE = resultTableB(x = trans_survey2)
tableF = resultTableC(x = resultsb)

tableC$Layer = "German Bank Box"
tableF$Layer = "Seal Island"

A = rbind(tableA,tableD)
B = rbind(tableB,tableE)
C = rbind(tableC,tableF)

write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)




