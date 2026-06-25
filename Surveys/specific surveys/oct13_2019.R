



# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")


## Seal Island  #5

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 43.8), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("surveyBoxes.csv")

seal=boxes[which(boxes$Box == "Seal_2019"), ]
CTD=boxes[which(boxes$Box == "GBocean"), ]
german=boxes[which(boxes$Box == "GermanBank"), ]
tow = boxes[which(boxes$Box == "Tow"), ]

boxes = read.csv("surveyBoxes.csv")

SL = boxes[which(boxes$Box == "SL_2019"), ]

# Survey Track
ggplot(trans_, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_polygon(data=SL,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark()

# Load data
# Survey plan


nineboatsGB = read.csv("nineboatsGB.csv")
nineboatsGB$id = c(1:18)

Yi = 43.6
nineboatsGB[10,2 ] = Yi
nineboatsGB[11,2 ] = Yi
nineboatsGB[12,2 ] = Yi
nineboatsGB[13,2 ]= Yi
nineboatsGB[14,2 ]= Yi
nineboatsGB[15,2 ]= Yi
nineboatsGB[16,2 ]= Yi
nineboatsGB[17,2 ]= Yi
nineboatsGB[18,2 ]= Yi

nineboatsGB$Vessel = c("LM", "TM", "SL", "MS", "LB", "BP", "LJ", "C1", "FM", "LM", "TM", "SL", "MS", "LB", "BP", "LJ", "C1", "FM")

nineboatsGB$Vessel = as.factor(nineboatsGB$Vessel)
trackSI$Vessel = c("LM", "TM", "SL", "MS", "LB", "BP", "LJ", "C1", "FM")

ggplot(nineboatsGB, aes(x=X, y=Y)) + geom_polygon(data = out, aes(x=long, y=lat, group=group))+ geom_polygon(data=CTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ coord_map() + labs(x=NULL, y=NULL) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + geom_segment(data = trackSI, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)








# Oct 13 Seal Island
regions = read.table("Region_Oct13_2019.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Oct13_2019.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)
head(map)

trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.609 )

# Load polygons
# German Bank
SUA = read.csv("polygon_GB1.csv")
polyGB = as.PolySet(SUA, projection="LL")


sealArea = as.PolySet(seal, projection="LL")


# Modify to original Seal Box
seal[1,4] = -66.229
seal[2,4] = -66.229
seal[3,4] = -66.075
seal[4,4] = -66.075

seal[1,5] = 43.5045
seal[4,5] = 43.5045

polyGB
polyGB[4,2] = 43.605
polyGB[5, ] = c(5, 43.605, -66.363,1,5)
polyGB[6, ] = c(6, 43.571, -66.363,1,6)

# reduce east border-make it the same as the seal west border - not overlapping
polyGB[1,3] = -66.229
polyGB[2,3] = -66.229


ggplot(data=seal, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

#bring 41 down, add 45 and 46 same X different Y. 41 and 45 have the same Y
seal[5,] = c(45, 11, 5, -66.20, 43.5045, "Seal_2019")
seal[6,] = c(46, 11, 6, -66.20, 43.36, "Seal_2019")
seal[1,5] = 43.36

seal$X = as.numeric(seal$X)
seal$Y = as.numeric(seal$Y)

# remove SL_T01 and SL_T03


ids = c("T03", "T02")
trans_ = trans[which((trans$Transect_No %in% ids)), ]
map_ = map[which((map$Transect_No %in% ids)), ]

trans = trans[which((trans$Transect_No == "T01")), ]
map = map[which((map$Transect_No == "T01")), ]

# Survey Track
ggplot(trans_, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_polygon(data=SL,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark()




 calcArea(polyGB) 
sealArea = as.PolySet(seal, projection="LL")
calcArea(sealArea) 

SLArea = as.PolySet(SL, projection="LL")
calcArea(SLArea) 


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)
ggplot(map_, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

head(map)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct13_2019"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


resultsa = biomassCalc(x = trans_survey, areaKm =305.77)
unique(resultsa$total_biomass)
# 34,263 mt

resultsb = biomassCalc(x = trans_ , areaKm =53.7)
unique(resultsb$total_biomass)
# 23,555 mt


# Run results
tableA = resultTableA(x = trans_survey)
tableB = resultTableB(x = trans_survey)
tableC = resultTableC(x = resultsa)

tableG = resultTableA(x = trans_)
tableH = resultTableB(x = trans_)
tableI = resultTableC(x = resultsb)

tableC$Layer = "Seal Island"
tableI$Layer = "Opportunistic Survey"

A = rbind(tableA, tableG)
B = rbind(tableB, tableH)
C = rbind(tableC, tableI)

# German
write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)






