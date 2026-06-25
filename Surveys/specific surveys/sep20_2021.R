
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")

# plankton
plankton=boxes[which(boxes$Box == "PlanktonBox"), ]

## German Bank/Seal Island #1

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 43.6), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("timGrounds.csv")


# Load polygons
# German Bank
SUA = read.csv("polygon_GB_sept20.csv")
SUA = as.PolySet(SUA, projection="LL")


SUA = read.csv("polygon_SI2021_Oct03.csv")
polySI = as.PolySet(SUA, projection="LL")

loadfunctions( "acousticHerring")



# Survey analysis

# September 20, 2021 German Banks
regions = read.table("Region_Sep20_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Sep20_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions

trans = transects(x= regions, TS38 = -35.5 , TS50 = NA )

SUA = read.csv("polygon_GB_sept20.csv")
polyGB_ = as.PolySet(SUA, projection="LL")

x = surveyTrack2(x=trans, polyNameA  = polyGB_,polyNameB  = polySI, title = name )

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Sep20_2021"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)

# remove T03 for GB
trans_surveyGB=trans_survey[which(trans_survey$Transect_No != "T03"), ]

# Only T03 for SI
trans_surveySI=trans_survey[which(trans_survey$Transect_No == "T03"), ]


calcArea(polyGB_) 
#801.6
calcArea(polySI)
# 264.7

# Results
resultsa = biomassCalc(x = trans_surveyGB, areaKm = 801.6)
unique(resultsa$total_biomass)

#80,644

resultsb = biomassCalc(x = trans_surveySI, areaKm = 264.7)
unique(resultsb$total_biomass)

#567.9


# Run results
tableA = resultTableA(x = trans_surveyGB)
tableB = resultTableB(x = trans_surveyGB)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = trans_surveySI)
tableE = resultTableB(x = trans_surveySI)
tableF = resultTableC(x = resultsb)

tableC$Layer = "German Bank"
tableF$Layer = "Seal Island"

A = rbind(tableA, tableD)
B = rbind(tableB, tableE)
C = rbind(tableC, tableF)


write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)



        