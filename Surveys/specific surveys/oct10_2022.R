
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")

# plankton
plankton=boxes[which(boxes$Box == "PlanktonBox"), ]

## German Bank/Seal Island #5

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -66, 43, 43.5), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("timGrounds.csv")
SUA = read.csv("polygon_GB_14.csv")
polyGB = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SI2021_Aug23.csv")
polySI = as.PolySet(SUA, projection="LL")

# Survey analysis
# JM
regions = read.table("Region_Oct10_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Oct10_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

#GM
regions = read.table("Region_Oct10_2022_GDM.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Oct10_2022_GDM.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


#######################
map = mapDat(x = mapping)

x = regions
trans = transects(x= regions, TS38 = -34.9192468 , TS50 = NA )

SUA = read.csv("polygon_GB_14.csv")
polyGB = as.PolySet(SUA, projection="LL")

unique(trans$Transect_No)

ids = c("T01", "T02", "T03")
trans1 = trans[which((trans$Transect_No %in% ids)), ]

ids = c("T04", "T05", "T06", "T07")
trans2 = trans[which((trans$Transect_No %in% ids)), ]

ids = c("T01", "T02", "T03")
map1 = map[which((map$Transect_No %in% ids)), ]

ids = c("T04", "T05", "T06", "T07")
map2 = map[which((map$Transect_No %in% ids)), ]


x = surveyTrack2(x=trans1, polyNameA  = polyGB, polyNameB  = polySI, title = name )
x = surveyTrack2(x=trans2, polyNameA  = polyGB, polyNameB  = polySI, title = name )


SUA = read.csv("polygon_Oct10.csv")
polySS = as.PolySet(SUA, projection="LL")

x = surveyTrack2(x=trans2, polyNameA  = polySI,polyNameB  = polySS, title = name )

ggplot(map1, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)
ggplot(map2, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct10_2022"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)

SI = trans[which(trans$Transect_No == "T03"), ]

ids = c("T01", "T02")
GB = trans[which((trans$Transect_No %in% ids)), ]

ids = c("T04", "T05", "T06", "T07")
SS = trans[which((trans$Transect_No %in% ids)), ]

calcArea(polySI) 
calcArea(polyGB) 
calcArea(polySS)

# Results
resultsa = biomassCalc(x = GB, areaKm = 805)
unique(resultsa$total_biomass)

resultsa = biomassCalc(x = GB, areaKm = 814)
unique(resultsa$total_biomass)

#22,307



# Results DFO area and TS
resultsa = biomassCalc(x = GB, areaKm = 815)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = SI, areaKm = 252)
unique(resultsb$total_biomass)

#5,243












#Gary's 
#27,973

resultsb = biomassCalc(x = SI, areaKm = 267)
unique(resultsb$total_biomass)

resultsb = biomassCalc(x = SI, areaKm = 252)
unique(resultsb$total_biomass)


#6,350
#5,994

resultsc = biomassCalc(x = SS, areaKm = 22)
unique(resultsc$total_biomass)

#6,891

# Run results

tableA = resultTableA(x = GB)
tableB = resultTableB(x = GB)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = SI)
tableE = resultTableB(x = SI)
tableF = resultTableC(x = resultsb)

tableG = resultTableA(x = SS)
tableH = resultTableB(x = SS)
tableI = resultTableC(x = resultsc)



tableC$Layer = "German Bank"
tableF$Layer = "Seal Island"
tableI$Layer = "School Survey"

A = rbind(tableA,tableD, tableG)
B = rbind(tableB,tableE,tableH)
C = rbind(tableC,tableF, tableI)

# Scots
write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

write.table(tableA, file= "tableA_GDM.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "tableB_GDM.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "tableC_GDM.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

save(tableA,file="tableA_GM_Oct10_2022.Rda")
save(tableB,file="tableB_GM_Oct10_2022.Rda")
save(tableC,file="tableC_GM_Oct10_2022.Rda")




