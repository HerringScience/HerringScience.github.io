
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

# GB #1
regions = read.table("Region_Aug14_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

#regions = read.table("Region_Aug14_2022_.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
#regions = read.table("Region_Aug14_2022_2.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Aug14_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

#mapping = read.table("Map_Aug14_2022__.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
#mapping = read.table("Map_Aug14_2022_2.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)
x = regions
trans = transects(x= regions, TS38 = -35.46135631, TS50 = NA )

SUA = read.csv("polygon_GB_14.csv")
polyGB = as.PolySet(SUA, projection="LL")

x = surveyTrack2(x=trans, polyNameA  = polyGB, polyNameB  = polySI, title = name )

head(trans)
ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug14_2022"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)

SI = trans[which(trans$Transect_No == "T03"), ]
calcArea(polySI) 
# SI = 267

ids = c("T01", "T02")

GB = trans[which((trans$Transect_No %in% ids)), ]

calcArea(polySI) 
# SI = 267

calcArea(polyGB) 
# GB = 805


# Results
resultsa = biomassCalc(x = GB, areaKm = 805)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = SI, areaKm = 267)
unique(resultsb$total_biomass)

#438

# Results DFO Area and TS
resultsa = biomassCalc(x = GB, areaKm = 799)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = SI, areaKm = 269)
unique(resultsb$total_biomass)



# Run results

tableA = resultTableA(x = GB)
tableB = resultTableB(x = GB)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = SI)
tableE = resultTableB(x = SI)
tableF = resultTableC(x = resultsb)

tableC$Layer = "German Bank"
tableF$Layer = "Seal Island"

A = rbind(tableA,tableD)
B = rbind(tableB,tableE)
C = rbind(tableC,tableF)

write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)





