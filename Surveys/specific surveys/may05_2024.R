
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "polygons")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

regions = read.table("Region_May05_2024.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_May05_2024.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

source("mapDat.R")

map = mapDat(x = mapping)
x = regions
head(x)

# TS Standard
# No samples so this remains

source("transects.R")

trans = transects(x= regions, TS38 = -35.5, TS50 = NA )

source("surveyTrack3.R")


SUA = read.csv("polygon_SB05.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern28.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern05.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

x = surveyTrack3(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern,  polyNameC  = polySB_eastern,  title = name )


head(trans)

ids = c("MS_T02","MS_T03", "MS_T04", "MS_T05")
northern = trans[which((trans$RegionName %in% ids)), ]


ids = c("LM_T02","LM_T03")
eastern = trans[which((trans$RegionName %in% ids)), ]

ids =c("MS_T01","LM_T01", "LM_T04", "BP_T01","BP_T02")
main = trans[which((trans$RegionName %in% ids)), ]

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


# Specify the survey you are doing an analysis for  
#trans_survey= trans[which(trans$Survey_date == "May24_2021"), ]

# QC
unique(trans$Survey_date)
unique(trans$Vessel)


area = calcArea(polySB_main) 
area - 1.61

#387.3

calcArea(polySB_northern)
#81.2

calcArea(polySB_eastern)
#73.9


source("biomassCalc.R")

# Results
resultsa = biomassCalc(x = main, areaKm = 387.3)
a = unique(resultsa$total_biomass)

#75,573

resultsb = biomassCalc(x = northern, areaKm = 81.2)
b = unique(resultsb$total_biomass)

#2,636

resultsc = biomassCalc(x = eastern, areaKm = 73.9)
c = unique(resultsc$total_biomass)

#453

a
b
c

a+b+c

#78,662


source("resultTableA.R")
source("resultTableB.R")
source("resultTableC.R")

# Run results
tableA = resultTableA(x = main)
tableB = resultTableB(x = main)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = northern)
tableE = resultTableB(x = northern)
tableF = resultTableC(x = resultsb)

tableG = resultTableA(x = eastern)
tableH = resultTableB(x = eastern)
tableI = resultTableC(x = resultsc)



tableC$Layer = "Main Box"
tableF$Layer = "Northern Box"
tableI$Layer = "Eastern Box"



A = rbind(tableA,tableD, tableG)
B = rbind(tableB,tableE, tableH)
C = rbind(tableC,tableF, tableI)

# Scots
write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

