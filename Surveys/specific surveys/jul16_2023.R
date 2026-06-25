
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

regions = read.table("Region_Jul16_2023.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Jul16_2023.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)

# TS Standard
# No samples so this remains

SUA = read.csv("polygon_SB18.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern19.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern19.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

trans = transects(x= regions, TS38 = -35.5, TS50 = NA )

x = surveyTrack3(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern,  polyNameC  = polySB_eastern,  title = name )

head(trans)

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


ids1 = c("LM_T01", "LM_T02","LM_T03", "LM_T04")
northern = trans[which((trans$RegionName %in% ids1)), ]

ids2 = c("C1_T01","C1_T02", "C1_T03", "C1_T04")
eastern = trans[which((trans$RegionName %in% ids2)), ]

ids3 =c("LB_T01","LB_T02", "SL_T01", "SL_T02", "LJ_T01", "LJ_T02", "FM_T01","FM_T02")
main = trans[which((trans$RegionName %in% ids3)), ]


# Specify the survey you are doing an analysis for  
#trans_survey= trans[which(trans$Survey_date == "May24_2021"), ]

# QC
unique(trans$Survey_date)
unique(trans$Vessel)

area = calcArea(polySB_main) 
area - 1.61

#661

calcArea(polySB_northern)

# 77

calcArea(polySB_eastern)

# 115 

# Results
resultsa = biomassCalc(x = main, areaKm = 661)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 77)
b = unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 115)
c = unique(resultsc$total_biomass)

a
b
c

a+b+c


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

