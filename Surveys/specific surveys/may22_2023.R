
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


SUA = read.csv("polygon_SBNorthern27.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern20.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

regions = read.table("Region_May22_2023.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_May22_2023.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)

# TS Standard
# No samples so this remains

SUA = read.csv("polygon_SB22.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern28.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern20.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


trans = transects(x= regions, TS38 = -35.5, TS50 = NA )



x = surveyTrack3(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern,  polyNameC  = polySB_eastern,  title = name )S

head(trans)

ids = c("FM_T02","FM_T03", "FM_T04")
northern = trans[which((trans$RegionName %in% ids)), ]


ids = c("MS_T02","MS_T03", "MS_T04", "MS_T05")
eastern = trans[which((trans$RegionName %in% ids)), ]

ids =c("MS_T02","MS_T03", "MS_T04", "MS_T05","FM_T02","FM_T03", "FM_T04")


ids =c("FM_T02","FM_T03", "FM_T04")

main = trans[which((trans$RegionName != ids)), ]



main_ = main[which((main$RegionName != ids)), ]


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


# Specify the survey you are doing an analysis for  
#trans_survey= trans[which(trans$Survey_date == "May24_2021"), ]

# QC
unique(trans$Survey_date)
unique(trans$Vessel)


area = calcArea(polySB_main) 
area - 1.61

#640.8


calcArea(polySB_northern)
calcArea(polySB_eastern)

# 83.3 for northern


# Results
resultsa = biomassCalc(x = main_, areaKm = 664)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 81)
b = unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 127)
c = unique(resultsc$total_biomass)

a
b
c

a+b+c


# Run results
tableA = resultTableA(x = main_)
tableB = resultTableB(x = main_)
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

