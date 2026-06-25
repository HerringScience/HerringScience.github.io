
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

SUA = read.csv("polygon_SB5.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern19.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern07.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

#Aug 7, 2022
regions = read.table("Region_Aug07_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Aug07_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions

# TS Standard

trans = transects(x= regions, TS38 = -35.30, TS50 = NA )

x = surveyTrack3(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_eastern, polyNameC  = polySB_northern, title = name )

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

unique(trans$Survey_date)
unique(trans$Vessel)

area = calcArea(polySB_main) 
area - 1.61
calcArea(polySB_northern) 
calcArea(polySB_eastern) 


ids = c("C1","FM", "LB", "LJ", "LM", "SL")

northern = trans[which((trans$Vessel == 'BP')), ]
eastern = trans[which((trans$Vessel == 'MS')), ]
main = trans[which((trans$Vessel %in% ids)), ]


# Results
resultsa = biomassCalc(x = main, areaKm = 640.8)
a = unique(resultsa$total_biomass)
a

resultsb = biomassCalc(x = northern, areaKm = 76.7)
b = unique(resultsb$total_biomass)
b

resultsc = biomassCalc(x = eastern, areaKm = 116)
c = unique(resultsc$total_biomass)
c

# DFO and TS
# Results
resultsa = biomassCalc(x = main, areaKm = 625)
a = unique(resultsa$total_biomass)
a

resultsb = biomassCalc(x = northern, areaKm = 83)
b = unique(resultsb$total_biomass)
b

resultsc = biomassCalc(x = eastern, areaKm = 117)
c = unique(resultsc$total_biomass)
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
        tableF$Layer = "Northern"
        tableI$Layer = "Eastern"
        
        
        A = rbind(tableA,tableD, tableG)
        B = rbind(tableB,tableE, tableH)
        C = rbind(tableC,tableF, tableI)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        