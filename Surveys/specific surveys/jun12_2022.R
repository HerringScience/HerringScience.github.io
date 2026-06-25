
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


#ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

SUA = read.csv("polygon_SBNorthern27.csv")
polySB_northern = as.PolySet(SUA, projection="LL")


SUA = read.csv("polygon_SBEastern.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


##polygon_SBNorthern2

#ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

# Survey analysis

# June 12, 2022 Scots Bay
#regions = read.table("Region_Jun12_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

regions = read.table("Region_Jun12_2022_2.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

#mapping = read.table("Map_Jun12_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Jun12_2022_2.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)

# TS Standard

trans = transects(x= regions, TS38 = -35.15, TS50 = NA )




SUA = read.csv("polygon_SBNorthern19.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern19.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


x = surveyTrack3(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern,  polyNameC  = polySB_eastern,  title = name )
head(trans)

ids = c("LM","LB", "MS", "SL", "C1", "BP")

northern = trans[which((trans$Vessel == 'FM')), ]
eastern = trans[which((trans$Vessel == 'LJ')), ]
main = trans[which((trans$Vessel %in% ids)), ]


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

# 77 for northern
#115 for eastern

# DFO Areas: 618.2
#eastern: 129.7
#nothern: 84.3

# Results
resultsa = biomassCalc(x = main, areaKm = 640.8)
a = unique(resultsa$total_biomass)

#35,760

resultsa = biomassCalc(x = main, areaKm = 618)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 77)
b = unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 115)
c = unique(resultsc$total_biomass)

a+b+c
#104,368



# DFO Area Results
resultsa = biomassCalc(x = main, areaKm = 617)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 84)
b = unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 111)
c = unique(resultsc$total_biomass)

a+b+c
#93,163
  

#DFO
resultsa = biomassCalc(x = main, areaKm = 619.4)
a = unique(resultsa$total_biomass)
a

resultsb = biomassCalc(x = northern, areaKm = 83.43)
b = unique(resultsb$total_biomass)
b

resultsc = biomassCalc(x = eastern, areaKm = 129.4)
c = unique(resultsc$total_biomass)
c

a+b+c


#111,299.1


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
        

        