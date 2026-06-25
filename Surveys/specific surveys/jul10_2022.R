
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

#June 26 2022
regions = read.table("Region_Jul10_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Jul10_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)

# TS Standard

trans = transects(x= regions, TS38 = -35.09, TS50 = NA )

SUA = read.csv("polygon_SBEastern19.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

## Need to shorten the C1_T04 and C1_T01

x = surveyTrack2(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_eastern,  title = name )

head(trans)

ids = c("LJ","SL", "C1", "BP")

eastern = trans[which((trans$Vessel == 'LB')), ]
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

calcArea(polySB_eastern)


# Results
resultsa = biomassCalc(x = main, areaKm = 640.8)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = eastern, areaKm = 115)
b = unique(resultsb$total_biomass)

a+b

#10,537

# DFO Area and TS Results
resultsa = biomassCalc(x = main, areaKm = 661)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = eastern, areaKm = 116)
b = unique(resultsb$total_biomass)

a+b

#9,886

# Run results
tableA = resultTableA(x = main)
tableB = resultTableB(x = main)
tableC = resultTableC(x = resultsa)

tableG = resultTableA(x = eastern)
tableH = resultTableB(x = eastern)
tableI = resultTableC(x = resultsb)



        tableC$Layer = "Main Box"
        tableI$Layer = "Eastern Box"
        
        
        
        A = rbind(tableA,tableG)
        B = rbind(tableB,tableH)
        C = rbind(tableC,tableI)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        