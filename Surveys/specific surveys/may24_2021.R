
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

SUA = read.csv("polygon_SB24.csv")
polySB_main = as.PolySet(SUA, projection="LL")
#ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

SUA = read.csv("polygon_SBNorthern27.csv")
polySB_northern = as.PolySet(SUA, projection="LL")
##polygon_SBNorthern2

#ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

# Survey analysis

# May 24, 2021 Scots Bay
regions = read.table("Region_May24_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_May24_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)

# TS applied from A.D
trans = transects(x= regions, TS38 = -35.52235009, TS50 = NA )



# trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )


SUA = read.csv("polygon_SB24.csv")
#SUA = read.csv("polygon_SB5.csv")

polySB_main = as.PolySet(SUA, projection="LL")
#ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

x = surveyTrack2(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern,  title = name )

head(trans)

ids = c("FM_T03","FM_T04", "FM_T05")
ids2 = c("FM_T01", "FM_T02", "MS_T01", "MS_T02", "MS_T03", "LM_T01", "LM_T02", "LM_T03")



mastersub = trans[which((trans$RegionName %in% ids)), ]


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


# Specify the survey you are doing an analysis for  
#trans_survey= trans[which(trans$Survey_date == "May24_2021"), ]

# QC
unique(trans$Survey_date)
unique(trans$Vessel)

# Northern 
transN=trans[which(trans$RegionName %in% ids), ]

# Main
transM=trans[which(trans$RegionName %in% ids2), ]





area = calcArea(polySB_main) 
area - 1.61

#411.18 is the area for the main



calcArea(polySB_northern)

# 83.27 for northern


# Results
resultsa = biomassCalc(x = transM, areaKm = 411.18)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = transN, areaKm = 83.27)
b = unique(resultsb$total_biomass)


a+b

# Run results
tableA = resultTableA(x = transM)
tableB = resultTableB(x = transM)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = transN)
tableE = resultTableB(x = transN)
tableF = resultTableC(x = resultsb)


        tableC$Layer = "Main Box"
        tableF$Layer = "Northern Box"
        
        A = rbind(tableA,tableD)
        B = rbind(tableB,tableE)
        C = rbind(tableC,tableF)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        