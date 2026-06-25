
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")

# plankton
plankton=boxes[which(boxes$Box == "PlanktonBox"), ]


# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


SUA = read.csv("polygon_SBEastern7.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

# 6
SUA = read.csv("polygon_SB9.csv")
polySB_main = as.PolySet(SUA, projection="LL")

ggplot(data=polySB_eastern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))




# Survey analysis

# july 6, 2019 Scots Bay
regions = read.table("Region_jul06_2019REDO.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_jul06_2019REDO.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

name = "Jul 6, 2019 - Scots Bay #2 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_eastern, title = name )


head(trans)
unique(trans$Region_name)

# remove by Region_name

# northern box not surveyed
# eastern box composed of T03 and T04

# eastern box
ids_eastern =c("T03", "T04")

#main survey box
ids_main = c("T01", "T02")


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jul06_2019"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)


head(trans)

# eastern box
eastern = trans_survey[which((trans_survey$Transect_No %in% ids_eastern)), ]

# main survey box
main = trans_survey[which((trans_survey$Transect_No %in% ids_main)), ]

area = calcArea(polySB_main) 
area - 1.61

calcArea(polySB_eastern)

# Results
resultsa = biomassCalc(x = main, areaKm = 643.6)
unique(resultsa$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 137)
unique(resultsc$total_biomass)



        # Run results
        tableA = resultTableA(x = main)
        tableB = resultTableB(x = main)
        tableC = resultTableC(x = resultsa)
        
        tableG = resultTableA(x = eastern)
        tableH = resultTableB(x = eastern)
        tableI = resultTableC(x = resultsc)
        
        tableC$Layer = "Main Box"
        tableI$Layer = "Eastern Box"
        
        A = rbind(tableA, tableG)
        B = rbind(tableB, tableH)
        C = rbind(tableC, tableI)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        