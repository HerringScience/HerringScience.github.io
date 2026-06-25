
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


SUA = read.csv("polygon_SBNorthern17.csv")
polySB_northern = as.PolySet(SUA, projection="LL")


SUA = read.csv("polygon_SB17.csv")
polySB_main = as.PolySet(SUA, projection="LL")


x = surveyTrack(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern,  title = name )

ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

# Survey analysis

# jun 2, 2020 Scots Bay
regions = read.table("RegionJun02_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapJun02_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)
# DFO calculated TS for 38
trans = transects(x= regions, TS38 = -35.25417057, TS50 = -35.609 )


trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )



name = "Jun 02, 2020 - Scots Bay #1 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern,  title = name )

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jun02_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect)

ids=c("T04", "T05")


# Northern Box
trans_survey1=trans_survey[which(trans_survey$Transect != "T04"), ]
trans_survey2=trans_survey1[which(trans_survey1$Transect != "T05"), ]

# Main
trans_survey3 = subset(trans_survey, (trans_survey$Transect_No %in% ids))

area = calcArea(polySB_main) 
area - 1.61

calcArea(polySB_northern)

# Results
resultsa = biomassCalc(x = trans_survey3, areaKm = 91.972)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = trans_survey2, areaKm = 60.147)
b = unique(resultsb$total_biomass)


a+b

# Run results
tableA = resultTableA(x = trans_survey2)
tableB = resultTableB(x = trans_survey2)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = trans_survey3)
tableE = resultTableB(x = trans_survey3)
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
        
        
        