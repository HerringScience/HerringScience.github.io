
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


SUA = read.csv("polygon_SBNorthern3.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

x = surveyTrack(x=trans, polyNameA  = polySB_eastern, polyNameB  = polySB_northern,  title = name )


SUA = read.csv("polygon_SBEastern.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

x = surveyTrack(x=trans, polyNameA  = polySB_eastern, polyNameB  = polySB_northern,  title = name )



# Survey analysis

# jun 14, 2020 Scots Bay
regions = read.table("RegionJun14_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapJun14_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)
trans = transects(x= regions, TS38 = -35.1910 , TS50 = -35.609 )

name = "Jun 13, 2020 - Scots Bay #1 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_eastern, polyNameB  = polySB_northern,  title = name )

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jun14_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# Northern
trans_surveyN=trans_survey[which(trans_survey$Vessel == "LB"), ]

# Eastern
trans_surveyE=trans_survey[which(trans_survey$Vessel == "MS"), ]


calcArea(polySB_northern)
calcArea(polySB_eastern)

# Results
# Northern
resultsa = biomassCalc(x = trans_surveyN, areaKm = 89.426)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = trans_surveyE, areaKm = 119.795)
unique(resultsb$total_biomass)




# Run results
tableA = resultTableA(x = trans_surveyN)
tableB = resultTableB(x = trans_surveyN)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = trans_surveyE)
tableE = resultTableB(x = trans_surveyE)
tableF = resultTableC(x = resultsb)


        tableC$Layer = "Northern"
        tableF$Layer = "Eastern"
        
        A = rbind(tableA,tableD)
        B = rbind(tableB,tableE)
        C = rbind(tableC,tableF)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        