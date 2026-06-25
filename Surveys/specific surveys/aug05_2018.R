
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

# Survey analysis

# August 05, 2018 Scots Bay
regions = read.table("Region_aug05_18.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_aug05_18.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )
# exclude LJ and FM

trans1 = trans [which(trans$Vessel != "FM"), ]
trans2 = trans1 [which(trans1$Vessel != "LJ"), ]

northern = trans [which(trans$Vessel == "LJ"), ]
eastern = trans [which(trans$Vessel == "FM"), ]




ggplot(data=polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

loadfunctions( "acousticHerring")


SUA = read.csv("polygon_SB7.csv")
polySB_main = as.PolySet(SUA, projection="LL")


SUA = read.csv("polygon_SBEastern1.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


SUA = read.csv("polygon_SBNorthern2.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

name = "Aug 5, 2018 - Scots Bay #5 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_main, polyNameB = polySB_eastern, polyNameC = polySB_northern, title = name )

ggplot(data=polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))



area = calcArea(polySB_main) 
area - 1.61
calcArea(polySB_eastern)
calcArea(polySB_northern)

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

  # Specify the survey you are doing an analysis for  
trans_survey= trans2[which(trans2$Survey_date == "Aug05_2018"), ]

eastern
northern

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# manipulate trans_survey to edit sealife II data - add in values for Dist_T for SL T01 and SL T02
# trans_survey[11,24] = 42.9664
# trans_survey[12,24] = 37.04

head(trans_survey)

# Results
resultsa = biomassCalc(x = trans_survey, areaKm = 647.73)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = eastern, areaKm = 113.06)
unique(resultsb$total_biomass)

resultsc = biomassCalc(x = northern, areaKm = 81.51)
unique(resultsc$total_biomass)

# Plot CTD location

boxes = read.csv("surveyBoxes.csv")

  scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
  scotsBox$PID = 1
  scotsBox$POS = 1:4

  planktonB=boxes[which(boxes$Box == "PlanktonBox"), ]
  planktonB$PID = 1
  planktonB$POS = 1:4
  
    # Points Plankton/CTD
    x = c(-64.7636, -64.7469, -65.2167, -65.2104)
    y = c(45.21034, 45.218, 45.05, 45.0562)
    pos = c(1,1,2,2)
    pos = as.factor(pos)
    plank = data.frame(x,y, pos)
    
        # Plankton tow    
        ggplot(plank,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="grey85",linetype = 3)+ geom_polygon(data=planktonB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(aes(colour = pos),size=2) + coord_map() + labs(x=NULL, y=NULL) + geom_line(aes(group = pos), size =1)
        
        # Run results
        tableA = resultTableA(x = trans_survey)
        tableB = resultTableB(x = trans_survey)
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
        
        
        