
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


# Survey plan
track = read.csv("aug18_plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

# Scot's Bay
ggplot(track,aes(x=X1, y=Y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data = plankton, aes(x=X, y=Y), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)+ coord_map() + labs(x=NULL, y=NULL)


# Survey analysis

# September 02, 2018 Scots Bay
regions = read.table("Region_sep02_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_sep02_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

SUA = read.csv("polygon_SB9.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern4.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern2.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

x = surveyTrack(x=trans, polyNameA  = polySB_main, polyNameB = polySB_northern, polyNameC = polySB_eastern)


ggplot(data=polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))



calcArea(polySB_northern) 

area - 1.61

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) +coord_map()

  # Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Sep02_2018"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove SL and MS
trans_survey1=trans_survey[which(trans_survey$Vessel != "SL"), ]
trans_survey2=trans_survey1[which(trans_survey1$Vessel != "MS"), ]

# Northern
northern=trans_survey[which(trans_survey$Vessel == "MS"), ]

# eastern
eastern=trans_survey[which(trans_survey$Vessel == "SL"), ]

# Results
resultsa = biomassCalc(x = trans_survey2, areaKm = 630.28)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 77.81)
unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 108.36)
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
  x = c(-65.2544, -65.2424, -64.9832, -64.8774, -64.8643)
  y = c(45.0375, 45.0345, 45.178, 45.2132, 45.227)
  pos = c(1,1,2,2,2)
  pos = as.factor(pos)
  plank = data.frame(x,y, pos)
    
        # Plankton tow    
        ggplot(plank,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="grey85",linetype = 3)+ geom_polygon(data=planktonB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(aes(colour = pos), size=2) + coord_map() + labs(x=NULL, y=NULL)
        
        # Run results
        tableA = resultTableA(x = trans_survey2)
        tableB = resultTableB(x = trans_survey2)
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
        
        
        