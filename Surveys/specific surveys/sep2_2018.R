
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
              track = read.csv("sept2_plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
              
              # Scot's Bay
              ggplot(track,aes(x=X1, y=Y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data = plankton, aes(x=X, y=Y), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)+ coord_map() + labs(x=NULL, y=NULL)


# Survey analysis

# jul 07, 2018 Scots Bay
regions = read.table("Region_jul07_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_jul07_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

loadfunctions( "acousticHerring")

SUA = read.csv("polygon_SB5.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern1.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern1.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

ggplot(data=polySB_eastern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

loadfunctions( "acousticHerring")

name = "Jul 07, 2018 - Scots Bay #3 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern, polyNameC  = polySB_eastern, title = name )

area = calcArea(polySB_main) 
area - 1.61

calcArea(polySB_northern)
calcArea(polySB_eastern)

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Plankton tow

# CTD/plankton position
x2 = c(-65.2472, -65.2349)
y2 = c(45.0581, 45.0534)
type = c(1,1,2)
type = as.factor(type)
plank1 = data.frame(x2,y2)

boxes = read.csv("surveyBoxes.csv")

planktonB=boxes[which(boxes$Box == "PlanktonBox"), ]
planktonB$PID = 1
planktonB$POS = 1:4

ggplot(plank1,aes(x=x2, y=y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+  geom_polygon(data=planktonB,aes(x=X, y=Y, group=PID), colour = "black", fill="grey80",linetype = 3) + geom_point(size = 3, colour="red") + coord_map() + labs(x=NULL, y=NULL) + geom_line(size = 1)





  # Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jul07_2018"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# Results
resultsa = biomassCalc(x = trans_survey2, areaKm = 638.47)
unique(resultsa$total_biomass)


SUA = read.csv("polygonSB_Shalf.csv")
polySB_main = as.PolySet(SUA, projection="LL")
calcArea(polySB_main)

# Plot CTD location



  scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
  scotsBox$PID = 1
  scotsBox$POS = 1:4

  
    
      
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
        
        
        