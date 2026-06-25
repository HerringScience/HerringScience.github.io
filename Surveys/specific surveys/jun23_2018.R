
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
              track = read.csv("june23_plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
              
              # Scot's Bay
              ggplot(track,aes(x=X1, y=Y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data = plankton, aes(x=X, y=Y), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Scots Bay Survey Plan - June 23, 2018") + coord_map() + labs(x=NULL, y=NULL)


# Survey analysis

# june 23, 2018 Scots Bay
regions = read.table("Region_June23_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_June23_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

SUA = read.csv("polygon_SB_jun23.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SB_N_June23.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

name = "Jun 23, 2018 - Scots Bay #2 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_main, title = name, polyNameB = polySB_northern)

area = calcArea(polySB_main) 
area - 1.61
calcArea(polySB_northern) 

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jun23_2018"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove the Brunswick Provider data
trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]
northern=trans_survey[which(trans_survey$Vessel == "BP"), ]

resultsa = biomassCalc(x = trans_survey1, areaKm = 679.03)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 92.05)
unique(resultsb$total_biomass)

# Run results
tableA = resultTableA(x = trans_survey1)
tableB = resultTableB(x = trans_survey1)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = northern)
tableE = resultTableB(x = northern)
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


# Plot CTD location

boxes = read.csv("surveyBoxes.csv")

  scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
  scotsBox$PID = 1
  scotsBox$POS = 1:4

  planktonB=boxes[which(boxes$Box == "PlanktonBox"), ]
  planktonB$PID = 1
  planktonB$POS = 1:4
  
    # CTD cast
  	
    x = c(-65.253,-65.2317)
    y = c(45.0526, 45.0536)
    #pos = c(1,2,2)
    #pos = as.factor(pos)
  #  plank = data.frame(x,y,pos)
    plank = data.frame(x,y)
    
    # Plankton tow
       # ggplot(plank,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="grey85",linetype = 3)+ geom_polygon(data=planktonB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(aes(color=pos), size=2) + coord_map() + labs(x=NULL, y=NULL) + geom_line(aes(group = pos), size =2)
    
    # Plankton tow only    
    ggplot(plank,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="grey85",linetype = 3)+ geom_polygon(data=planktonB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(size=2, col = "red") + coord_map() + labs(x=NULL, y=NULL) + geom_line(col = "red", size =2)  
    