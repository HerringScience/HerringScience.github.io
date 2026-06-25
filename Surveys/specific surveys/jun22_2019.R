
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

# look closely at the eastern box
CP <- as(extent(-64.8, -64.5, 45.2, 45.4), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


SUA = read.csv("polygon_SB5.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern1.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern5.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")

# make this bigger
# ggplot(data=polySB_eastern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))


#3	45.332	-64.554
#4	45.294	-64.69
#5	45.309	-64.708


# Survey analysis

# jun 22, 2019 Scots Bay
regions = read.table("Region_Jun22_2019REDO.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Jun22_2019REDO.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )


name = "Jun 22, 2019 - Scots Bay #2 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern, 
                polyNameC  = polySB_eastern, title = name )

# remove by Region_name

# eastern box
ids =c("Jun22_2019BP_T01", "Jun22_2019BP_T02", "Jun22_2019FM_T03", "Jun22_2019FM_T04", "Jun22_2019C1_T02", "Jun22_2019C1_T03", "Jun22_2019C1_T04", "Jun22_2019C1_T05", "Jun22_2019LJ_T03", "Jun22_2019LJ_T04", "Jun22_2019SL_T03" )

#main survey box
transects = c("Jun22_2019MS_T01", "Jun22_2019MS_T02", "Jun22_2019LB_T01", "Jun22_2019LB_T02", "Jun22_2019SL_T01", "Jun22_2019SL_T02", "Jun22_2019FM_T01", "Jun22_2019FM_T02", "Jun22_2019LJ_T01",  "Jun22_2019LJ_T02","Jun22_2019C1_T01", "Jun22_2019BP_T03")


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jun22_2019"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove northern box
trans_survey1=trans_survey[which(trans_survey$Vessel != "LM"), ]

# eastern box
eastern = trans_survey1[which((trans_survey1$Region_name %in% ids)), ]

# main survey box
main = trans_survey1[which((trans_survey1$Region_name %in% transects)), ]

# Northern
northern=trans_survey[which(trans_survey$Vessel == "LM"), ]


area = calcArea(polySB_main) 
area - 1.61

      calcArea(polySB_northern)
      # 80.58
      
      calcArea(polySB_eastern)
      # 127.05

# Results
resultsa = biomassCalc(x = main, areaKm = 638.47)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 80.58)
unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 127.05)
unique(resultsc$total_biomass)



# Plot CTD location

boxes = read.csv("surveyBoxes.csv")

  scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
  scotsBox$PID = 1
  scotsBox$POS = 1:4

  planktonB=boxes[which(boxes$Box == "PlanktonBox"), ]
  planktonB$PID = 1
  planktonB$POS = 1:4
  
    # CTD cast
    x = c(-65.1942, -65.0129)
    y = c(45.0867, 45.1379)
    plank = data.frame(x,y)
    
    # Plankton tows
    x = c(-65.2188,-65.2, -65.2685, -65.2492, -65.0079, -65.0129)
    y = c(45.0768, 45.08433, 45.0746, 45.0789, 45.1388, 45.1379)
    pos = c(1,1,2,2,3,3)
    pos = as.factor(pos)
    plank = data.frame(x,y,pos)
    
    # CTD casts
        ggplot(plank,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="grey85",linetype = 3)+ geom_polygon(data=planktonB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(size=2, col = "red") + coord_map() + labs(x=NULL, y=NULL) 
    
        # Plankton tow    
        ggplot(plank,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="grey85",linetype = 3)+ geom_polygon(data=planktonB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(aes(colour = pos),size=2) + coord_map() + labs(x=NULL, y=NULL) + geom_line(aes(group = pos), size =1)
        
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
        
        
        