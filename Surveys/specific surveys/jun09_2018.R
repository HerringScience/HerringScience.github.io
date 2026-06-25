
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

# Survey plan
              track = read.csv("june9_2018Planned.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
              track
              ids = 1:10
              track = track[which((track$id %in% ids)), ]
              
              # Scot's Bay
              ggplot(track,aes(x=X1, y=Y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = vessel), size = 1) + ggtitle("Scots Bay Survey Plan - June 9, 2018") + coord_map() + labs(x=NULL, y=NULL) 


# Survey analysis

# june 9, 2018 Scots Bay
regions = read.table("Region_june09_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_june09_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )


          # testing different TS values
            trans = transects(x= regions, TS38 =-34.8075, TS50 = -35.609 )


          # Specify the survey you are doing an analysis for  
          trans_survey= trans[which(trans$Survey_date == "Jun09_2018"), ]
          resultsa = biomassCalc(x = trans_survey, areaKm = 666.89)
          unique(resultsa$total_biomass)





SUA = read.csv("polygon_SB_jun09.csv")
polySB_main = as.PolySet(SUA, projection="LL")


name = "Jun 09, 2018 - Scots Bay #1 Survey Track"
x = surveyTrack(x=trans, polyName  = polySB_main, title = name )

area = calcArea(polySB_main) 
area - 1.61

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jun09_2018"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm = 666.89)
unique(resultsa$total_biomass)

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

# Plot CTD location

boxes = read.csv("surveyBoxes.csv")

  scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
  scotsBox$PID = 1
  scotsBox$POS = 1:4

  planktonB=boxes[which(boxes$Box == "PlanktonBox"), ]
  planktonB$PID = 1
  planktonB$POS = 1:4
  
    # CTD cast
    x = c(-65.1911,-65.2283, -65.2033)
    y = c(45.0723, 45.065, 45.06667)
    pos = c(1,2,2)
    pos = as.factor(pos)
    plank = data.frame(x,y,pos)
    
    # Plankton tow
        ggplot(plank,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="grey85",linetype = 3)+ geom_polygon(data=planktonB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(aes(color=pos), size=2) + coord_map() + labs(x=NULL, y=NULL) + geom_line(aes(group = pos), size =2)
        