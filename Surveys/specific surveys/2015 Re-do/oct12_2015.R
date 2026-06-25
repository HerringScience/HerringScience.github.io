
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")


# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Survey analysis
regions = read.table("Region_Oct12_2015.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

regions = read.table("Region_Oct12_Rab.Check.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


# map export wasn't working...suspcious
#mapping = read.table("Map_Aug17_2015.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

# TS used in 2015
trans = transects(x= regions, TS38 = -35.51 , TS50 = -35.319 )
trans

# fix MS EV_Filenames

SUA = read.csv("polygon_GB2.csv")
polyGB = as.PolySet(SUA, projection="LL")

x = surveyTrack(x=trans, polyName  = polyGB )


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct12_2015"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map() 

resultsa = biomassCalc(x = trans_survey, areaKm = 714)
a = unique(resultsa$total_biomass)
a

write.table(trans_survey, file= "results2.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
#resultsb = biomassCalc(x = eastern, areaKm = 121)
#b = unique(resultsb$total_biomass)

#resultsc = biomassCalc(x = northern, areaKm = 79)
#c = unique(resultsc$total_biomass)

a+b+c

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
        