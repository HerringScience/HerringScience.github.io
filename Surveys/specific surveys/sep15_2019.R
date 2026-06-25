
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")


# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


# Plan

sixboatsSB = read.csv("sixboatsSB.csv")

Vessels = c("Lady Janice", "Lady Melissa", "Canada 100", "Scotia Gardens", "Brunswick Provider", "Fundy Monarch","Lady Janice", "Lady Melissa", "Canada 100", "Scotia Gardens", "Brunswick Provider", "Fundy Monarch")

sixboatsSB$Vessel  = Vessels
str(sixboatsSB)

sixboatsSB$Vessel = as.factor(sixboatsSB$Vessel)


boxes = read.csv("surveyBoxes.csv")

# Scot's
scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
# CTD
ctd = boxes[which(boxes$Box == "SBocean"), ]


ggplot(sixboatsSB, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + geom_polygon(data=ctd,aes(x=X, y=Y)) + labs(x=NULL, y=NULL)+ scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) 





# Survey analysis

# Sep_15, 2019 Scots Bay
regions = read.table("Region_Sep15_2019.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Sep15_2019.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

loadfunctions( "acousticHerring")

SUA = read.csv("polygon_SB_Aug04_2019.csv")
polySB_main = as.PolySet(SUA, projection="LL")


ggplot(data=polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)



ggplot(trans, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() 

  # Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Sep15_2019"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


area = calcArea(polySB_main) 
area - 1.61


# Results
resultsa = biomassCalc(x = trans_survey, areaKm = 633.6)
unique(resultsa$total_biomass)


        # Run results
        tableA = resultTableA(x = trans_survey)
        tableB = resultTableB(x = trans_survey)
        tableC = resultTableC(x = resultsa)
        
        # Scots
        write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        