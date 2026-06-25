

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Aug 11, 2019 German Bank
regions = read.table("Region_Aug11_2019.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Aug11_2019.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.609 )

# Load polygons
# German Bank
SUA = read.csv("polygon_GB1.csv")
polyGB = as.PolySet(SUA, projection="LL")

boxes = read.csv("surveyBoxes.csv")

seal=boxes[which(boxes$Box == "Seal_2019"), ]
sealArea = as.PolySet(seal, projection="LL")


# Modify to original Seal Box
seal[1,4] = -66.21
seal[2,4] = -66.21
seal[3,4] = -66.09
seal[4,4] = -66.09


# Survey Track
ggplot(trans, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")+ geom_polygon(data=seal,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark()

ggplot(data=polyGB, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

calcArea(polyGB) 
calcArea(sealArea) 

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug11_2019"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove seal vessels

ids_seal = c("LB", "TM", "LJ" )
sealBox = trans_survey[which((trans_survey$Vessel %in% ids_seal)), ]

ids_gb = c("FM", "C1", "BP", "MS", "LM", "SL")
gbBox = trans_survey[which((trans_survey$Vessel %in% ids_gb)), ]

resultsa = biomassCalc(x = gbBox, areaKm =859.6)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = sealBox, areaKm =275.12)
unique(resultsb$total_biomass)



# Run results
tableA = resultTableA(x = gbBox)
tableB = resultTableB(x = gbBox)
tableC = resultTableC(x = resultsa)

tableG = resultTableA(x = sealBox)
tableH = resultTableB(x = sealBox)
tableI = resultTableC(x = resultsb)

tableC$Layer = "German Bank"
tableI$Layer = "Seal Island"

A = rbind(tableA, tableG)
B = rbind(tableB, tableH)
C = rbind(tableC, tableI)

# German
write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)






