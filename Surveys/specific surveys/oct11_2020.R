

# October 11, 2020

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")


## German Bank/Seal Island #6

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 43.8), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("timGrounds.csv")


# Load data
# Survey plan

regions = read.table("Region_Oct11_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Map
mapping = read.table("Map_Oct11_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

# Results from German Bank/Seal Island October 8, 2019
# Need to figure out what the 50khz was...

trans = transects(x= regions, TS38 =-34.8075 , TS50 = -34.9147)
head(boxes)


trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )


# Load polygons
# German Bank
SUA = read.csv("polygon_GB16.csv")
polyGB = as.PolySet(SUA, projection="LL")
calcArea(polyGB) 
  #853.5

SUA = read.csv("polygon_SI18.csv")
polySI = as.PolySet(SUA, projection="LL")
calcArea(polySI)
  #357.2



name = "October 11, 2020 - German Bank and Seal Island Survey Track"
x = surveyTrack2(x=trans, polyNameA  = polyGB, polyNameB  = polySI,title = name )

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

head(map)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct11_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove seal vessels

ids_seal = c("T03" )
sealBox = trans_survey[which((trans_survey$Transect_No == ids_seal)), ]

ids_gb = c("T01", "T02")
gbBox = trans_survey[which((trans_survey$Transect_No %in% ids_gb)), ]

#gbBox[3, ] = NA
#gbBox = na.omit(gbBox)




resultsa = biomassCalc(x = gbBox, areaKm =853.5)
unique(resultsa$total_biomass)
# 10,70.91 mt

resultsb = biomassCalc(x = sealBox, areaKm =357.2)
unique(resultsb$total_biomass)
# 577 mt


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






