

# Nov 6, 2020

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

regions = read.table("RegionNov06_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Map
mapping = read.table("MapNov06_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

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


SUA = read.csv("polygon_Nov.csv")
polyNew = as.PolySet(SUA, projection="LL")
calcArea(polyNew)
#26


name = "November 6, 2020 - German Bank and Seal Island Survey Track"
x = surveyTrack2(x=trans, polyNameA  = polyGB, polyNameB  = polyNew,title = name )

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

head(map)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Nov06_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm =26)
unique(resultsa$total_biomass)


# Run results
tableA = resultTableA(x = trans_survey)
tableB = resultTableB(x = trans_survey)
tableC = resultTableC(x = resultsa)


tableC$Layer = "German Bank"

# German
write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)






