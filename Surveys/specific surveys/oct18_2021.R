
# Oct 3 GM and SI plus a ad hoc on Oct 5

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")

# plankton
plankton=boxes[which(boxes$Box == "PlanktonBox"), ]

## German Bank/Seal Island #6

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 43.6), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("timGrounds.csv")


# tagging figure:

	



	
	
	


x =c(-66.33083333, -66.38633333, -66.38568333, -66.37368333, -66.33133333, -66.352,	-66.352,	-66.35,	-66.35) 
y =c(43.29266667, 43.295, 43.304, 43.26146667, 43.21255, 43.57, 43.56, 43.56, 43.57) 
no_tags = c(83, 117, 150, 150, 150, 100, 100, 100, 100)
vessel = c("LM", "LM","LJ","LJ","LJ", "LM", "LM","LM","LM") 
type= c("Tagging", "Tagging", "Tagging", "Tagging", "Tagging", "Tow","Tow","Tow","Tow" )
sum(no_tags)

df = data.frame(x,y,no_tags,vessel)

ggplot(df, aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID), fill = "yellow", colour = "black") +geom_polygon(data=polySI,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")+ geom_point(aes(colour = vessel, size = no_tags, shape = type))  + labs(x=NULL, y=NULL) + coord_map() + theme_dark()




# Load polygons
# German Bank
SUA = read.csv("polygon_GB.csv")

SUA1=SUA[which(SUA$Type == "Management"), ]
polyGB = as.PolySet(SUA1, projection="LL")

SUA2=SUA[which(SUA$Type == "Lines"), ]
polyGB_Ran = as.PolySet(SUA2, projection="LL")


SUA = read.csv("polygon_SI2021_Aug23.csv")
polySI = as.PolySet(SUA, projection="LL")

loadfunctions( "acousticHerring")

# Survey analysis

# October 18, 2021 German Bank
regions = read.table("Region_Oct18_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Oct18_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions

trans = transects(x= regions, TS38 = -35.5 , TS50 = NA )

SUA = read.csv("polygon_SI2021_Aug23.csv")
polySI = as.PolySet(SUA, projection="LL")

x = surveyTrack2(x=trans, polyNameA  = polyGB_Ran,polyNameB  = polySI, title = name )


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct18_2021"), ]
trans_survey= trans[which(trans$Survey_date == "Oct18_2021"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)

# remove T03 for GB
trans_surveyGB=trans_survey[which(trans_survey$Transect_No != "T03"), ]

# Only T03 for SI
trans_surveySI=trans_survey[which(trans_survey$Transect_No == "T03"), ]




calcArea(polyGB_Ran) 
#792.54

calcArea(polySI)
#267

# Results
resultsa = biomassCalc(x = trans_surveyGB, areaKm = 792.54)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = trans_surveySI, areaKm = 267)
unique(resultsa$total_biomass)


# Run results
tableA = resultTableA(x = trans_surveyGB)
tableB = resultTableB(x = trans_surveyGB)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = trans_surveySI)
tableE = resultTableB(x = trans_surveySI)
tableF = resultTableC(x = resultsb)

tableC$Layer = "German Bank"
tableF$Layer = "Seal Island"

A = rbind(tableA, tableD)
B = rbind(tableB, tableE)
C = rbind(tableC, tableF)


write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)



        