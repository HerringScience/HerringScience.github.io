

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

# Oct 01, 2018 German Bank
regions = read.table("Region_Oct1_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
mapping = read.table("Map_Oct1_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)
trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

# Load polygons
# German Bank
SUA = read.csv("polygon_GB12.csv")
polyGB = as.PolySet(SUA, projection="LL")

x = surveyTrack(x=trans, polyNameA  = polyGB)

ggplot(data=polyGB, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

calcArea(polyGB) 

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct01_2018"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm =839.2)
unique(resultsa$total_biomass)

map = mapDat(x = mapping)




# plankton tow

# CTD/plankton position
x2 = c(-66.34805, -66.34888333)
y2 = c(43.46365, 43.42971667)

plank1 = data.frame(x2,y2)


SUA = read.csv("polygon_GB1.csv")
polyGB = as.PolySet(SUA, projection="LL")

ggplot(plank1,aes(x=x2, y=y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_point(size = 3, colour = "red") + coord_map() + labs(x=NULL, y=NULL)

+ geom_line(aes(group = type), size = 1)



ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL) +coord_map()


# Run results
tableA = resultTableA(x = trans_survey)
tableB = resultTableB(x = trans_survey)
tableC = resultTableC(x = resultsa)

# German
write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)






