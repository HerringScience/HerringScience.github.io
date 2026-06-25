
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

regions = read.table("Region_Aug27_2023.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Aug27_2023.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)

SUA = read.csv("polygon_SB18.csv")
polySB_main = as.PolySet(SUA, projection="LL")

trans = transects(x= regions, TS38 = -35.5, TS50 = NA )

x = surveyTrack(x=trans, polyNameA  = polySB_main,title = name )

head(trans)

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


# Specify the survey you are doing an analysis for  
#trans_survey= trans[which(trans$Survey_date == "May24_2021"), ]

# QC
unique(trans$Survey_date)
unique(trans$Vessel)

area = calcArea(polySB_main) 
area - 1.61

#661

# Results
resultsa = biomassCalc(x = trans, areaKm = 661)
a = unique(resultsa$total_biomass)

a

# Run results
tableA = resultTableA(x = trans)
tableB = resultTableB(x = trans)
tableC = resultTableC(x = resultsa)

tableC$Layer = "Main Box"

# Scots
write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

