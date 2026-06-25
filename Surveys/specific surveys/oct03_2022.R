
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

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


#October 3, 2022

regions = read.table("Region_Oct03_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
mapping = read.table("Map_Oct03_2022.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)

# TS Standard


trans = transects(x= regions, TS38 = -35.5, TS50 = NA )
head(trans)

trans_1 = trans[which(trans$RegionName != "FM_T02"), ]


SUA = read.csv("polygon_Oct03.csv")
polySB_main = as.PolySet(SUA, projection="LL")



x = surveyTrack(x=trans_1, polyNameA  = polySB_main,  title = name )

head(map)
str(map)
map$Region_name = as.factor(map$Region_name)

unique(map$Region_name)
map_1 = map[which(map$Region_name != "Oct03_2022FM_T02"), ]

ggplot(map_1, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)



# Specify the survey you are doing an analysis for  
#trans_survey= trans[which(trans$Survey_date == "May24_2021"), ]

# QC

unique(trans_1$Survey_date)
unique(trans_1$Vessel)

area = calcArea(polySB_main) 

#390


# Results
resultsa = biomassCalc(x = trans_1, areaKm = 390)
a = unique(resultsa$total_biomass)
a


#resultsa = biomassCalc(x = trans_1, areaKm = 406)
#a = unique(resultsa$total_biomass)
#a




# Run results
tableA = resultTableA(x = trans_1)
tableB = resultTableB(x = trans_1)
tableC = resultTableC(x = resultsa)

        # Scots
        write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        