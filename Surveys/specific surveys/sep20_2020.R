

# September 20, 2020
# 4 boats in a subset of the main box
# There were samples
# TS applied from September 15, 2019: -34.8411 


# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

                loadfunctions( "acousticHerring")
                loadfunctions( "polygons")
                
                boxes = read.csv("timGrounds.csv")

# Load land data
                can<-getData('GADM', country="CAN", level=1) # provinces
                NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
                CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
                proj4string(CP) <- CRS(proj4string(NBNS))
                out <- gIntersection(NBNS, CP, byid=TRUE)


# Survey analysis

# September 20, 2020 Scots Bay
        regions = read.table("RegionSep20_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Map
                mapping = read.table("MapSep20_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

                map = mapDat(x = mapping)


# Use target strength from Sep 1, 2019
trans = transects(x= regions, TS38 = -34.8411  , TS50 = NA )

trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )


SUA = read.csv("polygon_SBSubset.csv")
polySB_sub = as.PolySet(SUA, projection="LL")


SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")


loadfunctions( "acousticHerring")


name = "September 20, 2020 - Scots Bay #9 Survey Track"
x = surveyTrack2(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_sub, title = name )

ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

# Mapping

head(map)

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Sep20_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


calcArea(polySB_sub) 


# Results
resultsa = biomassCalc(x = trans_survey, areaKm = 319.8)
unique(resultsa$total_biomass)


# Run results
                tableA = resultTableA(x = trans_survey)
                tableB = resultTableB(x = trans_survey)
                tableC = resultTableC(x = resultsa)
                
        # Scots
        write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        