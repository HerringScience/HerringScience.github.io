

# September 6, 2020
# 8 boats in the main box
# There were samples
# TS applied from September 1, 2019: -34.9690 


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
                
                # mini
                
                CP <- as(extent(-64.8, -64.7, 45.2, 45.3), "SpatialPolygons")
                
                
                
                
                proj4string(CP) <- CRS(proj4string(NBNS))
                out <- gIntersection(NBNS, CP, byid=TRUE)


# Survey analysis

# September 6, 2020 Scots Bay
        regions = read.table("RegionSep06_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


                ids_main = c("T01", "T02")
                regionsMain = regions[which((regions$Transect %in% ids_main)), ]

# Map
                mapping = read.table("MapSep06_20.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

                map = mapDat(x = mapping)


# Use target strength from Sep 1, 2019
trans = transects(x= regions, TS38 = -34.9690 , TS50 = NA )

trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )


ids_mini = c("T03", "T04","T05", "T06", "T07" )
mini = trans[which((trans$Transect %in% ids_mini)), ]

ids_main = c("T01", "T02")
main = trans[which((trans$Transect %in% ids_main)), ]


SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")



name = "August 8, 2020 - Scots Bay #6 Survey Track"
x = surveyTrack2(x=main, polyNameA  = polySB_main, polyNameB = polySB_mini, title = name )



SUA = read.csv("polygon_SBmini.csv")
polySB_mini = as.PolySet(SUA, projection="LL")

name = "August 8, 2020 - Scots Bay #6 Survey Track"
x = surveyTrack(x=mini, polyNameA  = polySB_mini, title = name )

name = "August 8, 2020 - Scots Bay #6 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_mini, title = name )



# Mapping

head(map)

miniMap = map[which((map$Transect_No %in% ids_mini)), ]

mainMap = map[which((map$Transect_No %in% ids_main)), ]


ggplot(mainMap, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

ggplot(miniMap, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)




# Specify the survey you are doing an analysis for  
trans_survey= main[which(main$Survey_date == "Sep06_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


area = calcArea(polySB_main) 
area - 1.61


calcArea(polySB_mini) 


# Results
resultsa = biomassCalc(x = trans_survey, areaKm = 630.1874)

resultsb = biomassCalc(x = mini, areaKm = 4.6)



unique(resultsa$total_biomass)


# Run results
                tableA = resultTableA(x = trans_survey)
                tableB = resultTableB(x = trans_survey)
                tableC = resultTableC(x = resultsa)
                
                tableD = resultTableA(x = mini)
                tableE = resultTableB(x = mini)
                tableF = resultTableC(x = resultsb)
                
                tableC$Layer = "Main Box"
                tableF$Layer = "Mini Survey"
                
                A = rbind(tableA,tableD)
                B = rbind(tableB,tableE)
                C = rbind(tableC,tableF)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        