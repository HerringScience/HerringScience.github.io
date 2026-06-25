

# August 23, 2020
# 8 boats in the main box
# There were samples
# TS applied from August 18, 2020: -35.0746 


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

SUA = read.csv("polygon_SB2.csv")
polySB_main = as.PolySet(SUA, projection="LL")



# Determine ordering

ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))







# Survey analysis

# August 23, 2020 Scots Bay
regions = read.table("Region_Aug23_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Map
mapping = read.table("Map_Aug23_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)


# Use target strength from Aug 23, 2020
trans = transects(x= regions, TS38 = -35.0746 , TS50 = NA )
trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )


SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

name = "August 8, 2020 - Scots Bay #6 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_main, title = name )


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)





# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug23_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


area = calcArea(polySB_main) 
area - 1.61

# Results
resultsa = biomassCalc(x = trans_survey, areaKm = 630.1874)
unique(resultsa$total_biomass)


# Run results
                tableA = resultTableA(x = trans_survey)
                tableB = resultTableB(x = trans_survey)
                tableC = resultTableC(x = resultsa)

                
           
        tableC$Layer = "Main Box"
        
        # Scots
        write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        