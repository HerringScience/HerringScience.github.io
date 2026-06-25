

# July 26, 2020
# 7 boats, 6 in the main box and one in the eastern boxes
# MS eastern box
# There were samples
# TS applied from July 20, 2019: -35.1558


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

#5
SUA = read.csv("polygon_SB2.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern1.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")



# Determine ordering

ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))







# Survey analysis

# July 26, 2020 Scots Bay
regions = read.table("RegionJuly26_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
# 0.5 offset
regions = read.table("Region_Jul26_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Map
mapping = read.table("MapJuly26_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Jul26_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)


# Use target strength from July 20, 2019
trans = transects(x= regions, TS38 = -35.1558 , TS50 = -35.609 )
trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )


SUA = read.csv("polygon_SB26.csv")
polySB_main = as.PolySet(SUA, projection="LL")

name = "July 26, 2020 - Scots Bay #5 Survey Track"
x = surveyTrack2(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_eastern,  title = name )


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)





# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jul26_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove MS
trans_survey1=trans_survey[which(trans_survey$Vessel != "MS"), ]

# eastern
eastern=trans_survey[which(trans_survey$Vessel == "MS"), ]

area = calcArea(polySB_main) 
area - 1.61

calcArea(polySB_northern)
calcArea(polySB_eastern)

# Results
resultsa = biomassCalc(x = trans_survey1, areaKm = 629.72)
j  = unique(resultsa$total_biomass)


resultsc = biomassCalc(x = eastern, areaKm = 113.06)
m = unique(resultsc$total_biomass)


j+m


# Run results
                tableA = resultTableA(x = trans_survey1)
                tableB = resultTableB(x = trans_survey1)
                tableC = resultTableC(x = resultsa)

                tableD = resultTableA(x = eastern)
                tableE = resultTableB(x = eastern)
                tableF = resultTableC(x = resultsc)

           
        tableC$Layer = "Main Box"
        tableF$Layer = "Eastern Box"

        A = rbind(tableA,tableD)
        B = rbind(tableB,tableE)
        C = rbind(tableC,tableF)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        