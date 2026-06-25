

# July 11, 2020
# 8 boats, 6 in the main box and one each in the northern and eastern boxes
# LJ eastern box
# Canada 100 in the northern box
# No catches were made


# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata","PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

                loadfunctions( "acousticHerring")
                loadfunctions( "polygons")
                
                boxes = read.csv("surveyBoxes.csv")

# Load land data
                can<-getData('GADM', country="CAN", level=1) # provinces
                NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
                CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
                proj4string(CP) <- CRS(proj4string(NBNS))
                out <- gIntersection(NBNS, CP, byid=TRUE)


SUA = read.csv("polygon_SB5.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern11.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern1.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))




boxes = read.csv("timGrounds.csv")


# Survey analysis

# july 11, 2020 Scots Bay
# regions = read.table("RegionJul11_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# redo with 0.5m offset
regions = read.table("RegionJul11_2020_2.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


# Map
mapping = read.table("MapJul11_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

# Use target strength from July 6, 2019
trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

#trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )


name = "July 11, 2020 - Scots Bay #4 Survey Track"
x = surveyTrack3(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern, 
                polyNameC  = polySB_eastern, title = name )


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


x = trans
polyNameA  = polySB_main 
polyNameB  = polySB_northern
polyNameC  = polySB_eastern
title = name
ggplot(x, aes(x=X, y=Y))  + geom_polygon(data=polyNameA,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") +geom_polygon(data=polyNameB,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_polygon(data=polyNameB,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")+ geom_polygon(data=polyNameC,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black')



# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jul11_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove C1 and LJ
trans_survey1=trans_survey[which(trans_survey$Vessel != "C1"), ]
trans_survey2=trans_survey1[which(trans_survey1$Vessel != "LJ"), ]

# Northern
northern=trans_survey[which(trans_survey$Vessel == "C1"), ]

# eastern
eastern=trans_survey[which(trans_survey$Vessel == "LJ"), ]

area = calcArea(polySB_main) 
area - 1.61

calcArea(polySB_northern)
calcArea(polySB_eastern)

# Results
resultsa = biomassCalc(x = trans_survey2, areaKm = 640.79)
j  = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 77.15)
g = unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 113.06)
m = unique(resultsc$total_biomass)


j+g+m


# Run results
                tableA = resultTableA(x = trans_survey2)
                tableB = resultTableB(x = trans_survey2)
                tableC = resultTableC(x = resultsa)

                tableD = resultTableA(x = northern)
                tableE = resultTableB(x = northern)
                tableF = resultTableC(x = resultsb)

                tableG = resultTableA(x = eastern)
                tableH = resultTableB(x = eastern)
                tableI = resultTableC(x = resultsc)

        tableC$Layer = "Main Box"
        tableF$Layer = "Northern Box"
        tableI$Layer = "Eastern Box"
        
        A = rbind(tableA,tableD, tableG)
        B = rbind(tableB,tableE, tableH)
        C = rbind(tableC,tableF, tableI)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        