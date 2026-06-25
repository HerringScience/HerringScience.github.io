
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

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





# Survey analysis

# Aug 4, 2019 Scots Bay
regions = read.table("Region_Aug04_2019.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Aug04_2019.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )

loadfunctions( "acousticHerring")

SUA = read.csv("polygon_SB_Aug04_2019.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern_Aug04.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern17.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


ggplot(data=polySB_eastern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


loadfunctions( "acousticHerring")

name = "Aug 04, 2019 - Scots Bay #5 Survey Track"
x = surveyTrack(x=trans, polyNameA  = polySB_eastern, polyNameB  = polySB_northern, 
                polyNameC  = polySB_main, title = name )

  # Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug04_2019"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove FM and SL
trans_survey1=trans_survey[which(trans_survey$Vessel != "SL"), ]
trans_survey2=trans_survey1[which(trans_survey1$Vessel != "FM"), ]

# Northern
northern=trans_survey[which(trans_survey$Vessel == "FM"), ]

# eastern
eastern=trans_survey[which(trans_survey$Vessel == "SL"), ]

area = calcArea(polySB_main) 
area - 1.61

calcArea(polySB_northern)
calcArea(polySB_eastern)

# Results
resultsa = biomassCalc(x = trans_survey2, areaKm = 633.6)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 59.2)
unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 133.2)
unique(resultsc$total_biomass)


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
        
        
        