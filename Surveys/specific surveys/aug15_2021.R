
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")

# plankton
plankton=boxes[which(boxes$Box == "PlanktonBox"), ]


# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)





SUA = read.csv("polygon_SB5.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBEastern.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")


# Survey analysis

# August 15, 2021 Scots Bay
regions = read.table("Region_Aug15_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Aug15_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Ad hoc August 17, 2021 Scots Bay Sealife II
regions = read.table("Region_Aug17_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Aug17_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)




map = mapDat(x = mapping)

x = regions

trans = transects(x= regions, TS38 = -35.51315083, TS50 = -35.609 )



#trans = transects(x= regions, TS38 = -35.1910 , TS50 = -35.609 )

x = surveyTrack2(x=trans, polyNameA  = polySB_main,polyNameB  = polySB_eastern,  title = name )


SUA = read.csv("polygon_SLadHoc.csv")
polySB_adhoc = as.PolySet(SUA, projection="LL")


x = surveyTrack2(x=trans, polyNameA  = polySB_main,polyNameB  = polySB_adhoc, title = name )



ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)



# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug15_2021"), ]
trans_survey= trans[which(trans$Survey_date == "Aug17_2021"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)

# remove MS
trans_survey1=trans_survey[which(trans_survey$Vessel != "MS"), ]

# Eastern
eastern=trans_survey[which(trans_survey$Vessel == "MS"), ]


area = calcArea(polySB_main) 
area - 1.61

calcArea(polySB_northern)
calcArea(polySB_eastern)
calcArea(polySB_Ad)
calcArea(polySB_adhoc)
### 4.5

#12.71



# Results
resultsa = biomassCalc(x = trans_survey1, areaKm = 640.8)
a=unique(resultsa$total_biomass)


resultsb = biomassCalc(x = trans_survey, areaKm = 4.5)
b = unique(resultsb$total_biomass)


resultsc = biomassCalc(x = eastern, areaKm = 119.8)
c =unique(resultsc$total_biomass)

a+c


# Run results
tableA = resultTableA(x = trans_survey1)
tableB = resultTableB(x = trans_survey1)
tableC = resultTableC(x = resultsa)



tableA = resultTableA(x = trans_survey)
tableB = resultTableB(x = trans_survey)
tableC = resultTableC(x = resultsa)


write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)





tableG = resultTableA(x = eastern)
tableH = resultTableB(x = eastern)
tableI = resultTableC(x = resultsc)

        tableC$Layer = "Main Box"
        tableI$Layer = "Eastern Box"
        
        A = rbind(tableA,tableG)
        B = rbind(tableB,tableH)
        C = rbind(tableC,tableI)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
S        
        
        