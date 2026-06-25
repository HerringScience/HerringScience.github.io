
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("surveyBoxes.csv")

# plankton
plankton=boxes[which(boxes$Box == "PlanktonBox"), ]

## German Bank/Seal Island #5

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -66, 43, 43.5), "SpatialPolygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("timGrounds.csv")
SUA = read.csv("polygon_GB_14.csv")
polyGB = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SI2021_Aug23.csv")
polySI = as.PolySet(SUA, projection="LL")

# Survey analysis
# GM and JM comparison, only for comparison purposes
regions = read.table("Comparison.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

x = regions
trans = transects(x= regions, TS38 = -35.5 , TS50 = NA )

SUA = read.csv("polygon_GB_14.csv")
polyGB = as.PolySet(SUA, projection="LL")

unique(trans$Transect_No)


x = surveyTrack2(x=trans, polyNameA  = polyGB, polyNameB  = polySI, title = name )


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Oct10_2022"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)
unique(trans_survey$Editor)

str(trans_survey$Editor)
trans_survey$Editor = as.factor(trans_survey$Editor)


transGM = trans[which(trans$Editor == "GM"), ]
transJM = trans[which(trans$Editor == "JM"), ]

# Results
resultsGM = biomassCalc(x = transGM, areaKm = 805)
GM = unique(resultsGM$total_biomass)
#30,157

resultsJM = biomassCalc(x = transJM, areaKm = 805)
JM = unique(resultsJM$total_biomass)
#24,166

GM-JM

GM-JM
5991/GM*100
20% difference...

# Create Bar Graphs to look at transect differences

unique(trans$Region_name)
head(trans)


ggplot(trans, aes(fill=Editor, y=Area_Backscatter_Strength, x=RegionName)) + geom_bar(position="dodge", stat="identity")


transGM = transGM[order(transGM$RegionName),]
transJM = transJM[order(transJM$RegionName),]


diff = transGM$Area_Backscatter_Strength - transJM$Area_Backscatter_Strength

Pdiff = (transGM$Area_Backscatter_Strength - transJM$Area_Backscatter_Strength)/transGM$Area_Backscatter_Strength * 100
 


transJM$diff = diff
transJM$Pdiff = Pdiff

transGM$diff = diff


ggplot(transJM, aes(y=diff, x=RegionName)) + geom_bar(position="dodge", stat="identity")
ggplot(transJM, aes(y=Pdiff, x=RegionName)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks = round(seq(min(transJM$Pdiff), max(transJM$Pdiff), by = 1),1))


round(seq(min(transJM$Pdiff), max(transJM$Pdiff)

# Run results

tableA = resultTableA(x = GB)
tableB = resultTableB(x = GB)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = SI)
tableE = resultTableB(x = SI)
tableF = resultTableC(x = resultsb)

tableG = resultTableA(x = SS)
tableH = resultTableB(x = SS)
tableI = resultTableC(x = resultsc)



tableC$Layer = "German Bank"
tableF$Layer = "Seal Island"
tableI$Layer = "School Survey"

A = rbind(tableA,tableD, tableG)
B = rbind(tableB,tableE,tableH)
C = rbind(tableC,tableF, tableI)

# Scots
write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

write.table(tableA, file= "tableA_GDM.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "tableB_GDM.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "tableC_GDM.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

save(tableA,file="tableA_GM_Oct10_2022.Rda")
save(tableB,file="tableB_GM_Oct10_2022.Rda")
save(tableC,file="tableC_GM_Oct10_2022.Rda")




