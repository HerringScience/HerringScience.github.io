rm(list = ls())

#Options
Ground = "GB" #either "SB" or "GB"

#Set vessels for SB only
ids = c("LM","LB", "MS", "SL", "C1", "BP")
NorthVessel = "FM"
EastVessel = "LJ"

#Area and TS values
SB1= 618 #SB main area
SB2= 77 #SB north area
SB3= 115 #SB east area

SBTS1 = -35.5 #TS38
GBTS1 = -35.5 #TS38

GB1 = 805 #GB main area
GB2 = 267 #Seal Island area
GB3 = 0.77 #Ad-hoc school survey area

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/"))
library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
library(weathercan)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
library(rgeos)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)

#Land Data
can<-getData('GADM', country="CAN", level=1)
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]

# Proper coordinates for German Bank
GBMap <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
proj4string(GBMap) <- CRS(proj4string(NBNS))
GBout <- gIntersection(NBNS, GBMap, byid=TRUE)

# Proper coordinates for Scots Bay
SBMap <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(SBMap) <- CRS(proj4string(NBNS))
SBout <- gIntersection(NBNS, SBMap, byid=TRUE)

#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")
SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")
SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")
GBCTD=boxes[which(boxes$Box == "GBocean"), ]
SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")
SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")

#Load functions
pathnames <- list.files(pattern="[.]R$", path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Functions"), full.names=TRUE)
sapply(pathnames, FUN=source)

#Echoview Data
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Test"))
mapping = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Test"), pattern = "Map") %>% 
  map_df(~read_csv(.))
regions = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Test"), pattern = "Region") %>% 
  map_df(~read_csv(.))

if(Ground == "SB"){
out = SBout
map = mapDat(x = mapping)
x = regions
trans = transects(x= regions, TS38 = SBTS1, TS50 = NA)
x = surveyTrack3(x=trans, polyNameA  = polySB_main, polyNameB  = polyNorthern,  polyNameC  = polyEastern,  title = name)
northern = trans[which((trans$Vessel == NorthVessel)), ]
eastern = trans[which((trans$Vessel == EastVessel)), ]
main = trans[which((trans$Vessel %in% ids)), ]
ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

# Results
resultsa = biomassCalc(x = main, areaKm = SB1)
resultsb = biomassCalc(x = northern, areaKm = SB2)
resultsc = biomassCalc(x = eastern, areaKm = SB3)

tableA = resultTableA(x = main)
tableB = resultTableB(x = main)
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

write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
}

if(Ground=="GB"){
out=GBMap
map = mapDat(x = mapping)
x = regions
trans = transects(x= regions, TS38 = GBTS1 , TS50 = NA)
  
ids = c("T01", "T02", "T03")
trans1 = trans[which((trans$Transect_No %in% ids)), ]
ids = c("T04", "T05", "T06", "T07")
trans2 = trans[which((trans$Transect_No %in% ids)), ]

x = surveyTrack2(x=trans1, polyNameA  = polyGB, polyNameB  = polySI, title = name )

SUA = list.files(path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Test"), pattern = "adhoc") %>% 
  map_df(~read_csv(.))
polyAD = as.PolySet(SUA, projection="LL")

x = surveyTrack2(x=trans2, polyNameA  = polyGB, polyNameB  = polyAD, title = name )

ggplot(trans2, aes(x=X, y=Y)) + geom_polygon(data=polyAD,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map()

ids = c("T01", "T02", "T03")
map1 = map[which((map$Transect_No %in% ids)), ]

ggplot(map1, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)

SI = trans[which(trans$Transect_No == "T03"), ]
ids = c("T01", "T02")
GB = trans[which((trans$Transect_No %in% ids)), ]

#Results
resultsa = biomassCalc(x = GB, areaKm = GB1)
resultsb = biomassCalc(x = SI, areaKm = GB2)
resultsc = biomassCalc(x = trans2, areaKm = GB3)

tableA = resultTableA(x = GB)
tableB = resultTableB(x = GB)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = SI)
tableE = resultTableB(x = SI)
tableF = resultTableC(x = resultsb)

tableG = resultTableA(x = trans2)
tableH = resultTableB(x = trans2)
tableI = resultTableC(x = resultsc)

tableC$Layer = "German Bank"
tableF$Layer = "Seal Island"
tableI$Layer = "School Survey"

A = rbind(tableA,tableD, tableG)
B = rbind(tableB,tableE, tableH)
C = rbind(tableC,tableF, tableI)

write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)}
