


rm(list=ls())
library(maptools)
library(rgdal)
library(sp)
library(raster)


DF<-as.data.frame(read.csv("Survey_boxes.csv", header = TRUE, sep = ","))

#p = Polygon(DF[(1:5),(2:3)],hole=TRUE)
#ps = Polygons(list(p),1)
#sps = SpatialPolygons(list(ps))
#proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

SB = Line(DF[DF[,1]=="SB",(2:3)])
GB = Line(DF[DF[,1]=="GB",(2:3)])
TL = Line(DF[DF[,1]=="TL",(2:3)])
LH = Line(DF[DF[,1]=="LH",(2:3)])
EP = Line(DF[DF[,1]=="EP",(2:3)])


Ls = Lines(list(SB,GB,TL,LH,EP),1)
Lps = SpatialLines(list(Ls))

raster::shapefile(Lps, "Survey_boxes.shp",overwrite=TRUE)
