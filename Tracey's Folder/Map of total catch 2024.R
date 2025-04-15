setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/"))
library(ggplot2)
library(patchwork)
library(scales)
library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
#library(weathercan)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
#library(rgeos)
library(sf)
library(terra)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(DT)
library(dygraphs)
library(leaflet)
library(rmapshaper)
library(plotly)
library(mapproj)
library(oce) #new CTD Data package
library(pander)
library(geodata)
library(pacman)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(devtools)
library(maps)
library(dplyr)
#install.packages("measurements")
library(measurements)
#Land Data
#can<-getData('GADM', country="CAN", level=1) #getData is discontinued
can<-gadm(country='CAN', level=1, path = "geodata_default_path",version="latest", resolution = 1, regions = c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador", "Québec"))
#us = getData('GADM', country = "USA", level = 1) # getData is discontinued
us<-gadm(country='USA', level=1, path = "geodata_default_path",version="latest", resolution = 1, regions = c("Maine"))
can1 = rbind(can,us)
NBNS = can1


#NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]
NBNS <- as(NBNS, "Spatial") #This causes it to run very slowly - takes a few minutes to process.

#2024 Total Catch Data
setwd(paste0("C:/Users/herri/Desktop/"))
TotalCatch = read.csv("2024 Total Catch.csv")

#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")

# Scots Bay plankton and CTD box
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]

# Scots Bay
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

#German Bank CTD box
GBCTD=boxes[which(boxes$Box == "GBocean"), ]

# German Bank      
SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")

# Seal Island      
SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")



#Boundaries for all Boxes

#CP <- as(extent(-66.5, -65.75, 43, 45.5), "SpatialPolygons")


#Filter Ground to SB only
ScotsBayTotalCatch = subset(TotalCatch, convLat >44.5 & convLat <46 & convLon < 66.5 & convLon > 63)
ScotsBayTotalCatch$convLon = ScotsBayTotalCatch$convLon*-1

#Boundaries for Scots Bay

CP <- as(extent(-66.5, -63, 44.5, 46), "SpatialPolygons") #set boundaries for Scots Bay before plotting
proj4string(CP) <- CRS(proj4string(NBNS))
out <- crop(NBNS, CP, byid=TRUE)




#Plot of 2024 catches in Scots Bay

ggplot(ScotsBayTotalCatch, aes(x=convLon, y=convLat)) +
  geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  
  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) +
  geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) +
  geom_polygon(data=polyEastern,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + 
  geom_point(data=ScotsBayTotalCatch, aes(fill = MT), pch=21, alpha = 0.6) +
  labs(x=NULL, y=NULL) + 
  coord_map()

#Inner Bay of Fundy

IBoFTotalCatch <- subset(TotalCatch, convLat  >44.15 & convLat <46 & convLon < 67.2 & convLon > 63)
IBoFTotalCatch$convLon = IBoFTotalCatch$convLon*-1

CP <- as(extent(-67.2, -63, 44.1, 46), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- crop(NBNS, CP, byid=TRUE)


#Plot of all 2024 catches
ggplot(IBoFTotalCatch, aes(x=convLon, y=convLat)) +
  geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  
  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) +
  geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) +
  geom_polygon(data=polyEastern,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + 
  geom_point(data=IBoFTotalCatch, aes(fill = MT), pch=21, alpha = 0.6) +
  labs(x=NULL, y=NULL) + 
  coord_map()


#Removing outlying catch data. Ex. Ones on land, off CB, off Halifax (Halifax may be valid - look into further)

TotalCatch$convLat <- as.numeric(as.character(TotalCatch$convLat))
TotalCatch$convLon <- as.numeric(as.character(TotalCatch$convLon))  

TotalCatch <- subset(TotalCatch, convLat  >43 & convLat <46)


CP <- as(extent(-68, -60, 43, 49), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- crop(NBNS, CP, byid=TRUE)


#Plot of all 2024 catches
ggplot(TotalCatch, aes(x=convLon, y=convLat)) +
  geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  
  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) +
  #geom_polygon(data=SBplankton,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) +
  #geom_polygon(data=SBCTD,aes(x=X, y=Y, group=PID), colour = "black", fill = "white", linetype = 3) +
  geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) +
  geom_polygon(data=polyEastern,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + 
  geom_point(data=TotalCatch, aes(fill = MT), pch=21, alpha = 0.6) +
  labs(x=NULL, y=NULL) + 
  coord_map()


