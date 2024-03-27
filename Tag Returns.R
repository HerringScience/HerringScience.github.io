## Global options
rm(list = ls())

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
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(measurements)

#All Tags deployed

Tags = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))

#Tag Returns from during the AFF 44 funding

TagReturns2021 = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns 2021.csv"))
  TagReturns2021 = subset()

TagReturns2022 = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns 2022.csv"))
TagReturns2023 = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns 2023.csv"))


#Load all of Canada data and extract Atlantic provinces
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

#Set boundaries that you need to map 

# Proper coordinates for Tagging 
CP1 <- as(extent(-69, -62, 42, 46), "SpatialPolygons")
# Proper coordinates for GB plankton tow
CP2 <- as(extent(-67, -65, 43, 43.6), "SpatialPolygons")
# Coordinates for German Bank and Spec
CP3 <- as(extent(-67, -65, 43, 44), "SpatialPolygons")
# Scots
CP4 <- as(extent(-66, -63, 44, 46), "SpatialPolygons") 
# Scotia Shelf
CP5 <- as(extent(-65, -60, 43, 46), "SpatialPolygons") 
# Grand Manan Area
CP6 <- as(extent(-68, -66, 44, 45), "SpatialPolygons")

#Reduce Province data down to only the above extents/limits
proj4string(CP1) <- CRS(proj4string(NBNS))
All <- gIntersection(NBNS, CP1, byid=TRUE)

#Load boxes
#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
points = read.csv("Points.csv")
boxes = read.csv("timGrounds.csv")

#Tim Grounds
SUA = read.csv("timGrounds.csv")
#polyST = as.PolySet(SUA, projection="LL")

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

#Add tagging data
setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Source Data/"))
tags = read.csv("TaggingEvents.csv")

Tag1 = tags %>%
    filter(Tag_Num == 555955 |         
           Tag_Num == 555352 |
           Tag_Num == 505883 |
           Tag_Num == 556380 |
           Tag_Num == 508798 |
           Tag_Num == 508679 |
           Tag_Num == 509188 |
           Tag_Num == 565723 |
           Tag_Num == 476485 |
           Tag_Num == 524866)


Tag1 = Tag1 %>%
  dplyr::select(Tag_Num, Date, Lon, Lat, Vessel)

CP6 <- as(extent(-68, -66, 44, 45), "SpatialPolygons")
proj4string(CP6) <- CRS(proj4string(NBNS))
GM <- gIntersection(NBNS, CP6, byid=TRUE)


#All Data at once
ggplot(boxes,aes(x=X, y=Y)) + 
  geom_polygon(aes(colour = Box),fill= NA,lwd=1) + 
  geom_polygon(data=All,aes(x=long, y=lat, group=group)) + 
  geom_point(data=Tag1, aes(x=Lon, y=Lat, colour = Date), size=3) +
  coord_map() + 
  labs(x=NULL, y=NULL)

