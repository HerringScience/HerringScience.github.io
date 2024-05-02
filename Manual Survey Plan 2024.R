### Survey Plans 2020

rm(list = ls())

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile

#Import all packages and data
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

setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/"))
setwd(paste0("C:/Users/herri/Herring Science Council/Tagging Project AFF 44 - Documents/Coding/"))

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active
# 
# loadfunctions("acousticHerring")
# loadfunctions( "polygons")

setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")
unique(boxes$Box)

# Scots Bay plankton and CTD box
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]
GBCTD=boxes[which(boxes$Box == "GBocean"), ]

# Scots Bay
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

# German Bank      

SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")

# Seal Island      

SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")



# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")

# look closely at the eastern box
CP <- as(extent(-64.8, -64.5, 45.2, 45.4), "SpatialPolygons")

# look closely at the northern box
CP <- as(extent(-65.25, -64.75, 45.2, 45.4), "SpatialPolygons")

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")

#Land Data - loads the land data the same way as Jenna's previous scripts
setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/"))
can<-getData('GADM', country="CAN", level=1)
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]

# # Proper coordinates for German Bank
# GBMap <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
# proj4string(GBMap) <- CRS(proj4string(NBNS))
# #GBout <- gIntersection(NBNS, GBMap, byid=TRUE)
# GBout <-crop(NBNS, GBMap, byid=TRUE)

# Proper coordinates for Scots Bay
SBMap <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(SBMap) <- CRS(proj4string(NBNS))
#SBout <- gIntersection(NBNS, SBMap, byid=TRUE)
SBout <- crop(NBNS, SBMap, byid=TRUE)

#Make CTD Cast Box (GB or SB)
a = c("4334.320", "4333.600", "4333.600", "4334.320")
b = c("6622.080", "6622.080", "6621.000", "6621.000")
c = c("4503.432", "4503.000", "4503.000", "4503.432")
d = c("6513.048", "6513.048", "6512.000", "6512.000")


#changed gIntersection to crop as it was discontinued
proj4string(CP) <- CRS(proj4string(NBNS))
out <- crop(NBNS, CP, byid=TRUE)


# Example of a Scots Bay Survey 

# Plan with all the transects, main, northern and eastern:
 setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Surveys/2024/SB1/"))
#trackSB = read.csv("july7_plan_ScotsBay.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
 
 #trackSB = read.csv("may29_2022Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
 
 trackSB = read.csv("May_05_2024SurveyPlanMain.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)



# Example of a German Bank/Seal Island Plan
# trackGB = read.csv("exampleGBSIplan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# 
# trackGB = read.csv("aug30_2020Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# trackGB = read.csv("oct25_2020Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# 
# 
# trackGB = read.csv("sep13_2020Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# 
# # Plot the transects: German Bank/Seal Island
# ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=GBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")
# 
# 
# # Plot the transects: German Bank/Seal Island
# ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=GBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")
# 
# 
# ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")
# 


# Load in all the survey polygons 


# Plot the transects: Scots Bay

setwd(paste0("C:/Users/herri/Desktop/"))

track1 = read.csv("May_05_2024SurveyPlanNorthxlsx.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

track1 = read.csv("May_05_2024SurveyPlanEastern.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

track1 = read.csv("May_05_2024SurveyPlanMain.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)



# This map was taken from Tag Returns and works. X and Ys were flipped.
ggplot(track1, aes(x=X, y=Y)) + 
  geom_polygon(data=SBout,aes(x=long, y=lat, group=group), fill = "gray") + 
  geom_polygon(data=SBplankton,aes(x=X, y=Y, group=PID),fill='white',col='black') +
  geom_polygon(data=SBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black') +
  geom_segment(aes(x=X, y=Y, xend=Xend, yend=Yend, colour=Vessel), size=0.5) +
  coord_map() + 
  labs(x=NULL, y=NULL, caption = "Figure 1. Survey lines to be completed by participating vessels.")



ggplot(trackSB, aes(x=X, y=Y))+ geom_polygon(data=polyEastern,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=SBplankton,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=SBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("") + ggtitle("Scots Bay Survey Plan July 7, 2019")


# May 2021
ggplot(trackSB, aes(x=X, y=Y))+ geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("") + ggtitle("Scots Bay Survey Plan May 2021")




trackSB = read.csv("sep20_2020plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

trackSB = read.csv("planSep20_20.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

trackSB = read.csv("CplanSep20_20.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

ggplot(trackSB, aes(x=X, y=Y))+ geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("") + ggtitle("Option C")




# Plot the transects: German Bank/Seal Island
# ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=GBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")+ ggtitle("German Bank Seal Island Survey Plan August 30, 2020")
# 
# 


# Determine the ordering of polygon points
ggplot(polyEastern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

# Lines for the Eastern and Northern Boxes
track = read.csv("easternLines2020.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
track = read.csv("northernLines2020.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

