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
library(geodata)
library(terra)
setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/"))
#All Tags deployed

Tags = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))

#Tag Returns from during the AFF 44 funding

TagReturns2021 = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/Tag Returns 2021.csv"))
TagReturns2022 = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/Tag Returns 2022.csv"))
TagReturns2023 = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/Tag Returns 2023.csv"))
TagReturns2024 = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/Tag Returns 2024.csv"))

# 1. Load Land Data (Returns terra SpatVector objects natively)
can <- gadm(country='CAN', level=1, path = "geodata_default_path", version="latest", 
            resolution = 1, regions = c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador", "Québec"))

us  <- gadm(country='USA', level=1, path = "geodata_default_path", version="latest", 
            resolution = 1, regions = c("Maine"))

NBNS <- rbind(can, us)

CP1 <- ext(-69, -62, 42, 46) # Proper coordinates for Tagging 
CP2 <- ext(-67, -65, 43, 43.6) # Proper coordinates for GB plankton tow
CP3 <- ext(-67, -65, 43, 44)   # Coordinates for German Bank and Spec
CP4 <- ext(-66, -63, 44, 46)   # Scots
CP5 <- ext(-65, -60, 43, 46)   # Scotia Shelf
CP6 <- ext(-68, -66, 44, 45)   # Grand Manan Area

All <- crop(NBNS, CP1)


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
Tags = read.csv("TaggingEvents.csv")

Tags = Tags %>% 
 dplyr::select(Tag_Num, Date, Lon, Lat, Vessel)

Tags$Date <- ymd(Tags$Date)
Tags$Year <- as.numeric(format(Tags$Date, "%Y"))

CP6 <- ext(-68, -66, 44, 45)
GM <- crop(NBNS, CP6)

#All Data at once

All_sf <- sf::st_as_sf(All)

ggplot() + 
  geom_sf(data = All_sf, fill = "grey70", color = "grey50") + 
  
  # Draw your tracking boxes
  geom_polygon(data = boxes, aes(x = X, y = Y, colour = Box), fill = NA, lwd = 1) + 
  geom_point(data = Tags, aes(x = Lon, y = Lat, colour = as.factor(Year)), size = 1) +
  coord_sf(xlim = c(-69, -62), ylim = c(42, 46)) + 
  labs(x = NULL, y = NULL, colour = "Year / Box") +
  theme_minimal()

