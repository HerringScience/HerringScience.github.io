
#Remove 

rm(list = ls())


#Import all packages, CTD data, and land data

#Packages
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

#Larval Data
Larval <- read_csv("Full Larval.csv")
Larval$Year <- as.factor(Larval$Year)
Larval$category <- as.factor(Larval$category)
Larval$Survey.No <- as.factor(Larval$Survey.No)

LarvalSI <- filter(Larval, Ground == "SI")
LarvalSB <- filter(Larval, Ground == "SB")
LarvalGB <- filter(Larval, Ground == "GB")

#Plankton Data
Plankton <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/planktonsamplingData.csv")

Plankton <- select(Plankton, id, TowTime, AvgTowDepth)
Plankton <- Plankton %>% drop_na(id)
is.na(Plankton$AvgTowDepth) <- 0

#'Exact' spawn date. Growth rate of .24mm/day based on Chenoweth 1989 paper. 
# Paper says applies estimate growth rates to calculate the number of days back to 5mm. Took 5mm off total length to account for this.
# Assumes hatching length is 5mm, day of hatching = day 0

Larval$AgeInDays <- ((Larval$Lengthmm - 5)/0.24)
Larval$SpawnDate <- Larval$Date-Larval$AgeInDays

  
MeanAgeInDays <- aggregate(AgeInDays~id, Larval, mean)
  colnames(MeanAgeInDays)[2]<- "MeanAgeInDays"
MinDateOfSpawn <- aggregate(SpawnDate~id, Larval, min)
  colnames(MinDateOfSpawn)[2] <- "MinDateOfSpawn"
MaxDateOfSpawn <- aggregate(SpawnDate~id, Larval, max)
  colnames(MaxDateOfSpawn)[2] <- "MaxDateOfSpawn"
  
Larval2 <- merge(MeanAgeInDays, MinDateOfSpawn)
Larval3 <- merge(Larval2, MaxDateOfSpawn)
  
#AvgTowDepth <- data.frame(Plankton$id, Plankton$AvgTowDepth)
#AvgTowDepth[is.na(AvgTowDepth)] <- 0
 # colnames(AvgTowDepth)[1] <- "id"
  #colnames(AvgTowDepth)[2] <- "AvgTowDepth"
  
Larval4 <- merge(Larval3, AvgTowDepth)
  
MeanTowTime <- aggregate(TowTime~id, Plankton, mean)

StartLat <- aggregate(Lat1~id, Plankton, mean)
StartLon <- aggregate(Lon1~id, Plankton, mean)
StartCoords <- merge(StartLat, StartLon)

EndLat <- aggregate(Lat2~id, Plankton, mean)
EndLon <- aggregate(Lon2~id, Plankton, mean)
EndCoords <- merge(EndLat, EndLon)

TowCoords <- merge(StartCoords, EndCoords)



Larval5 <- merge(Larval4, MeanTowTime)
Larval6 <- merge(Larval5, TowCoords)

LarvalSum <- Larval %>% select("Ground", "Year", "id", "Survey.No", "Abundance", "Preservative", "X", "Y", "TowTime", "MeanLength")
LarvalSum <- merge(LarvalSum, Larval6)
LarvalSum <- unique(LarvalSum)


