
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

#LarvalSI <- filter(Larval, Ground == "SI")
#LarvalSB <- filter(Larval, Ground == "SB")
#LarvalGB <- filter(Larval, Ground == "GB")

#Plankton Data
Plankton <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/planktonsamplingData.csv")

#'Exact' spawn date. Growth rate of .24mm/day based on Chenoweth 1989 paper. 
# Paper says applies estimate growth rates to calculate the number of days back to 5mm. Took 5mm off total length to account for this.
# Assumes hatching length is 5mm, day of hatching = day 0

Larval$AgeInDays <- ((Larval$Lengthmm - 5)/0.24)
Larval$SpawnDate <- Larval$Date-Larval$AgeInDays
LarvalA <- select(Larval, id, Date, Survey.No, Abundance, Density, TowTime, AvgTowDepth) 

  
MeanAgeInDays <- aggregate(AgeInDays~id, Larval, mean)
  colnames(MeanAgeInDays)[2]<- "MeanAgeInDays"
MinDateOfSpawn <- aggregate(SpawnDate~id, Larval, min)
  colnames(MinDateOfSpawn)[2] <- "MinDateOfSpawn"
MaxDateOfSpawn <- aggregate(SpawnDate~id, Larval, max)
  colnames(MaxDateOfSpawn)[2] <- "MaxDateOfSpawn"

   
LarvalB <- merge(MeanAgeInDays, MinDateOfSpawn)
LarvalC <- merge(LarvalB, MaxDateOfSpawn)
  LarvalC <- merge(LarvalA, LarvalC)
  LarvalC <- unique(LarvalC)
  
StartLat <- aggregate(Lat1~id, Plankton, mean)
StartLon <- aggregate(Lon1~id, Plankton, mean)
StartCoords <- merge(StartLat, StartLon)

EndLat <- aggregate(Lat2~id, Plankton, mean)
EndLon <- aggregate(Lon2~id, Plankton, mean)
EndCoords <- merge(EndLat, EndLon)

TowCoords <- merge(StartCoords, EndCoords)

LarvalD <- merge(LarvalC, TowCoords)

Plankton <- select(Plankton, id, TowTime, AvgTowDepth, Volume)
  Plankton <- Plankton %>% drop_na(id)
  Plankton$AvgTowDepth[is.na(Plankton$AvgTowDepth)] <- 0

Plankton2 <- select(Plankton, id, TowTime)
  Plankton2 <- Plankton2 %>% drop_na(TowTime)
  MeanTowTime <- aggregate(TowTime~id, Plankton2, mean)

#Left in because this should update the tow time with means
LarvalE <- merge(LarvalD, MeanTowTime)

Volume <- select(Plankton, id, Volume)

LarvalF <- merge(LarvalE, Volume)

LarvalSum <- Larval %>% select("Ground", "Year", "id", "Survey.No", "Abundance", "Density", "Preservative", "TowTime", "MeanLength")
  LarvalSum <- merge(LarvalSum, LarvalF)
  LarvalSum <- unique(LarvalSum)

write.csv(LarvalSum,"C:/Users/herri/Documents/GitHub/HerringScience.github.io//Main Data/LarvalSum.csv" )
