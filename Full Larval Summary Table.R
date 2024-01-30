
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
library(ggrepel)

#Larval Data
Larval <- read_csv("Full Larval.csv")
Larval$Year <- as.factor(Larval$Year)
Larval$category <- as.factor(Larval$category)
Larval$Survey.No <- as.factor(Larval$Survey.No)

# 0 is NA in these categories to allow for other data to be pulled.

Larval$Volume[is.na(Larval$Volume)] <- 0
Larval$Density[is.na(Larval$Density)] <- 0
Larval$AvgTowDepth[is.na(Larval$AvgTowDepth)] <- 0
Larval$TowTime[is.na(Larval$TowTime)] <- 0

#Plankton Data
Plankton <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/planktonsamplingData.csv")
Plankton$TowTime[is.na(Plankton$TowTime)] <- 0

#'Exact' spawn date. Growth rate of .24mm/day based on Chenoweth 1989 paper. 
# Paper says applies estimate growth rates to calculate the number of days back to 5mm. Took 5mm off total length to account for this.
# Assumes hatching length is 5mm, day of hatching = day 0

Larval$AgeInDays <- ((Larval$Lengthmm - 5)/0.24)
Larval$SpawnDate <- Larval$Date-Larval$AgeInDays
LarvalA <- select(Larval, id, Date, Survey.No, Abundance, Density, Volume, TowTime, AvgTowDepth) 

  
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

LarvalC$Density[is.na(LarvalC$Density)] <- 0 
LarvalC$AvgTowDepth[is.na(LarvalC$AvgTowDepth)] <- 0
LarvalC$TowTime[is.na(LarvalC$TowTime)] <- 0

LarvalD <- merge(LarvalC, TowCoords)

#Plankton <- select(Plankton, id, TowTime, AvgTowDepth, Volume)
  Plankton <- Plankton %>% drop_na(id)
  Plankton$AvgTowDepth[is.na(Plankton$AvgTowDepth)] <- 0

Plankton2 <- select(Plankton, id, TowTime, AvgTowDepth)
  Plankton2 <- Plankton2 %>% drop_na(TowTime)
  MeanTowTime <- aggregate(TowTime~id, Plankton2, mean)

#Left in because this should update the tow time with means
LarvalE <- merge(LarvalD, MeanTowTime)

Volume <- aggregate(Volume~id, Larval, mean)

LarvalF <- merge(LarvalE, Volume)

LarvalSum <- Larval %>% select("Ground", "Year", "id", "Survey.No", "Abundance", "Density", "Preservative", "TowTime", "MeanLength", "Volume")
  LarvalSum <- merge(LarvalSum, LarvalF)
  LarvalSum <- unique(LarvalSum)
  LarvalSum[LarvalSum == 0] <- NA

write.csv(LarvalSum,"C:/Users/herri/Documents/GitHub/HerringScience.github.io//Main Data/LarvalSum.csv" )

# Abundance by Year, differentiating Survey Numbers. Need to separate by Ground

print(ggplot(LarvalSum, aes(Year, Abundance, colour = Survey.No)) +
        geom_point() +
        geom_label_repel(aes(label = id), size = 3))

print(ggplot(LarvalSum, aes(Year, Density, colour = Survey.No)) +
        geom_point())

print(ggplot(LarvalSum, aes(Year, MeanLength, colour = Survey.No)) +
        geom_point())

print(ggplot(LarvalSum, aes(Year, MeanAgeInDays, colour = Survey.No)) +
        geom_point())

print(ggplot(LarvalSum, aes(Year, Abundance, colour = Survey.No)) +
        geom_point())