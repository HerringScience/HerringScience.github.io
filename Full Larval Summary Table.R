
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

options(ggrepel.max.overlaps = Inf)

## Importing Data

#Larval Data
Larval <- read_csv("Full Larval.csv")
Larval$Year <- as.factor(Larval$Year)
Larval$category <- as.factor(Larval$category)
Larval$Survey.No <- as.factor(Larval$Survey.No)
Larval$Date <- as.Date(Larval$Date)

#Adds TowID based on date
Larval <- Larval %>% 
              group_by(Ground) %>%
              transform(TowID = as.numeric(factor(Date)))

#Add Replicate Tow 1 or 2

Larval1 <- Larval %>%
              group_by(TowID) %>%
              summarize(TowReplicate = n_distinct(id))
              
Larval1$TowReplicate <- as.character(Larval1$TowReplicate)
  Larval1$TowReplicate[Larval1$TowReplicate == "1"] <- "False"
  Larval1$TowReplicate[Larval1$TowReplicate == "2"] <- "True"

Larval <- merge(Larval, Larval1, by = "TowID")

#Plankton Data
Plankton <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/planktonsamplingData.csv")



# if preservative is formalin, apply L  = 0.984 + 0.993 x X1. (X1 = fixed/preserved length therefore Larval$Lengthmm, L = Live length.) 
# if preservation is alcohol apply L = 0.532 + 0.989 x X1 
#This is taken from Fox 1996 alcohol vs Formalin paper. They did 5% and 5 minute net capture simulation. They did suggest that this adjustment would be less accurate the longer the tow period.


Larval$LengthAdjustment = with(Larval, ifelse(Larval$Preservative == "4% formalin", (0.984 + 0.993* Larval$Lengthmm),
                                              ifelse(Larval$Preservative == "70% Alcohol", (0.532 + 0.989*Larval$Lengthmm),
                                                     ifelse(Larval$Preservative == "Unknown", NA,
                                                            ifelse(Larval$Preservative == "70% Alcohol (1) 4% Formalin (1)", NA, NA)))))

MeanLengthAdjustment <- aggregate(LengthAdjustment~id, Larval, mean)
  colnames(MeanLengthAdjustment)[2]<- "MeanLengthAdjustment"

#Number of observations will drop from Larval when applying Mean Length Adjustment as Unknown and mixed preservatives do not have an adjusted formula. 

Larval <- merge(Larval, MeanLengthAdjustment, by = "id")

# 0 is NA in these categories to allow for other data to be pulled.

Larval$Volume[is.na(Larval$Volume)] <- 0
Larval$Density[is.na(Larval$Density)] <- 0
Larval$AvgTowDepth[is.na(Larval$AvgTowDepth)] <- 0
Larval$TowTime[is.na(Larval$TowTime)] <- 0
Plankton$TowTime[is.na(Plankton$TowTime)] <- 0

Larval$Volume <- as.numeric(Larval$Volume)

#'Exact' spawn date. Growth rate of .24mm/day based on Chenoweth 1989 paper. 
# Paper says applies estimate growth rates to calculate the number of days back to 5mm. Took 5mm off total length to account for this.
# Assumes hatching length is 5mm, day of hatching = day 0

Larval$AdjustedAgeInDays <- ((Larval$LengthAdjustment - 5)/0.24)

Larval <- merge(Larval, MeanLengthAdjustment)
  
Larval$AdjustedSpawnDate <- Larval$Date-Larval$AdjustedAgeInDays

AdjustedMeanAgeInDays <- aggregate(AdjustedAgeInDays~id, Larval, mean)
  colnames(AdjustedMeanAgeInDays)[2]<- "AdjustedMeanAgeInDays"
AdjustedMinDateOfSpawn <- aggregate(AdjustedSpawnDate~id, Larval, min)
  colnames(AdjustedMinDateOfSpawn)[2] <- "AdjustedMinDateOfSpawn"
AdjustedMaxDateOfSpawn <- aggregate(AdjustedSpawnDate~id, Larval, max)
  colnames(AdjustedMaxDateOfSpawn)[2] <- "AdjustedMaxDateOfSpawn"
  
AdjustedDays <- merge(AdjustedMaxDateOfSpawn, AdjustedMinDateOfSpawn, by = 'id')
AdjustedDays <- merge(AdjustedDays, AdjustedMeanAgeInDays, by = 'id')

Larval <- merge(Larval, AdjustedDays, by = 'id')

# Adding in Tow start and end coordinates

TowCoords <- data.frame(aggregate(Lat1~id, Plankton, mean),
                        aggregate(Lon1~id, Plankton, mean),
                        aggregate(Lat2~id, Plankton, mean),
                        aggregate(Lon2~id, Plankton, mean))

TowCoords <- TowCoords[, !grepl("id.", names(TowCoords))]

Larval <- merge(Larval, TowCoords, by = 'id')

MeanVolume <- aggregate(Volume~id, Larval, mean)
  colnames(Volume)[2] <- "MeanVolume"

Larval <- merge(Larval, Volume, by = 'id')

LarvalSum <- Larval %>% select("Ground",
                               "Date",
                               "Year", 
                               "id", 
                               "TowReplicate",
                               "TowID",
                               "Survey.No", 
                               "Abundance", 
                               "Density", 
                               "MeanVolume", 
                               "Preservative",
                               "MeanLengthAdjustment", 
                               "AdjustedMeanAgeInDays", 
                               "AdjustedMinDateOfSpawn", 
                               "AdjustedMaxDateOfSpawn", 
                               "Lat1",
                               "Lon1",
                               "Lat2",
                               "Lon2",
                               "TowTime",
                               "AvgTowDepth")

LarvalSum <- unique(LarvalSum)



#Left in because this should update the tow time with means


#LarvalSum <- merge(LarvalSum, Plankton, by = 'id')
  LarvalSum[LarvalSum == 0] <- NA
  Larval[Larval == 0] <- NA
  
#Saving as .csv file. Saved to Main Data on Github. Should change to Larval Data in Source Data.
  write.csv(LarvalSum,"C:/Users/herri/Documents/GitHub/HerringScience.github.io//Main Data/LarvalSum.csv" )


#Used Boxplot to more accurately see the bulk of abundance. Hard to differentiate individuals and ids in scatterplot.
  
for(i in unique(Larval$Year)) {
  cat("\n")
  cat(i, "\n")
  cat("\n")

print(ggplot(subset(Larval, Ground == "SB" & Year == i), aes(x=Survey.No, y=Lengthmm)) +
        geom_boxplot(aes(colour = id)) +
        scale_x_discrete(name = "Survey Number", drop = FALSE) +
        ylab("Length (mm)"))
      cat("\n")
}

for(i in unique(Larval$Year)) {
  cat("\n")
  cat(i, "\n")
  cat("\n")
  
  print(ggplot(subset(Larval, Ground == "GB" & Year == i), aes(x=Survey.No, y=Lengthmm)) +
          geom_boxplot(aes(colour = id)) +
          scale_x_discrete(name = "Survey Number", drop = FALSE) +
          ylab("Length (mm)"))
  cat("\n")
}

for(i in unique(Larval$Year)) {
  cat("\n")
  cat(i, "\n")
  cat("\n")
  
  print(ggplot(subset(Larval, Ground == "SI" & Year == i), aes(x=Survey.No, y=Lengthmm)) +
          geom_boxplot(aes(colour = id)) +
          scale_x_discrete(name = "Survey Number", drop = FALSE) +
          ylab("Length (mm)"))
  cat("\n")
}
