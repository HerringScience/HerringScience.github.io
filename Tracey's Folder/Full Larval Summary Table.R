
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

## Importing 

### Larval QC also writes larval summary table/ LarvalSum, but is not ready to fully take over this script. Figure out how to fix it and condense it.

#Larval Data
Larval <- read_csv("Full Larval.csv")
LarvalSum <- read_csv("LarvalSum.csv")
#LarvalSum <- LarvalSum[c("id", "TowReplicate", "TowID")]
#Larval <- merge(Larval, LarvalSum, by = "id")

Larval$Year <- as.factor(Larval$Year)
Larval$category <- as.factor(Larval$category)
Larval$Survey.No <- as.factor(Larval$Survey.No)
Larval$Date <- as.Date(Larval$Date)

LarvalSum <- LarvalSum %>% select("Ground",
                               "Date",
                               "Year", 
                               "id", 
                               "TowReplicate",
                               "TowID",
                               "Survey.No", 
                               "Abundance", 
                               "Density", 
                               "Volume", 
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



  
#Saving as .csv file. Saved to Main Data on Github. Should change to Larval Data in Source Data.
 # write.csv(LarvalSum,"C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/LarvalSum.csv" )
#  write.csv(Larval, "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/Full Larval.csv")
#  write.csv(Larval, "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Full Larval.csv")


#Used Boxplot to more accurately see the bulk of abundance. Hard to differentiate individuals and ids in scatterplot.
  
# for(i in unique(Larval$Year)) {
#   cat("\n")
#   cat(i, "\n")
#   cat("\n")
# 
# print(ggplot(subset(Larval, Ground == "SB" & Year == i), aes(x=Survey.No, y=Lengthmm)) +
#         geom_boxplot(aes(colour = id)) +
#         scale_x_discrete(name = "Survey Number", drop = FALSE) +
#         ylab("Length (mm)"))
#       cat("\n")
# }
# 
# for(i in unique(Larval$Year)) {
#   cat("\n")
#   cat(i, "\n")
#   cat("\n")
#   
#   print(ggplot(subset(Larval, Ground == "GB" & Year == i), aes(x=Survey.No, y=Lengthmm)) +
#           geom_boxplot(aes(colour = id)) +
#           scale_x_discrete(name = "Survey Number", drop = FALSE) +
#           ylab("Length (mm)"))
#   cat("\n")
# }
# 
# for(i in unique(Larval$Year)) {
#   cat("\n")
#   cat(i, "\n")
#   cat("\n")
#   
#   print(ggplot(subset(Larval, Ground == "SI" & Year == i), aes(x=Survey.No, y=Lengthmm)) +
#           geom_boxplot(aes(colour = id)) +
#           scale_x_discrete(name = "Survey Number", drop = FALSE) +
#           ylab("Length (mm)"))
#   cat("\n")
# }
