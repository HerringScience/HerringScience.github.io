
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
Larval = read_csv("Full Larval.csv")
Larval$Year <- as.factor(Larval$Year)
Larval$category <- as.factor(Larval$category)
Larval$Survey.No <- as.factor(Larval$Survey.No)

LarvalSI = filter(Larval, Ground == "SI")
LarvalSB = filter(Larval, Ground == "SB")
LarvalGB = filter(Larval, Ground == "GB")

Larval$DaysFromSpawn <- Larval$Lengthmm/0.24
Larval$SpawnDate <- Larval$Date-Larval$DaysFromSpawn
LarvalSI <- merge(LarvalSI, LarvalSICount)

  
aggregate(DaysFromSpawn~id, Larval, mean)

LarvalSICount<- count(LarvalSI, id, wt = NULL) # This gives the correct count, but brings it down to 2 columns, id and count

Larval %>% count(id) # same as above

