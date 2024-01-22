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
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
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

#CTD Data
SST = read_csv("CTD SST.csv") #SST
polysT = read_csv("timGrounds.csv") #coloured ground maps
CTD = read_csv("CTD Full.csv") #All Data
atDepth = read_csv("CTD 30m.csv") #At 30m Depth > This one contains all Stratified Temp + Salinity data as well
SST$Year <- as.factor(SST$Year)
SST$Month <- as.factor(SST$Month)
atDepth$Year <- as.factor(atDepth$Year)
atDepth$Month <- as.factor(atDepth$Month)
CTD$Year <- as.factor(CTD$Year)
CTD$Month <- as.factor(CTD$Month)
CTD$Survey <- as.factor(CTD$Survey)
CTD <- CTD %>%
  mutate(Julian_factor = Julian)
CTD$Julian_factor <- as.factor(CTD$Julian_factor)

#Land Data
can<-getData('GADM', country="CAN", level=1)
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]


data("sealevel")
t <- sealevel("time")
t<-sealevel[["time"]]
eta <- sealevel[["elevation"]]
m <- tidem(sealevel)
etaDetided <- eta - predict(m)

slot(ctd)
