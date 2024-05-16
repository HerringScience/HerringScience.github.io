rm(list=ls())

#Import all packages, CTD data, and land data

#Packages
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/"))
library(ggplot2)
library(patchwork)
library(scales)
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
library(pander)

#Survey Data
surveyData <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv")

#Tagging Data
Tag = read_csv("TaggingEvents.csv") #Tagging Data
polysT = read_csv("timGrounds.csv") #Coloured ground maps
Tag$Year = as.factor(Tag$Year)
Tag$Vessel = as.factor(Tag$Vessel)
Tag$Survey = as.factor(Tag$Survey)
Tag$Tagger = as.factor(Tag$Tagger)

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

#SSB Data
SSB = read_csv("SSB Estimates.csv")
SSB$Year <- as.factor(SSB$Year)
SSB$Survey_Number <- as.factor(SSB$Survey_Number)
SSB$Ground <- as.factor(SSB$Ground)

#LRP Data
LRP2 = read_csv("LRP Data.csv")
LRP2 = LRP2 %>% rename(ThreeYear = "3yr Avg")

#Fat Data
FatData = read_csv("Total Fat Data.csv")

#Larval Data
#All Adjusted Ages and Dates are originally added in Larval QC script.
#All preservative length adjustments added in in Larval QC script.
# if preservative is formalin, apply L  = 0.984 + 0.993 x X1. (X1 = fixed/preserved length therefore Larval$Lengthmm, L = Live length.) 
# if preservation is alcohol apply L = 0.532 + 0.989 x X1 
#This is taken from Fox 1996 alcohol vs Formalin paper. They did 5% and 5 minute net capture simulation. They did suggest that this adjustment would be less accurate the longer the tow period.
# These equations are when the maximum shrinkage has occurred.
#Adjusted Spawn Date to account for incubation period. Using overall 10 days, as per NOAA info that says 7-10 days, and DFO stock assessment 2020 says 10-12 days.

Larval = read_csv("Full Larval.csv")

Larval$Date <- lubridate::ymd(Larval$Date)
Larval <- dplyr::arrange(Larval, Date)
Larval$Year <- as.factor(Larval$Year)
Larval$category <- as.factor(Larval$category)
Larval$Survey.No <- as.factor(Larval$Survey.No)
Larval$MonthDay <- format(Larval$Date, "%m-%d")
Larval$AdjustedJulianSpawnDate <- as.numeric(Larval$AdjustedJulianSpawnDate)

#Seal Island Larval
LarvalSI = filter(Larval, Ground == "SI")


LarvalSum = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/LarvalSum.csv"))
LarvalSum$Year <- as.factor(LarvalSum$Year)

#Land Data
can<-getData('GADM', country="CAN", level=1)
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]


#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")

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


#Biomass and survey totals

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/"))
towTotals <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/1999-2023 acoustic surveys data Scots Bay.csv"))
towTotals$Ground[towTotals$Year >= 1999] <- "SB"
towTotals$Month <- format(towTotals$Date, "%m")
towTotals$Day <- format(towTotals$Date, "%d")
towTotals$Julian <- format(towTotals$Date, "%j")
#towTotals$WeekNumber <- format
towTotals$Biomass <- as.numeric(towTotals$Biomass)
#towTotals$Biomass[is.na(towTotals$Biomass)] <- 0

GBtowTotals <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/1999-2023 acoustic surveys data German Bank.csv"))
GBtowTotals$Ground[GBtowTotals$Year >= 1999] <- "GB"
GBtowTotals$Month <- format(GBtowTotals$Date, "%m")
GBtowTotals$Day <- format(GBtowTotals$Date, "%d")
GBtowTotals$Julian <- format(GBtowTotals$Date, "%j")

annualTowTotals <- subset(towTotals, !is.na(Biomass))
annualTowTotals <- annualTowTotals %>%
  group_by(Year) %>%
  summarize('Annual Total Biomass' = sum(Biomass), 
            'No of Surveys' = length(Year))
annualTowTotals$`Annual Total Biomass`[annualTowTotals$`Annual Total Biomass` == 0] <- "NA"


towTotals <- towTotals %>%
  select("Year","Ground", "Date", "Biomass", "Month", "Day", "Julian")

# Tow times - use different spreadsheet to Larval. This is just going up to 2021. Export as excel file.
TowTimes <- surveyData %>%
  select(Year, Month, Day, Date, id, Ground, Time1, TowTime, AvgTowDepth)

TowTimes %>% write.csv(paste0("C:/Users/herri/Herring Science Council/Science Team - Documents/HSC Data Compendium/Survey Start Times Table.csv"))

#### Biomass Totals
kbl(towTotals, col.names=c("Year", "Ground", "Date", "Biomass", "Month", "Day", "Julian"), align = "c") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = F, fixed_thead = T)

kbl(annualTowTotals, col.names=c("Year", "Annual Total Biomass", "No of Surveys"), align = "c") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = F, fixed_thead = T )


#### Tow Table

# Get Start time for each tow in as well if possible.

kbl(TowTimes, col.names=c("Year", "Month", "Day", "Date", "ID", "Ground", "Start Time", "Tow Time (min)", "Tow Depth (m)"), align = "c") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = F)


#Survey Start date for Scots Bay

SBstartDates <- towTotals %>%
  group_by(Year) %>%
  slice_min(Date)

SBstartDates$weekNo <- isoweek(SBstartDates$Date)

ggplot(SBstartDates, aes(Julian, Year, colour = weekNo)) +
  geom_point(data=SBstartDates, aes(Julian, Year, size = Biomass)) +
  ggtitle("Scots Bay First Survey")

kbl(SBstartDates, col.names=c("Year", "Ground", "Date", "Biomass", "Month", "Day", "Julian", "Week Number"), align = "c") %>%
  kable_paper("striped", full_width = F)

SBstartDates %>% write.csv(paste0("C:/Users/herri/Herring Science Council/Science Team - Documents/HSC Data Compendium/Scots Bay Start Dates.csv"))

#Survey Start Date for German Bank

GBstartDates <-GBtowTotals %>%
  group_by(Year) %>%
  slice_min(Date)

GBstartDates$weekNo <- isoweek(GBstartDates$Date)

GBstartDates <- GBstartDates %>%
  select("Year", "Ground", "Date", "Biomass", "Month", "Day", "Julian", "weekNo")
GBstartDates$Biomass <- as.numeric(GBstartDates$Biomass)

ggplot(GBstartDates, aes(Julian, Year, colour = weekNo)) +
  geom_point(data=GBstartDates, aes(Julian, Year, size = Biomass)) +
  ggtitle("German Bank First Survey ")

kbl(GBstartDates, col.names = c("Year", "Ground", "Date", "Biomass", "Month", "Day", "Julian", "Week Number"), align = "c") %>%
  kable_paper("striped", full_width = F)

GBstartDates %>% write.csv(paste0("C:/Users/herri/Herring Science Council/Science Team - Documents/HSC Data Compendium/German Bank Start Dates.csv"))


#Plankton tow depths

SBtowDepths <- TowTimes %>%
  select(Ground = "GB")
