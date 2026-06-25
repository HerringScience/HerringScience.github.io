## Global options

##Packages installed

#install.packages("readxl")

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
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(measurements)
library(geodata)
library(terra)
library(readxl)


setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/"))

#All Tags deployed

Tags = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))

#Tag return files to combine

#2026

#2025

#2024

Returns5 = read_xlsx("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/Tag Returns/All Tag Returns/Connors Fish Tag Data 2024.xlsx")
  Returns5$dataorigin <- "Connors Fish Tag Data 2024.xlsx"

#2023

#2022

Returns2 = read_xlsx("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/Tag Returns/All Tag Returns/2022 - Tag returns from website or email.xlsx")
Returns2$dataorigin <- "2022 - Tag returns from website or email.xlsx"

Returns3 = read_xlsx("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/Tag Returns/All Tag Returns/Connors 2022 Fish Tag Data.xlsx")
  Returns3$dataorigin <- "Connors 2022 Fish Tag Data.xlsx"

#2021

Returns1 = read_xlsx("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/Tag Returns/All Tag Returns/2021- Tag returns from website or email.xlsx")
  Returns1$dataorigin <- "2021- Tag returns from website or email.xlsx"
Returns4 = read_xlsx("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/Tag Returns/All Tag Returns/Connors Fish Tag Data 2021.xlsx")
  Returns4$dataorigin <- "Connors Fish Tag Data 2021.xlsx"

  
#2020

#2019

#2018
Returns6 = read_xlsx("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/Tag Returns/All Tag Returns/Fish Tag Info_connors.xlsx")
  Returns6$dataorigin <- "Fish Tag Info_connors.xlsx"

#2017

#2016
  
#Multi-year
Returns7 = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/Tag Returns/All Tag Returns/Mail in Tag Returns.csv"))
  Returns7$dataorigin <- "Mail in Tag Returns.csv"


Returns8 = read
Returns9
Returns10

#Will combine all tag returns into these two folders
completeReturns = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/complete.returns.csv"))
incompleteReturns = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/Tag Returns/All Tag Returns/incomplete.returns.csv"))

# Land Data
can <- gadm(country='CAN', level=1, path = "geodata_default_path", version="latest", 
            resolution = 1, regions = c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador", "Québec"))
us  <- gadm(country='USA', level=1, path = "geodata_default_path", version="latest", 
            resolution = 1, regions = c("Maine"))

can1 <- rbind(can, us)

NBNS_spat <- can1[can1$NAME_1 %in% c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador", "Québec", "Maine"), ]

NBNS_sf <- st_as_sf(NBNS_spat)
