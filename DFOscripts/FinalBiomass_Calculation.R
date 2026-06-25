#### biomass calculation from transects exported from Echoview into excel
#Integrations must be placed in same folder and seperated (e.g. GB and SI)

rm(list = ls())

library(dplyr)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(plotKML)
library(maptools)
library(lubridate)


source("D:/4VWXHER Acoustics/BlindComparisons/Acoustic_biomass functions.R")

read.csv("2020-08-31/Biomass/")

setwd(paste0("D:/4VWXHER Acoustics/Allan 2020/German Bank/2020-08-31/Biomass/"))
calFiles = dir(calfile.path,full.names=T,pattern='.ecs$')
calFiles <- grep("Cal_2020.e", calFiles, value =TRUE, fixed =TRUE) #really odd R bug, had to code it this way.
