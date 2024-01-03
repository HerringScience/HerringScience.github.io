# Load the libraries first

library(data.table)
library(suncalc)
library(rlang)
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
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(readxl)
library(hms)
library(measurements)
library(tibble)
library(zoo)
library(gtools)

Sys.setenv(tz = "America/Halifax")


# remove everything in the workspace
rm(list = ls())


#Set information here
Lat = "45 03 54" #Degree-Min-Sec format from the boat but only the numbers written with spaces (e.g. "44 16 23")
Lon = "65 14 93"
date = "2024-01-03"

### Code below ###

#SunsetStart <- read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/surveyFactorsAll_Tracey with SSB data.csv"))
SunsetStart <- read_csv(paste0("C:/Users/herri/Desktop/Tracey Local SSB Spreadsheet.csv"))
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")
SurveyData <- read_csv(paste0("C:/Users/herri/Desktop/Survey Data copy.csv"))

#Lat and Lon Converter
Lon = conv_unit(Lon,"deg_min_sec","dec_deg")
Lon = as.numeric(Lon)
Lat = conv_unit(Lat, "deg_min_sec", "dec_deg")
Lat = as.numeric(Lat)
Lon = -1*Lon

#Calendar date Converter
date = as.Date(as.POSIXct(date))
julian = yday(date)
Year = format(date, "%Y")


#Data Table of Sunset times - Current Data
Sunset <- suncalc::getSunlightTimes(date = date, lat = Lat, lon = Lon, keep = c("sunsetStart"), tz = "America/Halifax")
Sunset <- as.data.frame(Sunset)
SunsetStart <- as.data.frame(SunsetStart)
Sunset[] <- data.frame(lapply(Sunset, as.character), stringsAsFactors = FALSE)
Sunset$date = as.Date(Sunset$date)

SunsetStartOnly <- Sunset$sunsetStart
SunsetStartOnly <- as.POSIXct(SunsetStartOnly)

#Bind Data
SunsetStart <- full_join(SunsetStart, (data.frame(Survey_Date = date,
                                                 Year = as.double(Year),
                                                 sunsetStart = as.character(SunsetStartOnly),
                                                 Comments = "NA")))
SunsetStart <- SunsetStart %>% mutate(id = row_number())
