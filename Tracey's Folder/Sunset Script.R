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
date = "2024-01-03"
Ground="SI" #SB or GB or SI
Survey.No="7"
Survey_Type = "Structured" # Structured or Adhoc
Counted = "Yes" #Biomass counted to the total SSB number. Surveys should be minimum 10 days aprt, largest biomass number counted.
Comments = "NA" #Input any comments about the survey here. Example- The vessel Morning Star 


### Code below ###

#If statements for ground coordinates

if(Ground =="SB"){Lat = "45 03 00"
                  Lon = "65 13 80"}

if(Ground == "GB"| Ground == "SI") {Lat = "43 33 89" 
                                    Lon = "66 21 48"}

#SunsetStart <- read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/surveyFactorsAll_Tracey with SSB data.csv"))
SSB <- read_csv(paste0("C:/Users/herri/Desktop/Tracey Local SSB Spreadsheet.csv"))
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")
SurveyData <- read_csv(paste0("C:/Users/herri/Desktop/Survey Data copy.csv"))
  SurveyData <- SurveyData %>% slice_tail()
  
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
SSB <- as.data.frame(SSB)
Sunset[] <- data.frame(lapply(Sunset, as.character), stringsAsFactors = FALSE)
Sunset$date = as.Date(Sunset$date)

SunsetStartOnly <- Sunset$sunsetStart
SunsetStartOnly <- as.POSIXct(SunsetStartOnly)

#Pulling values from Survey Data

Vessel.No <- SurveyData$Vessel.No
Survey_Start <- as.character(SurveyData$StartTime)
format <- "%Y-%m-%d %H:%M:%S"
SurveyStarted <- as.POSIXct(paste(date, Survey_Start), format=format)

Sunset_Time <- as.numeric(SunsetStartOnly, "%H:%M:%S")
       


#Bind Data
SSB <- full_join(SSB, (data.frame(Survey_Date = date,
                                  Survey.No = Survey.No,
                                  Ground = Ground,
                                  Vessel.No = Vessel.No,
                                  Counted = Counted,
                                  Year = as.double(Year),
                                  Survey_Type = Survey_Type,
                                  Survey_Start = Survey_Start,
                                  SurveyStarted = SurveyStarted,
                                  Sunset_Time = Sunset_Time,
                                  sunsetStart = as.character(SunsetStartOnly),
                                  Comments = Comments)))
SSB <- SSB %>% mutate(id = row_number())
