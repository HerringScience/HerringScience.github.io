### Load these first

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
library(weathercan)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
library(rgeos)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(readxl)
library(hms)
library(measurements)
library(ggplot2)
library(maps)
library(dplyr)
library(RColorBrewer)
library(AICcmodavg)
library(datasets)
library(multcompView)

### Remove everything in environment
rm(list = ls())

### Set working directory
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")

###Load Required Documents and Data

Survey_Factors <- read_csv("surveyFactorsAll_Tracey with SSB data.csv")

Survey_Data <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv")

#Importing Data
Survey_Date <- Survey_Data %>% slice_tail()
  Survey_Date <- Survey_Date[4]
  Survey_Date <- as.character(Survey_Date)
  Survey_Date <- lubridate::dmy(Survey_Date)
  Julian = yday(Survey_Date)
  
Year <- Survey_Data %>% slice_tail()
  Year <- Year[1]
  Year <- as.character(Year)
    
Survey_Number <- Survey_Data %>% slice_tail()
  Survey_Number <- Survey_Number[7]
  Survey_Number <- as.character(Survey_Number)
  
Survey_Start <- Survey_Data %>% slice_tail()
  Survey_Start <-Survey_Start[10]
  Survey_Start <- as.POSIXct(Survey_Start$StartTime, format="%H:%M:%S")
  Survey_Start <- as_hms(Survey_Start)
  Survey_Start <- as.character(Survey_Start)  

No_of_Vessels <- Survey_Data %>% slice_tail()
  No_of_Vessels <- No_of_Vessels[11]  
  No_of_Vessels <- as.numeric(No_of_Vessels)  

Survey_Area <- Survey_Data %>% slice_tail()
  Survey_Area <- Survey_Area[5]  
  Survey_Area <- as.character(Survey_Area)
  
###Use to get Table C 
surv= Survey_Area
year= Year
surv.no=Survey_Number

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))

TableC <-read.csv("tableC.csv")

### HSC numbers
HSC_Estimate <- TableC %>% slice_head()
  HSC_Estimate <- HSC_Estimate[5]  
  HSC_Estimate <- as.numeric(HSC_Estimate)  

HSC_Turnover <- TableC %>% slice_head()
  HSC_Turnover <- HSC_Turnover[9]
  HSC_Turnover <- as.numeric(HSC_Turnover)
  
### DFO numbers
DFO_Estimate <- "NA"
DFO_Turnover_Adjusted <- "NA"

### High Tide
High_Tide <- "NA"
Tide_Difference <- "NA"
Tide_Relative <- "NA"

### Lat and Long from CTD Cast

Lat <- Survey_Data %>% slice_tail
  Lat <- Lat[47]
  Lat <- as.numeric(Lat)  

Lon <- Survey_Data %>% slice_tail  
  Lon <- Lon[48]
  Lon <- as.numeric(Lon)  

### Sunset Times
Sunset <- suncalc::getSunlightTimes(date = Survey_Date, lat = Lat, lon = Lon, keep = c("sunsetStart"), tz = "America/Halifax")
  Sunset <- as.data.frame(Sunset)
  Sunset[] <- data.frame(lapply(Sunset, as.character), stringsAsFactors = FALSE)
  Sunset$date = as.Date(Sunset$date)
  
Sunset_Date_Time <- Sunset$sunsetStart
  Sunset_Date_Time <- as.POSIXct(Sunset_Date_Time)
Sunset_Time <- as_hms(Sunset_Date_Time)
  Sunset_Time <- as.character(Sunset_Time)
  
  Survey_Start <- Survey_Data %>% slice_tail()
  Survey_Start <-Survey_Start[10]
  Survey_Start <- as.POSIXct(Survey_Start$StartTime, format="%H:%M:%S")
  Survey_Start <- as_hms(Survey_Start)
  Survey_Start <- as.character(Survey_Start)
  
Sunset_Difference <- "NA"
Sunset_Relative <- "NA"

### Set working Directory back to original forlder
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")

### Append data to Survey_Factors
Survey_Factors_Add <- data.frame((Survey_Date),
                                 (Survey_Number),
                                 Year <- c(Year),
                                 (No_of_Vessels),
                                 (Survey_Area),
                                 (HSC_Estimate),
                                 (HSC_Turnover),
                                 (DFO_Estimate),
                                 (DFO_Turnover_Adjusted),
                                 (Survey_Start),
                                 (High_Tide),
                                 (Tide_Difference),
                                 (Tide_Relative),
                                 (Julian),
                                 (Sunset_Time)
                                 )
                    