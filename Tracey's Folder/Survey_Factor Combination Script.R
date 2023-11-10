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
Date <- Survey_Data %>% slice_tail()
  Date <- Date[4]
  Date <- as.character(Date)
  Date <- lubridate::dmy(Date)
  Julian = yday(Date)
  
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
  
  ### Set Ground, Survey Number etc 
surv= Survey_Area
  #surv2="German Bank" #"German Bank" or "Scots Bay" as written
year= Year
surv.no=Survey_Number
  #adhoc = "FALSE" #true or false if an adhoc survey was completed (and "adhoc.csv" exists)
  #Sample = "N" #whether ("Y") or not ("N") they caught fish during this survey window
  #Tow = "Y" #whether or not plankton tow(s) were conducted

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))

TableC <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no, "tableC.csv")
  