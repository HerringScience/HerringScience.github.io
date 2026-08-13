# remove everything in the workspace
rm(list = ls())

# IMPORTANT : SET GROUND, YEAR, AND SURVEY # HERE
surv="SB" #SB or GB or SI
surv2="Scots Bay" #"German Bank", "Seal Island" or "Scots Bay" as written
year="2026"
surv.no="6"
adhoc = "false" #true or false if an adhoc survey was completed (and "adhoc.csv" exists)
Sample = "Y" #whether ("Y") or not ("N") they caught fish during this survey window
Tow = "N" #whether or not plankton tow(s) were conducted
# 
# #(SB ONLY) Set main-box vessels
# ## (SB ONLY) OG was main-box vessels only, but then it stopped doing distance properly. Add in all vessels here.
# ids = c("BP", "FM", "LJ", "MS", "LB", "LM")
# 
#Area and TS values - From table C
SB1= 690.2649 #SB main area
SB2= 80.94466 #SB north area
SB3= 119.3215 #SB east area

GB1 = 826 #GB main area
GB2 = 274  #Seal Island area
GB3 = 0 #Ad-hoc school survey area
# 
# ##
# ###
# ##
# 
# #BELOW VALUES SHOULD RARELY CHANGE#
# TS1 = -35.5 #TS38
# 
# #turnover calculation regression values
# GB_y = 0.199392662629964
# GB_x_var = 0.528381832773883
# GB_days = 31
# 
# SB_y = 0.364102758434224
# SB_x_var = 0.436969270679439
# SB_days = 29

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
#library(rgeos)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(readxl)
library(hms)
library(measurements)
library(ggplot2)
library(patchwork)
library(scales)
library(sf)
library(terra)
library(DT)
library(dygraphs)
library(leaflet)
library(rmapshaper)
library(plotly)
library(mapproj)
library(oce) #new CTD Data package
library(pander)
library(geodata) #this is an old version, and downloaded from archive.
library(pacman)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(devtools)
library(maps)
library(dplyr)
library(sp)
library(here)

##Survey Data import and filtering

survey_dir <- here(
  "Surveys",
  year,
  paste0(surv, surv.no)
)

# setwd(paste0("C:/Users/herri/OneDrive - Herring Science Council/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))

#Plankton = read_csv("PlanktonData.csv")

#get CTD data from Plankton
SurveyData = read_csv("Plan Data.csv")
SurveyData$StartDate = as.Date(SurveyData$Date, format = "%d/%m/%Y")

if(!is.na(first(Plankton$CTD_ID))){
  CTDData = read_csv(paste0(Plankton$CTD_ID, ".csv"))
  CTDData = CTDData %>%
    dplyr::select(Pressure = "Pressure (Decibar)", Depth = "Depth (Meter)", Temperature = "Temperature (Celsius)",	Conductivity = "Conductivity (MicroSiemens per Centimeter)", Specific_conductance = "Specific conductance (MicroSiemens per Centimeter)",
                  Salinity = "Salinity (Practical Salinity Scale)", Sound_velocity = "Sound velocity (Meters per Second)", Density = "Density (Kilograms per Cubic Meter)")
  CTDData = CTDData %>%
    mutate(plankton_ID = paste0(first(Plankton$Set_Number), "/", last(Plankton$Set_Number)),
           ground = surv2,
           id = first(Plankton$CTD_ID),
           Date = first(SurveyData$StartDate),
           Lat = first(Plankton$CTD_Lat),
           Lon = first(Plankton$CTD_Lon),
           Year = first(year),
           Survey = first(surv.no))
  setwd(paste0(survey_dir))
  CTDRaw = read_csv("CTD_Raw.csv")
  CTDData$Year = as.numeric(CTDData$Year)
  CTDData$Survey = as.numeric(CTDData$Survey)
  CTDTotal = full_join(CTDRaw, CTDData)
  CTDTotal %>% write_csv("CTD_Raw.csv")
  Plankton = Plankton %>%
    mutate(AvgTemp = mean(CTDData$Temperature),
           AvgSalinity = mean(CTDData$Salinity))
}

if(is.na(first(Plankton$CTD_ID))){
  Plankton = Plankton %>%
    mutate(AvgTemp = NA,
           AvgSalinity = NA)
}

##ECHOVIEW DATA##
#Land Data
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/"))
can=readRDS("gadm36_CAN_1_sp.rds")
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]

#Land Data
# Proper coordinates for German Bank. Replaced gIntersection with crop
GBMap <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
proj4string(GBMap) <- CRS(proj4string(NBNS))
GBout <- crop(NBNS, GBMap, byid=TRUE)

# Proper coordinates for Scots Bay. eplaced gIntersection with crop
SBMap <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(SBMap) <- CRS(proj4string(NBNS))
SBout <- crop(NBNS, SBMap, byid=TRUE)

#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]
GBCTD=boxes[which(boxes$Box == "GBocean"), ]
SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")
SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
if(surv == "SB") {
  if(!is.na(PlanData$EVessel)){SUA = read.csv("polygon_SBEastern.csv")
  polyEastern = as.PolySet(SUA, projection="LL")}
  if(!is.na(PlanData$NVessel)){SUA = read.csv("polygon_SBNorthern.csv")
  polyNorthern = as.PolySet(SUA, projection="LL")}
  SUA = read.csv("polygon_SB.csv")
  polySB_main = as.PolySet(SUA, projection="LL")}

if(surv == "GB"){
  SUA = read.csv("polygon_GB.csv")
  polyGB = as.PolySet(SUA, projection="LL")
  SUA = read.csv("polygon_SI.csv")
  polySI = as.PolySet(SUA, projection="LL")}
# 
#Load functions
pathnames <- list.files(pattern="[.]R$", path=paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Functions"), full.names=TRUE)
sapply(pathnames, FUN=source)

##CTD Data import and filtering

CTD <- read_csv(paste0("C:/Users/herri/OneDrive - Herring Science Council/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv"))
CTD <- CTD %>% dplyr::select(-Pressure, -Conductivity, -Specific_conductance, -Sound_velocity, -Density, -plankton_ID)
CTD$Date = ymd(CTD$Date)
CTD <- CTD %>% mutate(Julian = yday(Date)) #add Julian day
CTD <- CTD %>% rename(Ground = ground, ID = id)
Bio <- read_csv(paste0("C:/Users/herri/OneDrive - Herring Science Council/Documents/GitHub/HerringScience.github.io/Source Data/Biomass.csv"))
CTD <- left_join(CTD, Bio, by = "Date")
CTD <- CTD %>% mutate(Month = as.numeric(substr(Date, 6, 7))) %>% dplyr::select(-Location) #may need to remove other columns
CTD$Month <- as.factor(CTD$Month)
CTD$Year <- as.factor(CTD$Year)

#imports daily ECCC historical data for GB=Yarmouth=50133, SB=Greenwood=6354
# ECCC = weather_dl(station_ids = c(50133, 6354), start = "2017-01-01", interval = "day")
# ECCC = ECCC %>%
#   dplyr::select(station_name, date, mean_temp, total_precip, total_snow, total_rain, spd_max_gust,
#                 min_temp, max_temp, heat_deg_days, cool_deg_days) %>%
#   rename(Date = date) %>%
#   mutate(Ground = ifelse(station_name == "GREENWOOD A", "Scots Bay", "German Bank"))


#weathercan no longer working. 

  CTD$station_name = NA
  CTD$mean_temp = NA
  CTD$total_precip = NA
  CTD$total_snow = NA
  CTD$total_rain = NA
  CTD$spd_max_gust = NA
  CTD$min_temp = NA
  CTD$max_temp = NA
  CTDheat_deg_days = NA
  CTD$cool_deg_days = NA

CTD = CTD %>%
  mutate(Ground = ifelse(station_name == "GREENWOOD A", "Scots Bay", "German Bank"))

#Combine with ECCC data, need to make Scots Bay = Greenwod, German Bank = Yarmouth
CTD = left_join(CTD, ECCC, by = c("Date", "Ground"))

#Cast in or out of box factor
CTD = CTD %>% 
  mutate(In_Box = ifelse(Ground == "Scots Bay" & between(Lat, 45.03, 45.08) & between(Lon, -65.3, -65.1), "1",
                         ifelse(Ground == "German Bank" & between(Lat, 43.50, 43.60) & between(Lon, -66.4, -66.3), "1", "0")))

CTD$In_Box = as.factor(CTD$In_Box)

#SST
SST = CTD %>% 
  filter(between(Depth, 0, 5)) %>%
  filter(grepl('German Bank|Scots Bay', Ground)) %>%
  group_by(Ground, Date, Year, Julian, Month, Survey, In_Box) %>%
  summarize(TempSD = sd(Temperature),
            Temperature = mean(Temperature),
            Biomass = mean(Biomass),
            logTemp = log(Temperature),
            Lat = mean(Lat),
            Lon = mean(Lon),
            logBiomass = log(Biomass),
            SalinitySD = sd(Salinity),
            Salinity = mean(Salinity),
            mean_temp = mean(mean_temp),
            total_precip = mean(total_precip),
            total_snow = mean(total_precip),
            total_rain = mean(total_rain),
            spd_max_gust = max(spd_max_gust),
            min_temp = min(min_temp),
            max_temp = max(max_temp),
            heat_deg_days = mean(heat_deg_days),
            cool_deg_days = mean(cool_deg_days))

SST = SST %>%
  group_by(Year, Month, Ground) %>%
  mutate(Count = length(Temperature))

#At-depth
CTD30 = CTD %>% 
  filter(between(Depth, 28, 32)) %>%
  filter(grepl('German Bank|Scots Bay', Ground)) %>%
  group_by(Ground, Date, Year, Julian, Month, Survey, In_Box) %>%
  summarize(TempSD = sd(Temperature),
            Temperature = mean(Temperature),
            Biomass = mean(Biomass),
            logTemp = log(Temperature),
            Lat = mean(Lat),
            Lon = mean(Lon),
            logBiomass = log(Biomass),
            SalinitySD = sd(Salinity),
            Salinity = mean(Salinity),
            mean_temp = mean(mean_temp),
            total_precip = mean(total_precip),
            total_snow = mean(total_precip),
            total_rain = mean(total_rain),
            spd_max_gust = max(spd_max_gust),
            min_temp = min(min_temp),
            max_temp = max(max_temp),
            heat_deg_days = mean(heat_deg_days),
            cool_deg_days = mean(cool_deg_days))

CTD30 = CTD30 %>%
  group_by(Year, Month, Ground) %>%
  mutate(Count = length(Temperature))

#Adding Stratification
#Take SST temps and salinity and add it to 30m dataframe (30m will always have less), mutate stratified = 30m-1m

SSTTemp = SST %>% ungroup() %>% dplyr::select(Date, Temperature, Salinity)
Strat = left_join(CTD30, SSTTemp, by = "Date")
Strat = Strat %>% rename(Temperature = Temperature.x, SST = Temperature.y, Salinity = Salinity.x, SurfaceSalinity = Salinity.y)
Strat = Strat %>% mutate(StratTemp = SST-Temperature) %>% mutate(StratSalt = Salinity-SurfaceSalinity)
Strat = Strat %>%   
  group_by(Ground, Date, Year, Julian, Month, Survey, In_Box) %>%
  summarize(TempSD = sd(Temperature),
            Temperature = mean(Temperature),
            Biomass = mean(Biomass),
            logTemp = log(Temperature),
            Lat = mean(Lat),
            Lon = mean(Lon),
            logBiomass = log(Biomass),
            SalinitySD = sd(Salinity),
            Salinity = mean(Salinity),
            mean_temp = mean(mean_temp),
            total_precip = mean(total_precip),
            total_snow = mean(total_precip),
            total_rain = mean(total_rain),
            spd_max_gust = max(spd_max_gust),
            min_temp = min(min_temp),
            max_temp = max(max_temp),
            heat_deg_days = mean(heat_deg_days),
            cool_deg_days = mean(cool_deg_days),
            SST = mean(SST),
            SurfaceSalinity = mean(SurfaceSalinity),
            StratTemp = mean(StratTemp),
            StratSalt = mean(StratSalt))

CTD30 = Strat

CTD %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD Full.csv"))
CTD30 %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD 30m.csv"))
SST %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD SST.csv"))
# 
# #Larval Data
# 
# larv = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Measurements.csv"))
# arc = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/ARC Data.csv"))
# arc = arc %>% dplyr::select(id, Larvae_Count, Notes)
# survey = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
# survey = survey %>% mutate(Ground = substr(id,1,2))
# 
# larv = left_join(larv, arc, by="id")
# larv = left_join(larv, survey)
# larv = larv %>% dplyr::select(Ground, id, Date, Survey.No, No_jars, Lengthmm, Condition, Yolk_sac, Yolk_sac_length, Preservative, ARC_Count=Larvae_Count, ARC_Notes=Notes, Lon1, Lat1, Lon2, Lat2, TowTime, AvgTowDepth, MaxTowDepth, CTDAvgTemp=AvgTemp, CTDAvgSalinity=AvgSalinity, Volume, Month, Year, Day)
# larv$Date = dmy(larv$Date)
# larv$Survey.No = as.factor(larv$Survey.No)
# larv$Year = as.factor(larv$Year)
# larv$category =  with(larv, ifelse(larv$Lengthmm < 8 , 1, 
#                                    ifelse(larv$Lengthmm < 13 & larv$Lengthmm >= 8, 2, 
#                                           ifelse(larv$Lengthmm >= 13  & larv$Lengthmm < 18, 3, 
#                                                  ifelse(larv$Lengthmm > 17 & larv$Lengthmm <= 27, 4, 5)))))
# larv$category = as.factor(larv$category)
# larv$hatchDate = larv$Date - 10 #incubation duration of 10 days
# larv$hatchDate = ymd(larv$hatchDate)
# 
# #Calculating spawn dates
# larv=larv %>% mutate(MAXspawnDate = ifelse(category == 1, hatchDate-14,
#                                            ifelse(category == 2, hatchDate-35,
#                                                   ifelse(category == 3, hatchDate-56,
#                                                          ifelse(category == 4, hatchDate-98,
#                                                                 ifelse(category == 5, hatchDate-99, "NA"))))))
# larv$MAXspawnDate=as.numeric(larv$MAXspawnDate)
# larv$MAXspawnDate=as.Date(larv$MAXspawnDate, origin = "1970-01-01")
# 
# larv=larv %>% mutate(MINspawnDate = ifelse(category == 1, hatchDate,
#                                            ifelse(category == 2, hatchDate-14,
#                                                   ifelse(category == 3, hatchDate-35,
#                                                          ifelse(category == 4, hatchDate-56,
#                                                                 ifelse(category == 5, hatchDate-98, "NA"))))))
# larv$MINspawnDate=as.numeric(larv$MINspawnDate)
# larv$MINspawnDate=as.Date(larv$MINspawnDate, origin = "1970-01-01")
# 
# #add Julian
# larv<-larv %>% mutate(Julian = yday(Date),
#                       JulianMin = yday(MINspawnDate),
#                       JulianMax = yday(MAXspawnDate))
# 
# #Calculating SE/mean/min/max of larval measurements.
# 
# # Changed group_by to (id, Year, Date) from group_by(Survey.No, Year, Date) to fix abundance to count larvae within the tow, vs abundance of larvae from the survey.
# 
# larv <- larv %>%
#   group_by(id, Year, Date) %>%
#   mutate(SD = sd(Lengthmm), 
#          MinLength = min(Lengthmm), 
#          MaxLength = max(Lengthmm), 
#          MeanLength = mean(Lengthmm), 
#          Abundance = length(Lengthmm),
#          X = ((Lon1 + Lon2)/2),
#          Y = ((Lat1 + Lat2)/2)) %>%
#   ungroup()
# 
# larvsummary <- larv %>% group_by(Ground, id, Year) %>%
#   summarize(MinLength = mean(MinLength, na.rm = TRUE), 
#             MaxLength = mean(MaxLength, na.rm = TRUE), 
#             MeanLength = mean(MeanLength, na.rm = TRUE),
#             SD = mean(SD, na.rm = TRUE),
#             Abundance = length(Lengthmm)) %>%
#   mutate(SE = SD/sqrt(Abundance))
# surveysummary = survey %>% dplyr::select(Ground, Survey.No, Year) %>% group_by(Ground, Survey.No, Year) %>% summarize(Survey.No = mean(Survey.No), Year = mean(Year))
# surveysummary$Year = as.factor(surveysummary$Year)
# surveysummary$Survey.No = as.factor(surveysummary$Survey.No)
# larvsummary = left_join(surveysummary, larvsummary)
# larvsummary %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Summary Table.csv"))
# 
# larv = larv %>%
#   mutate(Larv_per_jar = Abundance/No_jars) %>%
#   mutate(Volume = ifelse(Volume < 0.01, NA, Volume)) %>%
#   mutate(Density = Larv_per_jar/Volume)
# 
# larv = larv %>% group_by(Year) %>%
#   mutate(DayDiff = last(Julian)-Julian) %>%
#   mutate(LastLength = ifelse(DayDiff == 0, Lengthmm, ((DayDiff*0.24)+Lengthmm))) 
# 
# #join CTD + larval data, filter temp and salinity to where depth = avg tow depth +/- 0.25m
# CTDLarval = CTD %>%
#   dplyr::select(Depth, Temperature, Salinity, Ground, Year, Survey.No=Survey) %>%
#   mutate(Ground = ifelse(Ground == "Scots Bay", "SB", "GB"),
#          Survey.No = as.factor(Survey.No)) %>%
#   left_join(larv, by=c("Ground", "Year", "Survey.No")) %>%
#   group_by(Year, Survey.No, Ground, id) %>%
#   summarize(Depth, Temperature, Salinity, AvgTowDepth) %>%
#   group_by(Year, Survey.No, Ground, id) %>%
#   filter(between(Depth, min(AvgTowDepth-0.25), min(AvgTowDepth+0.25))) %>%
#   summarize(CTDTemp = mean(Temperature),
#             CTDSalinity = mean(Salinity))
# 
# larv = left_join(larv,CTDLarval)
# 
# larv = larv %>%
#   dplyr::select(Ground, id, Date, Survey.No, No_jars, Abundance, Lengthmm, category, MinLength, MaxLength, MeanLength, SD, Abundance, Larv_per_jar, Density, hatchDate, MINspawnDate, MAXspawnDate, Julian, JulianMin, JulianMax, LastLength, Day, Month, Year, Condition, Yolk_sac, Preservative, ARC_Count, ARC_Notes, X, Y, TowTime, AvgTowDepth, MaxTowDepth, CTDTemp, CTDSalinity, Volume)
# larv %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Full Larval.csv"))
