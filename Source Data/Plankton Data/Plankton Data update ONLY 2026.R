# remove everything in the workspace
rm(list = ls())

# IMPORTANT : SET GROUND, YEAR, AND SURVEY # HERE
surv="GB" #SB or GB or SI
surv2="German Bank" #"German Bank", "Seal Island" or "Scots Bay" as written
year="2026"
surv.no="1"
adhoc = "false" #true or false if an adhoc survey was completed (and "adhoc.csv" exists)
Sample = "Y" #whether ("Y") or not ("N") they caught fish during this survey window
Tow = "Y" #whether or not plankton tow(s) were conducted

# #(SB ONLY) Set main-box vessels
# ## (SB ONLY) OG was main-box vessels only, but then it stopped doing distance properly. Add in all vessels here.
# ids = c("LB", "FM", "LJ")
# 
# #Area and TS values - From table C
# SB1= 675.4278 #SB main area
# SB2= 80.94466 #SB north area
# SB3= 119.3215 #SB east area
# 
# GB1 = 826 #GB main area
# GB2 = 274  #Seal Island area
# GB3 = 0 #Ad-hoc school survey area
# 
# ##
###
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
# 
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

repo <- file.path(
  path.expand("~"),
  "GitHub",
  "HerringScience.github.io"
)
##Survey Data import and filtering

setwd(file.path(repo, 
                 "Surveys",
                 year,
                 paste0(surv, surv.no)
                ))
  
Plankton = read_csv(file.path(repo, 
                              "Surveys",
                              year,
                              paste0(surv, surv.no),
                              "PlanktonData.csv"))
                    
Plankton = Plankton %>%
  mutate(Year = year,
         Ground = surv,
         Survey.No = surv.no)
if(Tow == "Y") {Plankton$TowTime = difftime(Plankton$Time2, Plankton$Time1, units = "mins")}
if(Tow == "N") {Plankton$TowTime = 0}
Plankton$Year = as.numeric(Plankton$Year)
Plankton$Swell = as.character(Plankton$Swell)
if(Sample == "Y"){Plankton$Sample = "Y"}
if(Sample == "N"){Plankton$Sample = "N"}

#get CTD data from Plankton
SurveyData = read_csv(file.path(repo, 
                                "Surveys",
                                year,
                                paste0(surv, surv.no),
                                "Plan Data.csv"))

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
 # setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data"))
  CTDRaw = read_csv(file.path(repo, 
                              "Source Data",
                              "CTD_Raw.csv"))
                    
  CTDData$Year = as.numeric(CTDData$Year)
  CTDData$Survey = as.numeric(CTDData$Survey)
  CTDTotal = full_join(CTDRaw, CTDData)
  #CTDTotal %>% write_csv("CTD_Raw.csv")
  Plankton = Plankton %>%
    mutate(AvgTemp = mean(CTDData$Temperature),
           AvgSalinity = mean(CTDData$Salinity))
}

if(is.na(first(Plankton$CTD_ID))){
  Plankton = Plankton %>%
    mutate(AvgTemp = NA,
           AvgSalinity = NA)
}

#get Ruskin data
if(Tow == "Y"){
  TowData = readxl::read_excel(file.path(repo,
                               "Surveys",
                               year,
                               paste0(surv, surv.no),
                               "Ruskin.xlsx"), skip = 1, sheet = 'Data')
    
#    read_excel(path = paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no, "/Ruskin.xlsx"), skip = 1, sheet = 'Data')
  TowData$DateTime = TowData$Time
  
  #Changed to UTC  
  
  TowData$DateTime = TowData$DateTime-hours(3) #Summer daylight savings
  #TowData$DateTime = TowData$DateTime-hours(4) #Fall daylight savings
  TowData$Date = substr(TowData$DateTime,1,10)
  TowData$Time = substr(TowData$DateTime,12,19)
  TowData$Time = hms::as_hms(TowData$Time)
  
  #Filter tow data based on Time1 + Time
  Tow1 = TowData[TowData$Time >= first(Plankton$Time1) & TowData$Time <= first(Plankton$Time2),]
  Tow2 = TowData[TowData$Time >= last(Plankton$Time1) & TowData$Time <= last(Plankton$Time2),]
  
  #Calculate average and max tow depths for each
  Tow1 = Tow1 %>%
    mutate(AvgTowDepth = mean(Depth),
           MaxTowDepth = max(Depth),
           Tow_No = 1)
  
  Tow2 = Tow2 %>%
    mutate(AvgTowDepth = mean(Depth),
           MaxTowDepth = max(Depth),
           Tow_No = 2)
  
  #ggplot the profiles for each tow
  setwd(file.path(repo,
                  "Surveys",
                  year,
                  paste0(surv, surv.no)
                  ))
    
   # paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Surveys/", year, "/", surv, surv.no))
  
  Tow1 %>%
    ggplot(aes(x = Time, y = Depth)) +
    geom_path(linewidth = 1, colour = "blue") +
    scale_y_reverse() +
    labs(y = "Depth (m)")
  ggsave("Tow 1.jpg")
  
  Tow2 %>%
    ggplot(aes(x = Time, y = Depth)) +
    geom_path(linewidth = 1, colour = "blue") +
    scale_y_reverse() +
    labs(y = "Depth (m)")
  ggsave("Tow 2.jpg")
  
  #combine the avg and max values with plankton sheet
  TowTbl1 = tibble_row(Tow_No = 1, AvgTowDepth = first(Tow1$AvgTowDepth), MaxTowDepth = first(Tow1$MaxTowDepth))
  TowTbl2 = tibble_row(Tow_No = 2, AvgTowDepth = first(Tow2$AvgTowDepth), MaxTowDepth = first(Tow2$MaxTowDepth))
  TowCalcs = full_join(TowTbl1, TowTbl2)
  
  
  Plankton = full_join(Plankton, TowCalcs, by = "Tow_No")
  
  #convert lat/lon before combining
  Plankton$Lon1 = as.numeric(conv_unit(Plankton$Lon1, "deg_dec_min", "dec_deg"))*-1
  Plankton$Lon2 = as.numeric(conv_unit(Plankton$Lon2, "deg_dec_min", "dec_deg"))*-1
  Plankton$Lat1 = as.numeric(conv_unit(Plankton$Lat1, "deg_dec_min", "dec_deg"))
  Plankton$Lat2 = as.numeric(conv_unit(Plankton$Lat2, "deg_dec_min", "dec_deg"))
  
  Plankton = Plankton %>%
    rename(id = Set_Number)
  
  PlanData = read_csv(file.path(repo, 
                                "Surveys",
                                year,
                                paste0(surv, surv.no),
                                "Plan Data.csv"))
    
  PlanData$Survey.No = as.character(PlanData$Survey.No)
  
  SurveyTotal = full_join(Plankton, PlanData)
  SurveyTotal = SurveyTotal %>%
    rename(Observer = Observers)
  
  setwd(file.path(repo, 
                  "Source Data"
  ))
  
  Survey = read_csv("planktonsamplingData.csv")
  
  SurveyTotal$TowTime = as.numeric(SurveyTotal$TowTime)
  
  Total = full_join(Survey, SurveyTotal)
  
  Total = Total %>%
    mutate(Day = as.numeric(substr(Date, 1, 2)),
           Month = as.numeric(substr(Date, 4, 5)),
           Year = as.numeric(substr(Date, 7, 10)),
           TowTime = difftime(Time2, Time1, units ="mins"),
           NoRevs = FlowReading2-FlowReading1,
           DistanceCalc = NoRevs*26873/1000000,
           Volume = DistanceCalc*3.1415) ## Not importing properly.
  
  Total = Total %>%
    dplyr::select(Year, Month, Day, Date, Ground, id, Survey.No, Fishing, Sample, 
                  StartTime, Vessel.No, ExtraBox, EVessel, NVessel, PlanktonVessel, 
                  Tow_No, Time1, Time2, TowTime, Lat1, Lon1, Lat2, Lon2, No_jars, 
                  Speed, Heading, TideDirection, Swell, WindDirection, WindSpeed, 
                  AirTemp, Observer, Net, Gear, TowType, FlowmeterType, FlowReading1, 
                  FlowReading2, NoRevs, DistanceCalc, Volume, AvgTowDepth, MaxTowDepth, 
                  DiscDepthD, DiscDepthA, CTD_ID, CTD_Lat, CTD_Lon, AvgTemp, AvgSalinity, 
                  SurfaceTemp, WaterDepth1, WaterDepth2)
  
  #Total %>% write_csv("planktonsamplingData.csv")
  setwd(file.path(repo, 
                  "Main Data"))
  
  # Total %>% write_csv("Survey Data.csv")
}

#### Need to fix Volume ####

if(Tow == "N"){
  Survey = read_csv(file.path(repo, 
                              "Source Data",
                              "planktonsamplingData.csv"))
  
  Total = read_csv(file.path(repo, 
                             "Main Data",
                             "Survey Data.csv"))
    

  PlanData = read_csv(file.path(repo, 
                                "Surveys",
                                year,
                                paste0(surv, surv.no),
                                "Plan Data.csv"))
    
  PlanData$Survey.No = as.character(PlanData$Survey.No)
  #Sample = Did they catch fish within the survey window. 
  PlanData$Sample = as.character(Sample)
  
  #To set the plankton tow data to NA since a plankton tow did not happen.
  PlanData$FlowmeterType = "General Oceanics"
  PlanData$TowType = "Surface Tow"
  PlanData$Gear = "1/500"
  PlanData$Net = "1"
  Total = full_join(Total, PlanData)
  Total = Total %>%
    mutate(Day = as.numeric(substr(Date, 1, 2)),
           Month = as.numeric(substr(Date, 4, 5)),
           Year = as.numeric(substr(Date, 7, 10)))
  
  Survey = full_join(Survey, PlanData)
  Survey = Survey %>%
    mutate(Day = as.numeric(substr(Date, 1, 2)),
           Month = as.numeric(substr(Date, 4, 5)),
           Year = as.numeric(substr(Date, 7, 10)))
  
  
 # write_csv(Total, paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
#  write_csv(Survey, paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Source Data/planktonsamplingData.csv"))
  
  
  setwd(file.path(repo, 
                  "Surveys",
                  year,
                  paste0(surv, surv.no)))
  
  Depth=1
  Time=1
  Tow1 = tibble(Depth, Time)
  Tow1 %>%
    ggplot(aes(x = Time, y = Depth)) +
    scale_y_reverse() +
    labs(y = "Depth (m)")
  ggsave("Tow 1.jpg")
  ggsave("Tow 2.jpg")
}