# remove everything in the workspace
rm(list = ls())

# # IMPORTANT : SET GROUND, YEAR, AND SURVEY # HERE
# surv="SB" #SB or GB or SI
# surv2="Scots Bay" #"German Bank", "Seal Island" or "Scots Bay" as written
# year="2026"
# surv.no="9"
# adhoc = "false" #true or false if an adhoc survey was completed (and "adhoc.csv" exists)
# Sample = "Y" #whether ("Y") or not ("N") they caught fish during this survey window
# Tow = "N" #whether or not plankton tow(s) were conducted
# 
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
#library(weathercan) #There is new version... How do I undownload this, and redownload the newer version.
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
library(chcd)

#install.packages("chcd")

repo <- file.path(
  path.expand("~"),
  "GitHub",
  "HerringScience.github.io"
)
##CTD Data import and filtering
CTD <- read_csv(file.path(repo, "Source Data", "CTD_Raw.csv"))

CTD <- CTD %>% dplyr::select(-Pressure, -Conductivity, -Specific_conductance, -Sound_velocity, -Density, -plankton_ID)
CTD$Date = ymd(CTD$Date)
CTD <- CTD %>% mutate(Julian = yday(Date)) #add Julian day
CTD <- CTD %>% rename(Ground = ground, ID = id)

#Bio <- read_csv(file.path(repo, "Source Data", "Biomass.csv"))
Bio <- read_csv(file.path(repo, "Main Data", "SSB Estimates.csv"))
Bio$Survey_Date <- as.Date(Bio$Survey_Date)

CTD <- left_join(CTD, 
                 Bio %>% select(Survey_Date, DFO_Turnover_Adjusted), 
                 join_by(Date == Survey_Date), relationship = "many-to-many")
CTD <- CTD %>% mutate(Month = as.numeric(substr(Date, 6, 7))) #%>% dplyr::select(-Location) #may need to remove other columns
CTD$Month <- as.factor(CTD$Month)
CTD$Year <- as.factor(CTD$Year)

#imports daily ECCC historical data for GB=Yarmouth=8202000, SB=Greenwood=8202000
ECCC = get_climatedata('yarmouth a', 2017:2025, 'd')
ECCC <- ECCC %>%
  filter(!is.na(max_temp_c)) #remove duplicates to keep the one that has the most info.

ECCC2 = get_climatedata('greenwood a', 2017:2025, 'd')
ECCC <- full_join(ECCC, ECCC2)
  ECCC <- ECCC %>% rename(date = date_time)

ECCC = ECCC %>% 
  dplyr::select(station_name, date, mean_temp_c, total_precip_mm, total_snow_cm, total_rain_mm, spd_of_max_gust_km_h, 
                min_temp_c, max_temp_c, heat_deg_days_c, cool_deg_days_c) %>%
  rename(Date = date) %>%
  mutate(station_name = case_when(
    station_name == "GREENWOOD A" ~ "Scots Bay",
    station_name == "YARMOUTH A" ~ "German Bank",
    TRUE ~ station_name
  ))

ECCC <- ECCC %>%
  rename(Ground = station_name)

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

### Write all CTD files.

# CTD %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD Full.csv"))
# CTD30 %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD 30m.csv"))
# SST %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD SST.csv"))
