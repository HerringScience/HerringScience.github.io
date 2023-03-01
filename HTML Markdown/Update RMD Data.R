# remove everything in the workspace
rm(list = ls())

# IMPORTANT : SET GROUND, YEAR, AND SURVEY # HERE
surv="SB"
surv2="Scots Bay"
year="2022"
surv.no="6"

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

##Tagging Data
#Data import and filtering
Tag <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))
Tag$Date = ymd(Tag$Date) 
Tag <- Tag %>% mutate(Julian = yday(Date)) #add Julian day
Tag <- Tag %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
Tag = Tag %>% dplyr::select(-Tag_Annual)

#Add tags per year per tagger
Tag_Annual = Tag %>%
  group_by(Tagger) %>%
  mutate(count = n_distinct(Year)) %>%
  summarize(n=n(),
            count2 = mean(count)) %>%
  mutate(Tag_Annual = n/count2)

Tag_Annual = Tag_Annual %>%
  dplyr::select(-n, -count2)

Tag = left_join(Tag, Tag_Annual, by = "Tagger")

#Adding Ground
Tag <- Tag %>% 
  mutate(Ground = ifelse(between(Lat, 45.02, 45.4) & between(Lon, -65.5, -64.5), "Scots Bay", 
                         ifelse(between(Lat, 43.15, 43.7) & between(Lon, -66.75, -66.05), "German Bank", 'Other')))

#Change any 'Sealife' to 'Sealife II', and other corrections
Tag$Vessel = as.factor(Tag$Vessel)
summary(Tag$Vessel) #spot any issues
Tag$Vessel[which(Tag$Vessel=="Sealife")] <- "Sealife II"
Tag$Vessel[which(Tag$Vessel=="Lady Meliss")] <- "Lady Melissa"
Tag$Vessel[which(Tag$Vessel=="Lady Janice II")] <- "Lady Janice"
summary(Tag$Vessel) #double check

Tag %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/TaggingEvents.csv"))

##Performance data import and filtering
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/HTML Markdown/Surveys/", year, "/", surv, surv.no))
actual = list.files(pattern = "*tableA.csv") %>% map_df(~read_csv(.))
actual = actual %>% mutate(Type = "Actual")
plan = list.files(pattern = "*plan.csv") %>% map_df(~read_csv(.))
plan = plan %>% mutate(Type = "Plan")
wd = getwd()
Perform = full_join(actual, plan) %>% mutate(Survey = substr(wd, 60,60)) %>% mutate(Location = substr(wd, 58,59))
Perform = Perform %>% rename(End.Lat="End Lat", End.Lon="End Lon", Start.Lat="Start Lat", Start.Lon="Start Lon", Dist..km.="Dist (km)", Date.Time.Start="Date Time Start", Date.Time.End="Date Time End", Transect.No.="Transect No.")
Perform = Perform %>% mutate(Distance = distHaversine(cbind(Start.Lon,Start.Lat), cbind(End.Lon,End.Lat))) %>% mutate(Distance = Distance/1000)
Perform = Perform %>% mutate(Distance = ifelse(is.na(Dist..km.), Distance, Dist..km.))

#calculate time/speed
Perform<-Perform %>% mutate(Start=as.POSIXct(Date.Time.Start, origin = "1970-01-01")) %>% 
  mutate(End=as.POSIXct(Date.Time.End, origin = "1970-01-01")) %>%
  mutate(Duration = as.numeric(End-Start)*60) %>%
  mutate(Speed = (Distance*1000)/(Duration))
Perform<-Perform %>% mutate(Speed = Speed*1.94384) #convert from m/s to knots
Perform<-Perform %>% mutate(Year = as.numeric(substr(Start, 1, 4)))
Perform<-Perform %>% mutate(Date = date(Start)) 

#summarize speed by Transect 1 + 2 (remove any box runners)
Speed<-Perform %>%
  dplyr::group_by(Vessel, Transect.No.) %>%
  dplyr::summarize(Speed = mean(Speed, na.rm = TRUE)) %>%
  filter(!Speed == "NaN") %>%
  mutate(Transect = as.factor(Transect.No.))

#group by survey + vessel + type, summarize distance
Distance<-Perform %>% 
  dplyr::group_by(Vessel, Type) %>%
  dplyr::summarize(Distance = sum(Distance)) %>%
  spread(Type, Distance, fill = 0) %>%
  transmute(Vessel, Difference = Actual-Plan)

Speed %>% write_csv("Speed.csv")
Distance %>% write_csv("Distance.csv")
Perform %>% write_csv("Performance Total.csv")

##Survey Data import and filtering
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/")
Survey = read_csv("planktonsamplingData.csv")
Survey = Survey %>% dplyr::select(Ground, id, Survey.No, Date, StartTime, Sample, Vessel.No, ExtraBox, EVessel, NVessel, PlanktonVessel, Lon1, Lat1, Lon2, Lat2, Time1, Time2, TowTime, AirTemp, Speed, Heading, TideDirection, AvgTowDepth, MaxTowDepth, CTD_ID, AvgTemp, AvgSalinity, WindDirection, WindSpeed, Swell, FlowReading1, FlowReading2, NoRevs, DistanceCalc, Volume, DepthDiscD, DepthDiscA) 
Survey = Survey %>%
  mutate(Month = as.numeric(substr(Date, 1, 2)),
         Year = as.numeric(substr(Date, 7, 10)),
         Day = as.numeric(substr(Date, 4, 5)))

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/"))
Survey %>% write_csv("Survey Data.csv")

##CTD Data import and filtering
CTD <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv"))
CTD <- CTD %>% dplyr::select(-Pressure, -Conductivity, -Specific_conductance, -Sound_velocity, -Density, -plankton_ID)
CTD$Date = ymd(CTD$Date)
CTD <- CTD %>% mutate(Julian = yday(Date)) #add Julian day
CTD <- CTD %>% rename(Ground = ground, ID = id)
Bio <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Biomass.csv"))
CTD <- left_join(CTD, Bio, by = "Date")
CTD <- CTD %>% mutate(Month = as.numeric(substr(Date, 6, 7))) %>% dplyr::select(-Location) #may need to remove other columns
CTD$Month <- as.factor(CTD$Month)
CTD$Year <- as.factor(CTD$Year)

#imports daily ECCC historical data for GB=Yarmouth=50133, SB=Greenwood=6354
ECCC = weather_dl(station_ids = c(50133, 6354), start = "2017-01-01", interval = "day")
ECCC = ECCC %>% 
  dplyr::select(station_name, date, mean_temp, total_precip, total_snow, total_rain, spd_max_gust, 
                min_temp, max_temp, heat_deg_days, cool_deg_days) %>%
  rename(Date = date) %>%
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
