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

#Load Data
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Fat Data")
Connors = read_csv("Connors 2004-2017.csv")
Com1 = read_csv("Comeaus 2005-2010.csv")
Com2 = read_csv("Comeaus 2017.csv")
Com3 = read_csv("Comeaus 2018.csv")
Com4 = read_csv("Comeaus 2020.csv")
Com5 = read_csv("Comeaus 2021.csv")
Com6 = read_csv("Comeaus 2022.csv")
Scotia = read_csv("Scotia Update.csv") #manually adjusted date_time due to missing leading 0's
Unknown = read_csv("Unknown 2007-2013.csv")

#Connors QC / structuring
Connors = Connors %>% dplyr::select(Date = DATE, Vessel = BOAT, Ground = AREA, 
                                    Size4.5 = Size_4.5, Size4.5_5, Size5_6, Size6_7, Size7_8, 
                                    Size8_9, Size9_10, Size10_11, Size11_12, Size12 = Size_12, Month) %>%
  mutate(Year = substr(Date,1,4))
Connors = pivot_longer(data=Connors, cols=starts_with("Size"), names_to="Size", names_prefix = "Size", values_to="Fat") %>%
  filter(Fat < 40)
Connors$Year = as.factor(Connors$Year)
Connors$Month = as.factor(Connors$Month)

#Scotia QC / structuring
Scotia = Scotia %>% rename(Fat = Average)
Scotia$Month = as.factor(Scotia$Month)
Scotia$Year = as.factor(Scotia$Year)

#Unknown QC / structuring
Unknown = Unknown %>% dplyr::select(Date, Fat = "% Fat") %>%
  mutate(Year = substr(Date,1,4)) %>%
  mutate(Month = substr(Date,6,7))
Unknown$Year = as.factor(Unknown$Year)
Unknown$Month = as.factor(Unknown$Month)

#Comeau's QC / structuring
Com1 = Com1 %>% dplyr::select(Year, Month, Date, Vessel, Ground = Area, Type, Size, Fat)
Com2 = Com2 %>% dplyr::select(Plant, Date, Method, Ground = Harvest_Site, Vessel = Boat, Bait = bait, Small = small, Med = med, Large = large)
Com2 = pivot_longer(data=Com2, cols=c("Bait", "Small", "Med", "Large"), names_to="Size", values_to="Fat") %>% filter(Fat > 0)
Com3 = Com3 %>% dplyr::select(Plant, Date, Method, Ground = Harvest_Site, Vessel = Boat, Bait = bait, Small = small, Med = med, Large = large)
Com3 = pivot_longer(data=Com3, cols=c("Bait", "Small", "Med", "Large"), names_to="Size", values_to="Fat") %>% filter(Fat > 0)
Com4 = Com4 %>% dplyr::select(Plant, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com5 = Com5 %>% dplyr::select(Plant, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com6 = Com6 %>% dplyr::select(Plant, CollectionMethod, SampleID, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com6$Lon = as.character(Com6$Lon)
Com7 = full_join(Com6, Com5)
Com8 = full_join(Com7, Com4)
Com9 = full_join(Com8, Com3)
Com10 = full_join(Com9, Com2)
Comeau = full_join(Com10, Com1)
rm(list= c("Com1", "Com2", "Com3", "Com4", "Com5", "Com6", "Com7", "Com8", "Com9", "Com10"))
Comeau$Month = as.factor(Comeau$Month)
Comeau$Year = as.factor(Comeau$Year)

Comeau = Comeau %>%
  mutate(Supplier = "Comeaus") %>%
  rename(Date2 = Date)

Connors = Connors %>%
  mutate(Supplier = "Connors")

Scotia = Scotia %>%
  mutate(Supplier = "Scotia")

Unknown = Unknown %>%
  mutate(Supplier = "Unknown")

Master1 = full_join(Comeau, Connors)
Master2 = full_join(Master1, Scotia)
Master = full_join(Master2, Unknown)
rm(list=c("Master1", "Master2"))

Master = Master %>%
  relocate(Supplier, Year, Month, Day, Date, Date2, Vessel, Lat, Lon, Ground, 
           "FishLength(cm)", "FishWeight(g)", Size, Fat, CollectionMethod, SampleID, Method, Type, Filets, Plant)
Master$Month <- str_remove(Master$Month, "^0+")
Master$Month = as.numeric(Master$Month)
Master$Month = as.factor(Master$Month)
Master = Master %>% dplyr::filter(Fat < 40)

#Correct and consolidate Grounds
Master = Master %>%
  mutate(Ground = ifelse(Ground == "Scotts Bay", "Scots Bay",
                  ifelse(Ground == "Off Shore Banks", "Offshore Banks",
                  ifelse(Ground == "Scot's Bay", "Scots Bay",
                  ifelse(Ground == "Long Island Shore", "Long Island",
                  ifelse(Ground == "North East Bank", "NE banks",
                  ifelse(Ground == "Southwest Ground", "SW Grounds",
                  ifelse(Ground == "South of Seal Island", "Seal Island",
                  ifelse(Ground == "Northeast Bank", "NE Banks",
                  ifelse(Ground == "off Shelburne", "Shelburne",
                  ifelse(Ground == "Not Specified", NA,
                  ifelse(Ground == "NW of Horshoe", "Horseshoe",
                  ifelse(Ground == "Block Island Sd", "Block Island Sound",
                  ifelse(Ground == "Cape Cod Hi", "Cape Cod",
                  ifelse(Ground == "North Mud Hole", "Mud Hole",
                  ifelse(Ground == "Mud hole", "Mud Hole",
                  ifelse(Ground == "Georges Banks", "Georges Bank",
                  ifelse(Ground == "From NS", NA,
                  ifelse(Ground == "Nova Scotia", NA,
                  ifelse(Ground == "N.S Seine", NA,
                  ifelse(Ground == "N.S.Seine", NA,
                  ifelse(Ground == "S.W.Banks", "SW Grounds",
                  ifelse(Ground == "North East Banks", "NE Banks",
                  ifelse(Ground == "St Mary's Bay", "St Marys Bay",
                  ifelse(Ground == "Sw Banks", "SW Grounds",
                  ifelse(Ground == "German Banks", "German Bank",
                  ifelse(Ground == "USA fish", NA,
                  ifelse(Ground == "USA", NA,
                  ifelse(Ground == "Long Island Sh", "Long Island Shoal",
                  ifelse(Ground == "South West Bank", "SW Grounds",
                  ifelse(Ground == "N.E.Banks", "NE Banks",
                  ifelse(Ground == "NS Seine", NA,
                  ifelse(Ground == "USA Seine", "NA", Ground)))))))))))))))))))))))))))))))))
Master = Master %>%
  mutate(Ground = ifelse(Ground == "Scott's Bay", "Scots Bay",
                  ifelse(Ground == "Yankee Banks", "Yankee",
                  ifelse(Ground == "Scotts Bay", "Scots Bay",
                  ifelse(Ground == "Grn", NA,
                  ifelse(Ground == "Mt. Desert", "Mt Desert",
                  ifelse(Ground == "Mt.Desert rock", "Mt Desert",
                  ifelse(Ground == "Mt. Desert Rock", "Mt Desert",
                  ifelse(Ground == "Mystrey", "Mystrey Island",
                  ifelse(Ground == "Double R", NA,
                  ifelse(Ground == "Local Shut Off", "Shut Off",
                  ifelse(Ground == "Shut off", "Shut Off",
                  ifelse(Ground == "Eagel Island", "Eagle Island",
                  ifelse(Ground == "Pacific Herring", NA,
                  ifelse(Ground == "Mt.Desert Rock", "Mt Desert",
                  ifelse(Ground == "Mt Desert Rock", "Mt Desert",
                  ifelse(Ground == "Unknown", NA,
                  ifelse(Ground == "Weir", NA,
                  ifelse(Ground == "Connor's Weir", "Connors Weir",
                  ifelse(Ground == "Sw German Bank", "German Bank",
                         ifelse(Ground == "NE Bank", "NE Banks",
                                ifelse(Ground == "Scots", "Scots Bay",
                                       ifelse(Ground == "Sw of Seal Island", "Seal Island",
                                              ifelse(Ground == "West of Seal Island", "Seal Island",
                                                     Ground))))))))))))))))))))))))


# Add Lat/Lon for Grounds - general center point
unique(Master$Ground)

Master = Master %>%
  mutate(Lat = ifelse(Ground == X & is.na(Lat), NewValue, Lat)) %>% #Need to keep original lat/lon if exists
  mutate(Lon = ifelse(Ground == X & is.na(Lon), NewValue, Lon))
  
Master = Master %>%
  mutate(Lat = ifelse(Ground == "NB Coastal" & is.na(Lat), 45.095, Lat)) %>%
  mutate(Lon = ifelse(Ground == "NB Costal" & is.na(Lon), -66.476, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Grand Manan" & is.na(Lat), 44.611, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Grand Manan" & is.na(Lon), -66.6666, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Grand Manan Banks" & is.na(Lat), 44.33325, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Grand Manan Banks" & is.na(Lon), -66.9375, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Long Island" & is.na(Lat), 44.33344, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Long Island" & is.na(Lon), -66.3517, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "German Bank" & is.na(Lat), 43.3, Lat)) %>%
  mutate(Lon = ifelse(Ground == "German Bank" & is.na(Lon), -66.8334, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Seal Island" & is.na(Lat), 43.35714, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Seal Island" & is.na(Lon), -65.619, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Scots Bay" & is.na(Lat), 45, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Scots Bay" & is.na(Lon), -65.5756, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Yankee" & is.na(Lat), 44.85714, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Yankee" & is.na(Lon), -66.095, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "" & is.na(Lat), 44, Lat)) %>%
  mutate(Lon = ifelse(Ground == "" & is.na(Lon), -66, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "" & is.na(Lat), 45, Lat)) %>%
  mutate(Lon = ifelse(Ground == "" & is.na(Lon), -66, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "" & is.na(Lat), 44, Lat)) %>%
  mutate(Lon = ifelse(Ground == "" & is.na(Lon), -66, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "" & is.na(Lat), 44, Lat)) %>%
  mutate(Lon = ifelse(Ground == "" & is.na(Lon), -66, Lon)) %>%

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/")
Master %>% write_csv("Total Fat Data.csv")

### PLOTTING ###

#Comeau
ggplot(data=subset(Master, Supplier == "Comeaus"), aes(x=Year, y=Fat)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

ggplot(data=subset(Master, Supplier == "Comeaus" & !is.na(Month)), aes(x=Month, y=Fat, group=Month)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

#Connors
ggplot(data=subset(Master, Supplier == "Connors"), aes(x=Year, y=Fat)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

ggplot(data=subset(Master, Supplier == "Connors" & !is.na(Month)), aes(x=Month, y=Fat, group=Month)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

#Scotia
ggplot(data=subset(Master, Supplier == "Scotia"), aes(x=Year, y=Fat)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

ggplot(data=subset(Master, Supplier == "Scotia" & !is.na(Month)), aes(x=Month, y=Fat, group=Month)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

#Unknown
ggplot(data=subset(Master, Supplier == "Unknown"), aes(x=Year, y=Fat)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

ggplot(data=subset(Master, Supplier == "Unknown" & !is.na(Month)), aes(x=Month, y=Fat, group=Month)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

#Master-wide
ggplot(data=Master, aes(x=Year, y=Fat)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

ggplot(data=subset(Master, !is.na(Month)), aes(x=Month, y=Fat)) +
  geom_boxplot() + 
  labs(y="Fat Perecent (%)")
