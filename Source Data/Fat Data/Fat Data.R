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
setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Source Data/Fat Data"))
Connors = read_csv("Connors 2004-2017.csv")
Com1 = read_csv("Comeaus 2005-2010.csv")
Com2 = read_csv("Comeaus 2017.csv")
Com3 = read_csv("Comeaus 2018.csv")
Com4 = read_csv("Comeaus 2020.csv")
Com5 = read_csv("Comeaus 2021.csv")
Com6 = read_csv("Comeaus 2022.csv")
Scotia = read_csv("Scotia Update.csv") #manually adjusted date_time due to missing leading 0's
Maine = read_csv("Maine 2007-2013.csv")

#Connors QC / structuring
Connors = Connors %>% dplyr::select(Date = DATE, Vessel = BOAT, Ground = AREA, 
                                    Size4.5 = Size_4.5, Size4.5_5, Size5_6, Size6_7, Size7_8, 
                                    Size8_9, Size9_10, Size10_11, Size11_12, Size12 = Size_12, Month) %>%
  mutate(Year = as.numeric(substr(Date,1,4)))
Connors = pivot_longer(data=Connors, cols=starts_with("Size"), names_to="Size", names_prefix = "Size", values_to="Fat")

#Scotia QC / structuring
Scotia = Scotia %>% rename(Fat = Average)
Scotia$Date = ymd(Scotia$Date)

#Maine QC / structuring
Maine = Maine %>% dplyr::select(Date, Fat = "% Fat") %>%
  mutate(Year = as.numeric(substr(Date,1,4))) %>%
  mutate(Month = as.numeric(substr(Date,6,7)))

#Comeau's QC / structuring
Com1 = Com1 %>% dplyr::select(Year, Month, Date, Vessel, Ground = Area, Type, Size, Fat)
Com2 = Com2 %>% dplyr::select(Plant, Date, Method, Ground = Harvest_Site, Vessel = Boat, Bait = bait, Small = small, Med = med, Large = large)
Com2 = pivot_longer(data=Com2, cols=c("Bait", "Small", "Med", "Large"), names_to="Size", values_to="Fat") %>% filter(Fat > 0)
Com3 = Com3 %>% dplyr::select(Plant, Date, Method, Ground = Harvest_Site, Vessel = Boat, Bait = bait, Small = small, Med = med, Large = large)
Com3 = pivot_longer(data=Com3, cols=c("Bait", "Small", "Med", "Large"), names_to="Size", values_to="Fat") %>% filter(Fat > 0)
Com4 = Com4 %>% dplyr::select(Plant, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com5 = Com5 %>% dplyr::select(Plant, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com6 = Com6 %>% dplyr::select(Plant, CollectionMethod, SampleID, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com7 = full_join(Com6, Com5)
Com8 = full_join(Com7, Com4)
Com9 = full_join(Com8, Com3)
Com10 = full_join(Com9, Com2)
Comeau = full_join(Com10, Com1)
rm(list= c("Com1", "Com2", "Com3", "Com4", "Com5", "Com6", "Com7", "Com8", "Com9", "Com10"))
Comeau = Comeau %>%
  mutate(Year = as.numeric(substr(Date,1,4)))

Comeau = Comeau %>%
  mutate(Supplier = "Comeaus")
Connors = Connors %>%
  mutate(Supplier = "Connors")
Scotia = Scotia %>%
  mutate(Supplier = "Scotia")
Maine = Maine %>%
  mutate(Supplier = "Maine")

Master1 = full_join(Comeau, Connors)
Master2 = full_join(Master1, Scotia)
Master = full_join(Master2, Maine)
rm(list=c("Master1", "Master2"))

Master = Master %>%
  relocate(Supplier, Year, Month, Day, Date, Vessel, Lat, Lon, Ground, 
           "FishLength(cm)", "FishWeight(g)", Size, Fat, CollectionMethod, SampleID, Method, Type, Filets, Plant) %>%
  dplyr::filter(Fat < 40)

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
                  ifelse(Ground == "Block Island Sd", "Block Island",
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
                  ifelse(Ground == "Area 10", "Murphys Weir",
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
                                                     Ground)))))))))))))))))))))))))

Master = Master %>%
  mutate(Ground = ifelse(Ground == "Deadman's", "Deadmans",
                  ifelse(Ground == "North East  Banks", "NE Banks",
                  ifelse(Ground == "NE banks", "NE Banks",
                  ifelse(Ground == "North West Banks", "NW Banks",
                  ifelse(Ground == "South West Banks", "SW Grounds",
                  ifelse(Ground == "SW of Seal Island", "Seal Island",
                  ifelse(Ground == "SW German Bank", "German Bank",
                  ifelse(Ground == "SW Banks", "SW Grounds",
                  ifelse(Ground == "Tear Drop", "Teardrop",
                  ifelse(Ground == "Patch", "The Patch",
                  ifelse(Ground == "Winner Weir", "Winner",
                  ifelse(Ground == "East Mud Hole", "Mud Hole",
                  ifelse(Ground == "Pt. Judith", "Point Judith",
                  ifelse(Ground == "Block Island Sound", "Block Island",
                  ifelse(Ground == "Kettle Ground", "Kettle Bottom",
                  ifelse(Ground == "Montauk Point", "Montauk",
                  ifelse(Ground == "Narragansett Beach", "Narragansett",
                  ifelse(Ground == "Pilchard Sardine", "Pilchard",
                  ifelse(Ground == "St. Marys Bay", "St Marys Bay",
                  ifelse(Ground == "Trucks", "Tuckers Cove",
                  ifelse(Ground == "Tuckers", "Tuckers Cove",
                  ifelse(Ground == "Tom's River", "Toms River",
                  ifelse(Ground == "Mud Hole", "Hudson Canyon",
                  ifelse(Ground == "One Mike", NA,
                  ifelse(Ground == "Connors Weir", NA,
                  ifelse(Ground == "East of Jauquau", NA,
                  ifelse(Ground == "Perry Shore", NA,
                  ifelse(Ground == "Head of Bay", NA,
                  ifelse(Ground == "NA", NA,
                  Ground))))))))))))))))))))))))))))))

# Add Lat/Lon for Grounds - general center point
#sort(unique(Master$Ground))

# Master = Master %>%
#  mutate(Lat = ifelse(Ground == X & is.na(Lat), NewValue, Lat)) %>% #Need to keep original lat/lon if exists
#  mutate(Lon = ifelse(Ground == X & is.na(Lon), NewValue, Lon))
  
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
  
  mutate(Lat = ifelse(Ground == "Trinity" & is.na(Lat), 44.0334, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Trinity" & is.na(Lon), -66.3, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Browns Bank" & is.na(Lat), 42.4998, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Browns Bank" & is.na(Lon), -66.8998, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "SW Grounds" & is.na(Lat), 43.02385714, Lat)) %>%
  mutate(Lon = ifelse(Ground == "SW Grounds" & is.na(Lon), -66.45242857, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Lurcher" & is.na(Lat), 43.85714286, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Lurcher" & is.na(Lon), -66.88085714, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Gannet Dry Ledge" & is.na(Lat), 43.69042857, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Gannet Dry Ledge" & is.na(Lon), -66.52385714, Lon)) %>% 
  
  mutate(Lat = ifelse(Ground == "Back Bay" & is.na(Lat), 45.04816667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Back Bay" & is.na(Lon), -66.854, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Blacks Harbour" & is.na(Lat), 45.052645, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Blacks Harbour" & is.na(Lon), -66.793285, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Blisses" & is.na(Lat), 45.01983333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Blisses" & is.na(Lon), -66.8445, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Bradfords Cove" & is.na(Lat), 44.60584, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Bradfords Cove" & is.na(Lon), -66.94, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Campobello" & is.na(Lat), 44.89316667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Campobello" & is.na(Lon), -66.959, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Chance Harbour" & is.na(Lat), 45.131, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Chance Harbour" & is.na(Lon), -66.3485, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Chattis Point" & is.na(Lat), 45.016, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Chattis Point" & is.na(Lon), -66.9, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Cora Bell" & is.na(Lat), 44.7564, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Cora Bell" & is.na(Lon), -66.7342, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Crow Harbour" & is.na(Lat), 45.10216667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Crow Harbour" & is.na(Lon), -66.61083333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Crow Island" & is.na(Lat), 45.04283333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Crow Island" & is.na(Lon), -66.88016667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Curry Cove" & is.na(Lat), 44.92866667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Curry Cove" & is.na(Lon), -66.9365, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Deadmans" & is.na(Lat), 45.04983333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Deadmans" & is.na(Lon), -66.771, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Deep Cove" & is.na(Lat), 45.11933333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Deep Cove" & is.na(Lon), -66.52616667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Digdeguash Basin" & is.na(Lat), 45.19116667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Digdeguash Basin" & is.na(Lon), -66.95966667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Fairhaven" & is.na(Lat), 44.96583333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Fairhaven" & is.na(Lon), -67.01016667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Friers Bay" & is.na(Lat), 44.8865, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Friers Bay" & is.na(Lon), -66.95283333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Fryes Island" & is.na(Lat), 45.06583333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Fryes Island" & is.na(Lon), -66.8365, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Herring Cove" & is.na(Lat), 44.87466667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Herring Cove" & is.na(Lon), -66.92183333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Indian Island" & is.na(Lat), 44.932, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Indian Island" & is.na(Lon), -66.966, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Iron Lady" & is.na(Lat), 44.75683333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Iron Lady" & is.na(Lon), -66.75, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Lawrence Cove" & is.na(Lat), 45.00183333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Lawrence Cove" & is.na(Lon), -66.9485, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Leonardville" & is.na(Lat), 44.97483333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Leonardville" & is.na(Lon), -66.9515, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Lepreau" & is.na(Lat), 45.14533333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Lepreau" & is.na(Lon), -66.485, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Lords Cove" & is.na(Lat), 45.00183333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Lords Cove" & is.na(Lon), -66.9485, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Meadow Brook" & is.na(Lat), 44.88083333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Meadow Brook" & is.na(Lon), -66.9125, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Mill Cove" & is.na(Lat), 44.93033333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Mill Cove" & is.na(Lon), -66.90466667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Money Cove" & is.na(Lat), 44.777, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Money Cove" & is.na(Lon), -66.82, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Mumps" & is.na(Lat), 44.67364, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Mumps" & is.na(Lon), -66.686615, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "New River" & is.na(Lat), 45.1315, Lat)) %>%
  mutate(Lon = ifelse(Ground == "New River" & is.na(Lon), -66.53383333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Oak Bay" & is.na(Lat), 45.22416667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Oak Bay" & is.na(Lon), -67.17916667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Patridge Island" & is.na(Lat), 45.2375, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Patridge Island" & is.na(Lon), -66.052, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Pipe Dream" & is.na(Lat), 44.74283333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Pipe Dream" & is.na(Lon), -66.75, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Red Head" & is.na(Lat), 45.10633333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Red Head" & is.na(Lon), -66.6005, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Round Meadow" & is.na(Lat), 45.10566667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Round Meadow" & is.na(Lon), -66.49716667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Sand Beach" & is.na(Lat), 45.00616667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Sand Beach" & is.na(Lon), -66.9, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Sandy Cove" & is.na(Lat), 45.07083333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Sandy Cove" & is.na(Lon), -66.68116667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Schooner Cove" & is.na(Lat), 44.9, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Schooner Cove" & is.na(Lon), -66.902, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Seelys Basin" & is.na(Lat), 45.07166667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Seelys Basin" & is.na(Lon), -66.67883333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Seelys Cove" & is.na(Lat), 45.08633333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Seelys Cove" & is.na(Lon), -66.65033333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Seelys Head" & is.na(Lat), 45.04533333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Seelys Head" & is.na(Lon), -66.78, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Ship Beach" & is.na(Lat), 44.97816667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Ship Beach" & is.na(Lon), -66.69433333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Spectacle" & is.na(Lat), 44.99033333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Spectacle" & is.na(Lon), -66.91533333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Spider Cove" & is.na(Lat), 45.02916667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Spider Cove" & is.na(Lon), -66.827, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Spruce Cove" & is.na(Lat), 45.02566667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Spruce Cove" & is.na(Lon), -66.86116667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Tuckers Cove" & is.na(Lat), 45.01716667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Tuckers Cove" & is.na(Lon), -66.84833333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Whale Cove" & is.na(Lat), 44.77283333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Whale Cove" & is.na(Lon), -66.758, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Winner" & is.na(Lat), 44.77433333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Winer" & is.na(Lon), -66.76016667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Wolves" & is.na(Lat), 44.95533333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Wolves" & is.na(Lon), -66.72105556, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Beaver Harbour" & is.na(Lat), 45.0456118, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Beaver Harbour" & is.na(Lon), -66.73253046, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Block Island" & is.na(Lat), 41.16518921, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Block Island" & is.na(Lon), -71.51952191, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Cape Cod" & is.na(Lat), 41.93737305, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Cape Cod" & is.na(Lon), -70.31897133, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Cape Cod Island" & is.na(Lat), 41.93737305, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Cape Cod Island" & is.na(Lon), -70.31897133, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Caraquet" & is.na(Lat), 47.95945882, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Caraquet" & is.na(Lon), -64.97430071, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Deer Island" & is.na(Lat), 44.12778656, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Deer Island" & is.na(Lon), -68.57367468, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Dipper Harbour" & is.na(Lat), 45.08319767, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Dipper Harbour" & is.na(Lon), -66.38234915, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Eagle Island" & is.na(Lat), 45.03, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Eagle Island" & is.na(Lon), -66.87, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Emerald Basin" & is.na(Lat), 43.93238838, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Emerald Basin" & is.na(Lon), -63.56728998, Lon)) %>%  
  
  mutate(Lat = ifelse(Ground == "Georges Bank" & is.na(Lat), 41.6857638, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Georges Bank" & is.na(Lon), -67.43312954, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Gloucester" & is.na(Lat), 42.66990965, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Gloucester" & is.na(Lon), -70.56927822, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Halifax" & is.na(Lat), 44.50120419, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Halifax" & is.na(Lon), -63.350585, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Jeffreys Ledge" & is.na(Lat), 42.84561449, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Jeffreys Ledge" & is.na(Lon), -70.2785453, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Letang" & is.na(Lat), 45.05463326, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Letang" & is.na(Lon), -66.82447571, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "L'etete" & is.na(Lat), 45.05919424, Lat)) %>%
  mutate(Lon = ifelse(Ground == "L'etete" & is.na(Lon), -66.92032799, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Long Island Shoal" & is.na(Lat), 44.3443951, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Long Island Shoal" & is.na(Lon), -66.34333657, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Maces Bay" & is.na(Lat), 45.08543954, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Maces Bay" & is.na(Lon), -66.49890408, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Margaretsville" & is.na(Lat), 45.06269418, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Margaretsville" & is.na(Lon), -65.07807333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Montauk" & is.na(Lat), 41.08410176, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Montauk" & is.na(Lon), -71.78377623, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Moores Ledge" & is.na(Lat), 44.03835829, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Moores Ledge" & is.na(Lon), -68.66055861, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Moose Island" & is.na(Lat), 44.89083083, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Moose Island" & is.na(Lon), -66.98362489, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Moriches" & is.na(Lat), 40.76967358, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Moriches" & is.na(Lon), -72.78866097, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Mt Desert" & is.na(Lat), 44.22250499, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Mt Desert" & is.na(Lon), -68.1347541, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Narragansett" & is.na(Lat), 41.41855419, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Narragansett" & is.na(Lon), -71.40934304, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Nauset Beach" & is.na(Lat), 41.78625851, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Nauset Beach" & is.na(Lon), -69.92909104, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "NE Banks" & is.na(Lat), 44.27697, Lat)) %>%
  mutate(Lon = ifelse(Ground == "NE Banks" & is.na(Lon), -67.055, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Passamaquoddy" & is.na(Lat), 45.09881325, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Passamaquoddy" & is.na(Lon), -66.98237386, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Point Judith" & is.na(Lat), 41.35334783, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Point Judith" & is.na(Lon), -71.47100556, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Port Mouton" & is.na(Lat), 43.92620148, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Port Mouton" & is.na(Lon), -64.80896741, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Prong" & is.na(Lat), 44.41020588, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Prong" & is.na(Lon), -66.73238236, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Rhode Island" & is.na(Lat), 41.41460769, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Rhode Island" & is.na(Lon), -71.35063657, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Roseway" & is.na(Lat), 43.62558316, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Roseway" & is.na(Lon), -65.32248582, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Schoodic Ridge" & is.na(Lat), 44.32285195, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Schoodic Ridge" & is.na(Lon), -68.05717077, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Seelys Head" & is.na(Lat), 45.08633333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Seelys Head" & is.na(Lon), -66.65033333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Seelys Cove" & is.na(Lat), 45.083633, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Seelys Cove" & is.na(Lon), -66.65, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Sheet Harbour" & is.na(Lat), 44.79921858, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Sheet Harbour" & is.na(Lon), -62.50327167, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Shelburne" & is.na(Lat), 43.3, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Shelburne" & is.na(Lon), -64.75, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Ship Head Cove" & is.na(Lat), 44.91616667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Ship Head Cove" & is.na(Lon), -66.88983333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "St Andrews Bay" & is.na(Lat), 45.06925971, Lat)) %>%
  mutate(Lon = ifelse(Ground == "St Andrews Bay" & is.na(Lon), -67.05879512, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "St Marys Bay" & is.na(Lat), 44.43039754, Lat)) %>%
  mutate(Lon = ifelse(Ground == "St Marys Bay" & is.na(Lon), -66.09021807, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Teardrop" & is.na(Lat), 44.42, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Teardrop" & is.na(Lon), -66.725, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Thatchers Island" & is.na(Lat), 42.63782397, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Thatchers Island" & is.na(Lon), -70.57020679, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "The Patch" & is.na(Lat), 44.35, Lat)) %>%
  mutate(Lon = ifelse(Ground == "The Patch" & is.na(Lon), -62.25, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Tusket Basin" & is.na(Lat), 42.83436333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Tusket Basin" & is.na(Lon), -66.7513967, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Western Head" & is.na(Lat), 43.97724536, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Western Head" & is.na(Lon), -64.67842978, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "White Head" & is.na(Lat), 44.29816667, Lat)) %>%
  mutate(Lon = ifelse(Ground == "White Head" & is.na(Lon), -66.65083333, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Bullpen" & is.na(Lat), 44.33333333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Bullpen" & is.na(Lon), -61.6, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Centerville" & is.na(Lat), 44.51033333, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Centerville" & is.na(Lon), -66.10066667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Crab Rock" & is.na(Lat), 45.02, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Crab Rock" & is.na(Lon), -66.85, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Mystrey Island" & is.na(Lat), 44.78, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Mystrey Island" & is.na(Lon), -66.76, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Sea Wall" & is.na(Lat), 44.75, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Sea Wall" & is.na(Lon), -66.84, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "North Head" & is.na(Lat), 44.75915098, Lat)) %>%
  mutate(Lon = ifelse(Ground == "North Head" & is.na(Lon), -66.739379, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Horseshoe" & is.na(Lat), 43.65, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Horseshoe" & is.na(Lon), -66.9, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "East of Gully" & is.na(Lat), 43.45, Lat)) %>%
  mutate(Lon = ifelse(Ground == "East of Gully" & is.na(Lon), -66.21666667, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Kettle Bottom" & is.na(Lat), 43.5, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Kettle Bottom" & is.na(Lon), -69.5, Lon)) %>%
  
  mutate(Lat = ifelse(Ground == "Hudson Canyon" & is.na(Lat), 39.44444609, Lat)) %>%
  mutate(Lon = ifelse(Ground == "Hudson Canyon" & is.na(Lon), -72.19678104, Lon))

#Set method/instrument type
Master = Master %>%
  mutate(Method = ifelse(Supplier == "Comeaus", "Microwave",
                  ifelse(Supplier == "Connors", "Distillation",
                  ifelse(Supplier == "Maine", "Microwave",
                  ifelse(Supplier == "Scotia", "Fatmeter",
                  NA)))))
  
setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/Main Data/"))
WAA = read_csv("WeightAtAge.csv")
WAA = WAA %>% dplyr::select(Year = YEAR, Age = AGE, WAA) %>%
  dplyr::filter(Age < 12)
Master = full_join(Master, WAA)
Master = Master %>% filter(!is.na(Fat))
Master %>% write_csv("Total Fat Data.csv")
