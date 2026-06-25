## Global options
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
library(measurements)
library(geodata)
library(terra)
setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/"))
#All Tags deployed

Tags = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))
completeReturns = read_csv(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/complete.returns.csv"))

# Land Data
can <- gadm(country='CAN', level=1, path = "geodata_default_path", version="latest", 
            resolution = 1, regions = c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador", "Québec"))
us  <- gadm(country='USA', level=1, path = "geodata_default_path", version="latest", 
            resolution = 1, regions = c("Maine"))

can1 <- rbind(can, us)

NBNS_spat <- can1[can1$NAME_1 %in% c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador", "Québec", "Maine"), ]

NBNS_sf <- st_as_sf(NBNS_spat)

GBMap <- ext(-66.5, -65.5, 43.2, 44)
GBout_sf <- st_as_sf(crop(NBNS_spat, GBMap))


# German Bank Only tags using attribute bounds
GermanBankTags <- Tags %>%
  filter(Lon >= xmin(GBMap) & Lon <= xmax(GBMap),
         Lat >= ymin(GBMap) & Lat <= ymax(GBMap))

GermanBankTagsBeforeAugust <- GermanBankTags %>%
  mutate(Date = ymd(Date)) %>%
        filter(month(Date) < 8 | (month(Date) == 8 & day(Date) == 1))

returnedTagsGermanBank <- inner_join(GermanBankTagsBeforeAugust, completeReturns, by = c("Tag_Num" = "TAG_NUMBER")) %>%
  mutate(TaggedArea = "German Bank") %>%
  rename(TagReturnArea = catchAREA,
         DateTagged = Date,
         TagReturnDate = DATE) %>%
  dplyr::select(-c(Vessel,...1, BOAT, GearType, Company, Catch.t, ...11, Year, Lat, Lon)) %>%
  dplyr::select(Tag_Num, DateTagged, TaggedArea, TagReturnDate, TagReturnArea, id, dataorigin)

GermanBankpreAugust1 <- write_csv(returnedTagsGermanBank, paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/GermanBankpreAugust1.csv"))
