# remove everything in the workspace
rm(list = ls())

library(lubridate)
library(tidyverse)
library(measurements)

# Set all tagging log data here
Tag_Num = c(523501:523650) #Can add any tag entries, including breaks/gaps in the sequences
Date = "2023-07-05" #"YYYY-MM-DD"
Lon = "44 39 02" #Degree-Min-Sec format from the boat but only the numbers written with spaces (e.g. "44 16 23")
Lat = "66 35 07"
Vessel = "Lady Janice" #As written unless changed in script below: "Lady Melissa", "Sealife II", "Tasha Marie", "Lady Janice", "Morning Star"
Survey = NA #Survey number for Scots Bay or German Bank when tags were applied, otherwise "NA"
CTD = NA #Add the CTD id only if a TAGGER completed a cast (not the cast by the HSC tech)

##
###
### SCRIPT BELOW ###
###
##

# Load TaggingEvents.csv
Tagging <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))
Tagging$Date = ymd(Tagging$Date) 

# Modify Data
Lon = conv_unit(Lon,"deg_min_sec","dec_deg")
Lon = as.numeric(Lon)
Lat = conv_unit(Lat, "deg_min_sec", "dec_deg")
Lat = as.numeric(Lat)
Lat = -1*Lat
Tags = tibble(Tag_Num, Date, Lon, Lat, Vessel, Survey, CTD)
Tags$Date = ymd(Tags$Date)
Tags = Tags %>% 
  mutate(Tagger = ifelse(Vessel == "Lady Melissa", "Joseph Nickerson",
                  ifelse(Vessel == "Sealife II", "Annik Doucette",
                  ifelse(Vessel == "Tasha Marie", "Dale Fitzgerald",
                  ifelse(Vessel == "Lady Janice", "Lee Surette",
                  ifelse(Vessel == "Morning Star", "Nicholas D'entremont",
                  NA)))))) %>%
  mutate(Ground = ifelse(between(Lat, 45.02, 45.4) & between(Lon, -65.5, -64.5), "Scots Bay", 
                         ifelse(between(Lat, 43.15, 43.7) & between(Lon, -66.75, -66.05), "German Bank", 'Other')))

# Bind Data and add Julian/Annual Tags
TaggingEvents = full_join(Tagging, Tags)
TaggingEvents = TaggingEvents %>% 
  mutate(Julian = yday(Date)) %>% 
  mutate(Year = as.numeric(substr(Date, 1, 4))) %>% 
  dplyr::select(-Tag_Annual)

Tag_Annual = TaggingEvents %>%
  group_by(Tagger) %>%
  mutate(count = n_distinct(Year)) %>%
  summarize(n=n(), count2 = mean(count)) %>%
  mutate(Tag_Annual = n/count2) %>%
  dplyr::select(-n, -count2)

TaggingEvents = left_join(TaggingEvents, Tag_Annual, by = "Tagger")

# Save TaggingEvents.csv
TaggingEvents %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))
TaggingEvents %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/TaggingEvents.csv"))