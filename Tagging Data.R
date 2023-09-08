# remove everything in the workspace
rm(list = ls())

library(lubridate)
library(tidyverse)
library(measurements)

# Set all tagging log data here
Tag_Num = c(578551:578800) #Can add any tag entries, including breaks/gaps in the sequences
Date = "2023-09-04" #"YYYY-MM-DD"
Lat = "43 14 71" #Degree-Min-Sec format from the boat but only the numbers written with spaces (e.g. "44 16 23")
Lon = "66 21 59"
Vessel = "" #As written unless changed in script below: "Lady Melissa", "Sealife II", "Tasha Marie", "Lady Janice", "Morning Star"
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
Lon = conv_unit(Lon,"deg_min_sec","dec_deg") #converts the entered longitude from deg_min_sec to decimal degrees
Lon = as.numeric(Lon) #then saves it as a numeric value
Lat = conv_unit(Lat, "deg_min_sec", "dec_deg") #same as above but for latitude
Lat = as.numeric(Lat)
Lon = -1*Lon #convert longitude to negative
Tags = tibble(Tag_Num, Date, Lon, Lat, Vessel, Survey, CTD) #saves all the values into a tibble (a tidyverse dataframe)
Tags$Date = ymd(Tags$Date) #convert the Date to a year-month-day (ymd) format
Tags = Tags %>% #this entire pipeline (%>%) will take the Tags dataframe, and mutate (create or modify) a variable named Tagger
  mutate(Tagger = ifelse(Vessel == "Lady Melissa", "Joseph Nickerson", #this is used to link each tagger to their vessel
                  ifelse(Vessel == "Sealife II", "Annik Doucette", #if the vessel-tagger assignments change it needs to be edited here
                  ifelse(Vessel == "Tasha Marie", "Dale Fitzgerald",
                  ifelse(Vessel == "Lady Janice", "Lee Surette",
                  ifelse(Vessel == "Morning Star", "Nicholas D'entremont",
                  ifelse(Vessel == "Fundy Monarch", "William Cusack",
                  ifelse(Vessel == "Canada 100", "William Cusack",
                  ifelse(Vessel == "Brunswick Provider", "William Cusack", 
                  NA))))))))) %>%
  mutate(Ground = ifelse(between(Lat, 45.02, 45.4) & between(Lon, -65.5, -64.5), "Scots Bay", #finally, if lat/lon is between these values it will assign ground as either SB or GB
                         ifelse(between(Lat, 43.15, 43.7) & between(Lon, -66.75, -66.05), "German Bank", 'Other'))) #if lat/lon is in neither range, ground will be set to Other

# Bind Data and add Julian/Annual Tags
TaggingEvents = full_join(Tagging, Tags) #joins the tagging database with the new data
TaggingEvents = TaggingEvents %>% 
  mutate(Julian = yday(Date)) %>% #adds the Julian date column (again, through Mutate which makes new columns)
  mutate(Year = as.numeric(substr(Date, 1, 4))) %>% #Year is a substring of Date, from the 1st to the 4th value
  dplyr::select(-Tag_Annual) #a negative select will drop the Annual Tag count for now, because it is calculated later

Tag_Annual = TaggingEvents %>%
  group_by(Tagger) %>% #group_by can by read as "for each", so in this case, for each unique Tagger and each unique Year they tagged in we will calculate their annual tags applied
  mutate(count = n_distinct(Year)) %>% 
  summarize(n=n(), count2 = mean(count)) %>% 
  mutate(Tag_Annual = n/count2) %>% 
  dplyr::select(-n, -count2) #used to drop the columns made just to calculate the annual tags

TaggingEvents = left_join(TaggingEvents, Tag_Annual, by = "Tagger") #combine the database + new data with the annual counts all into one frame

# Save TaggingEvents.csv - saves the same file in both Source and Main data which is redundant but necessary because of how other coding is setup
TaggingEvents %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))
TaggingEvents %>% write_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/TaggingEvents.csv"))