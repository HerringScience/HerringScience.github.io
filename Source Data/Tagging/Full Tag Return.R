rm(list=ls())

library(lubridate)
library(ggplot2)
library(patchwork)
library(scales)
library(cli)
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
library(sf)
library(terra)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(DT)
library(dygraphs)
library(leaflet)
library(rmapshaper)
library(plotly)
library(mapproj)
library(oce) #new CTD Data package
library(pander)
library(janitor)

#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")

#Year spreadsheets - To be entered
tagReturns <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2026/2026 Tag Returns.csv")
tagReturns$Tag_Num = as.numeric(tagReturns$Tag_Num)

#Previously entered
tagReturns2023 <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2023/2023 Tag Returns.csv")
tagReturns2024 <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2024/2024 Tag Returns.csv")
tagReturns2025 <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2025/2025 Tag Returns.csv")

#############################################


# Add source file information
returns2023 <- tagReturns2023 %>% mutate(source_year = 2023)
returns2024 <- tagReturns2024 %>% mutate(source_year = 2024)
returns2025 <- tagReturns2025 %>% mutate(source_year = 2025)

# Combine all years
all_returns <- bind_rows(
  mutate_all(tagReturns2023, as.character),
  mutate_all(tagReturns2024, as.character),
  mutate_all(tagReturns2025, as.character)
)

# Parse dates
all_returns <- all_returns %>%
  mutate(
    returnedDate = parse_date_time(
      returnedDate,
      orders = c("ymd", "dmy", "mdy")
    ),
    return_year = year(returnedDate)
  )

# Count non-missing fields to identify the most complete record
all_returns <- all_returns %>%
  mutate(
    completeness =
      rowSums(!is.na(.)) +
      rowSums(across(everything(), ~ . != ""), na.rm = TRUE)
  )

# If a tag exists in multiple year files,
# keep the most complete version
all_returns <- all_returns %>%
  arrange(Tag_Num, desc(completeness)) %>%
  distinct(Tag_Num, .keep_all = TRUE)



tagReturns2023 <- all_returns %>%
  filter(return_year == 2023)

tagReturns2024 <- all_returns %>%
  filter(return_year == 2024)

tagReturns2025 <- all_returns %>%
  filter(return_year == 2025)

# Records requiring manual review
unknownDateReturns <- all_returns %>%
  filter(is.na(return_year))

############################################

#Full Returns.csv - This is the sheet to update!
fullReturnsCSV <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Full Returns.csv")
fullReturnsCSV$Tag_Num <- as.numeric(fullReturnsCSV$Tag_Num)
fullReturnsCSV$Date <- as.Date(fullReturnsCSV$Date)
fullReturnsCSV$returnedDate <- as.Date(fullReturnsCSV$returnedDate)


#Grounds to use
timGrounds <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/timGrounds.csv")

# Build polygons from timGrounds
ground_list <- split(timGrounds, timGrounds$Box)

polys <- lapply(ground_list, function(x){
  
  coords <- as.matrix(x[, c("X", "Y")])
  
  if(!all(coords[1, ] == coords[nrow(coords), ])){
    coords <- rbind(coords, coords[1, ])
  }
  
  st_polygon(list(coords))
})

grounds_sf <- st_sf(
  Box = names(ground_list),
  geometry = st_sfc(polys, crs = 4326)
)



#Remove any tag that was unable to be associated with a catch/catch weight.
tagReturns = drop_na(tagReturns, Catch.t)


#Change coordinates in tagReturns2024 to decimal degrees
dmm_to_dd <- function(x) {
  sapply(strsplit(as.character(x), "\\s+"), function(z) {
    as.numeric(z[1]) + as.numeric(z[2]) / 60
  })
}

tagReturns$returnedLat <- dmm_to_dd(tagReturns$returnedLat)
tagReturns$returnedLon <- -dmm_to_dd(tagReturns$returnedLon)

# tagReturns = drop_na(tagReturns, returnedLat) Do we always need the lat and lon if we have the ground?

needs_fill <- (
  is.na(tagReturns$returnedArea) |
    trimws(tagReturns$returnedArea) == "" |
    toupper(trimws(tagReturns$returnedArea)) %in%
    c("UNKNOWN", "UNKNOWN GROUND")
) &
  !is.na(tagReturns$returnedLat) &
  !is.na(tagReturns$returnedLon)

pts <- st_as_sf(
  tagReturns[needs_fill, ],
  coords = c("returnedLon", "returnedLat"),
  crs = 4326
)

joined <- st_join(
  pts,
  grounds_sf["Box"],
  join = st_intersects,
  left = TRUE
)

tagReturns$returnedArea[needs_fill] <- joined$Box

#Add Julien Date
tagReturns <- tagReturns %>%
  mutate(
    returnedDate = ymd(returnedDate),   # yyyy/mm/dd or dd/mm/yyyy. Seems to keep switching between the two.
    returnedJulian = yday(returnedDate)
  )


#Original Tagging Events
taggingEvents <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/TaggingEvents.csv")
taggingEvents$Tag_Num <- as.numeric(taggingEvents$Tag_Num)

# First coordinate for each ground
ground_lookup <- timGrounds %>%
  group_by(Box) %>%
  summarise(
    fill_lon = first(X),
    fill_lat = first(Y),
    .groups = "drop"
  )

# Fill missing coordinates - The Tagger did not enter actual coordinates, but did enter the ground. The coordinates were pulled from timGrounds and filled in.

taggingEvents <- taggingEvents %>%
  left_join(
    ground_lookup,
    by = c("Ground" = "Box")
  ) %>%
  mutate(
    Lon = coalesce(Lon, fill_lon),
    Lat = coalesce(Lat, fill_lat)
  ) %>%
  select(-fill_lon, -fill_lat)

events_sf <- st_as_sf(
  taggingEvents,
  coords = c("Lon", "Lat"),
  crs = 4326,
  remove = FALSE
)

# Spatial join
events_joined <- st_join(
  events_sf,
  grounds_sf["Box"],
  join = st_within
)

# Replace Other with polygon-assigned ground
events_joined <- events_joined %>%
  mutate(
    Ground = case_when(
      Ground == "Other" & !is.na(Box) ~ Box,
      TRUE ~ Ground
    )
  )

# Remove geometry if desired
taggingEvents <- events_joined %>%
  st_drop_geometry()


#Shrink taggingEvents down to just what is needed
taggingEvents <- taggingEvents %>%
  dplyr::select(
    Tag_Num,
    Date,
    Lon,
    Lat,
    Ground,
    Julian)


#Join taggingEvents by YYYYtagReturns

fullReturns <- inner_join(tagReturns, taggingEvents,
                      by = c("Tag_Num" = "Tag_Num"))


#Number of days to recapture

fullReturns <- fullReturns %>%
  mutate(
    Date = as.Date(Date),
    daysAtLarge = as.numeric(returnedDate - Date)
  )

fullReturns <- fullReturns %>%
  mutate(
    daysAtLarge = as.numeric(returnedDate - Date)
  )

#remove any tags that the values are negative. Must go through this list later to figure out what happened.

fullReturns <- fullReturns %>%
  filter(daysAtLarge > 0)


#Remove columns that do not add value and rearrange

fullReturns <- fullReturns %>%
  dplyr::select(
    Tag_Num,
    Date,
    Julian,
    Ground,
    Lat,
    Lon,
    returnedDate,
    returnedJulian,
    returnedArea,
    returnedLat,
    returnedLon,
    Catch.t,
    daysAtLarge,
    dataorigin, #returned data origin
    Comments
  )

fullReturnsCSV$Ground <- as.character(fullReturnsCSV$Ground)
fullReturnsCSV$returnedArea <- as.character(fullReturnsCSV$returnedArea)
fullReturnsCSV$dataorigin <- as.character(fullReturnsCSV$dataorigin)
fullReturnsCSV$Comments <- as.character(fullReturnsCSV$Comments)

#Combine to FullReturnsCSV
fullReturnsCSV <- bind_rows(fullReturnsCSV, fullReturns)

#Write new full returns file
setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/"))
fullTagReturns <- write_csv(fullReturnsCSV, "Full Returns.csv" )
