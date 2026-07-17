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
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
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
library(oce)
library(pander)
library(janitor)

#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")


#Previously entered
tagReturns2023 <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2023/2023 Tag Returns.csv")
tagReturns2024 <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2024/2024 Tag Returns.csv")
tagReturns2025 <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2025/2025 Tag Returns.csv")
complete.returns <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/complete.returns.csv")
complete.returns <- complete.returns %>%
  dplyr::select(-X)
tagReturns <- complete.returns

#Year spreadsheets - To be entered
tagReturns1 <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2026/2026 Tag Returns.csv")
  
tagReturns$Tag_Num = as.numeric(tagReturns$Tag_Num)
  #Add in missing tag num/incomplete tag number count and sheet
  
  tagReturns$returnedArea = as.character(tagReturns$returnedArea)

original_tagReturns <- tagReturns

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

#Change coordinates in tagReturns to decimal degrees
dmm_to_dd <- function(x) {
  sapply(strsplit(as.character(x), "\\s+"), function(z) {
    as.numeric(z[1]) + as.numeric(z[2]) / 60
  })
}

tagReturns$returnedLat <- dmm_to_dd(tagReturns$returnedLat)
tagReturns$returnedLon <- -dmm_to_dd(tagReturns$returnedLon)

#Insert lat and lon from timGrounds if Ground is present.

ground_lookup <- timGrounds %>%
  group_by(Box) %>%
  summarise(
    fill_lon = first(X),
    fill_lat = first(Y),
    .groups = "drop"
  )

#Clean up any extra white space from Grounds

tagReturns <- tagReturns %>%
  mutate(
    returnedArea = trimws(returnedArea)
  )

ground_lookup <- ground_lookup %>%
  mutate(
    Box = trimws(Box)
  )

#Complete lat/lon/Ground

tagReturns <- tagReturns %>%
  mutate(
    returnedLat = na_if(as.character(returnedLat), ""),
    returnedLon = na_if(as.character(returnedLon), ""),
    returnedLat = as.numeric(returnedLat),
    returnedLon = as.numeric(returnedLon)
  )

tagReturns <- tagReturns %>%
  left_join(
    ground_lookup,
    by = c("returnedArea" = "Box")
  ) %>%
  mutate(
    returnedLon = coalesce(returnedLon, fill_lon),
    returnedLat = coalesce(returnedLat, fill_lat)
  ) %>%
  select(-fill_lon, -fill_lat)

#Missing lat/lons therefore unable to track tag return

removed_missing_coords <- tagReturns %>%
  filter(
    is.na(returnedLat) |
      is.na(returnedLon) |
      trimws(returnedLat) == "" |
      trimws(returnedLon) == ""
  ) %>%
  mutate(
    RemovalReason = "Missing return coordinates"
  )

tagReturns <- tagReturns %>%
  filter(
    !is.na(returnedLat),
    !is.na(returnedLon),
    trimws(returnedLat) != "",
    trimws(returnedLon) != ""
  )


events_sf <- st_as_sf(
  tagReturns,
  coords = c("returnedLon", "returnedLat"),
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
    returnedArea = case_when(
      returnedArea == "Other" & !is.na(Box) ~ Box,
      TRUE ~ returnedArea
    )
  )

# Remove geometry if desired
taggingEvents <- events_joined %>%
  st_drop_geometry()

#Track tags without area
removed_missing_area <- tagReturns %>%
  filter(
    is.na(returnedArea) |
      trimws(returnedArea) == ""
  ) %>%
  mutate(
    RemovalReason = "Could not assign return area"
  )

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
    #returnedDate = dmy(returnedDate),
    returnedJulian = yday(returnedDate)
  )

removed_bad_dates <- tagReturns %>%
  mutate(
    returnedDate_parsed = ymd(returnedDate)
  ) %>%
  filter(is.na(returnedDate_parsed)) %>%
  mutate(
    RemovalReason = "Invalid returnedDate"
  )


#Original Tagging Events
taggingEvents <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/TaggingEvents.csv")
taggingEvents$Tag_Num <- as.numeric(taggingEvents$Tag_Num)

#First coordinate for each ground
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

#Tracking removed tags

removed_negative_days <- fullReturns %>%
  filter(daysAtLarge <= 0) %>%
  mutate(
    RemovalReason = paste(
      "Days at large <= 0 (",
      daysAtLarge,
      ")"
    )
  )

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


#Write/Update Full Returns file
setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/"))
fullTagReturns <- write_csv(fullReturnsCSV, "Full Returns.csv" )


#Write/Update tags that were found but had to be removed for some reason.
removed_missing_coords <- removed_missing_coords %>%
  mutate(across(everything(), as.character))

removed_negative_days <- removed_negative_days %>%
  mutate(across(everything(), as.character))

removed_missing_area <- removed_missing_area %>%
  mutate(across(everything(), as.character))

removed_tags <- bind_rows(
  removed_missing_coords,
  removed_negative_days,
  removed_missing_area
) %>%
  distinct(Tag_Num, .keep_all = TRUE)

removed_tags <- removed_tags %>%
  mutate(across(everything(), as.character))

removed_tagsCSV <- read_csv("Removed_From_FullReturns.csv" )

removed_tagsCSV <- removed_tagsCSV %>%
  mutate(across(everything(), as.character))

removed_tagsCSV <- bind_rows(removed_tagsCSV, removed_tags)

write_csv(
  removed_tagsCSV,
  "Removed_From_FullReturns.csv"
)
