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
tagReturns <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2024/2024 Tag Returns.csv")
tagReturns2025 <-read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2025/2025 Tag Returns.csv")
complete.returns <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/complete.returns.csv")
complete.returns <- complete.returns %>%
  dplyr::select(-X)

#Year spreadsheets - To be entered
tagReturns <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/Compiled tag return spreadsheets/2026/2026 Tag Returns.csv")
  
tagReturns$Tag_Num = as.numeric(tagReturns$Tag_Num)

#Remove damaged/missing/incomplete tag numbers and put in their own dataset
removed_missing_tag <- tagReturns %>%
  dplyr::filter(is.na(Tag_Num)) %>%
  dplyr::mutate(RemovalReason = "Missing complete Tag Number")

tagReturns <- tagReturns %>%
  dplyr::filter(!is.na(Tag_Num))
  
  tagReturns$returnedArea = as.character(tagReturns$returnedArea)

original_tagReturns <- tagReturns

#Full Returns.csv - This is the sheet to update!
fullReturnsCSV <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Full Returns.csv")
fullReturnsCSV$Tag_Num <- as.numeric(fullReturnsCSV$Tag_Num)
fullReturnsCSV$Date <- as.Date(fullReturnsCSV$Date)
fullReturnsCSV$returnedDate <- as.Date(fullReturnsCSV$returnedDate)

NAFO_subunits <- read.csv(
  "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/NAFO_subunits.csv"
)

groundWeirMasterSheet <- read.csv(
  "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/Grounds Weir Master Sheet.csv",
  stringsAsFactors = FALSE
)

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

# Build NAFO polygons
NAFO_subunits <- NAFO_subunits %>%
  filter(
    !Area %in% c(
      "3",
      "4",
      "5",
      "6"
    )
  )

nafo_list <- split(
  NAFO_subunits,
  NAFO_subunits$Area
)

nafo_polys <- lapply(
  nafo_list,
  function(x){
    
    coords <- as.matrix(
      x[, c("X","Y")]
    )
    
    if(!all(coords[1,] == coords[nrow(coords),])){
      
      coords <- rbind(
        coords,
        coords[1,]
      )
      
    }
    
    st_polygon(list(coords))
  }
)

nafo_sf <- st_sf(
  Area = names(nafo_list),
  geometry = st_sfc(
    nafo_polys,
    crs = 4326
  )
)

nafo_sf <- st_make_valid(nafo_sf)

nafo_sf$PolyArea <- as.numeric(
  st_area(
    st_transform(
      nafo_sf,
      3347
    )
  )
)

find_smallest_nafo <- function(
    lon,
    lat,
    nafo_sf
){
  
  if(
    is.na(lon) ||
    is.na(lat)
  ){
    return(NA_character_)
  }
  
  pt <- st_sfc(
    st_point(c(lon, lat)),
    crs = 4326
  )
  
  hits <- st_within(
    pt,
    nafo_sf
  )[[1]]
  
  if(length(hits) == 0){
    return(NA_character_)
  }
  
  candidates <- nafo_sf[hits, ]
  
  candidates$Area[
    which.min(candidates$PolyArea)
  ]
}

#Change coordinates in tagReturns to decimal degrees
dmm_to_dd <- function(x) {
  as.numeric(
    sapply(strsplit(trimws(as.character(x)), "\\s+"), function(z) {
      as.numeric(z[1]) + as.numeric(z[2]) / 60
    })
  )
}

tagReturns$returnedLat <- dmm_to_dd(tagReturns$returnedLat)
tagReturns$returnedLon <- -dmm_to_dd(tagReturns$returnedLon)

removed_missing_location <- tagReturns %>%
  dplyr::filter(
    (is.na(returnedArea) | returnedArea == "") &
      (is.na(returnedLat) | is.na(returnedLon))
  ) %>%
  dplyr::mutate(
    RemovalReason = "Missing return area and latitude/longitude"
  )

# Keep records that have either: A return area AND/OR both latitude and longitude

tagReturns <- tagReturns %>%
  dplyr::filter(
    !(
      (is.na(returnedArea) | returnedArea == "") &
        (is.na(returnedLat) | is.na(returnedLon))
    )
  )

#Add Julien Date

tagReturns <- tagReturns %>%
  mutate(
    #returnedDate = ymd(returnedDate),   # yyyy/mm/dd or dd/mm/yyyy. Seems to keep switching between the two.
    returnedDate = dmy(returnedDate),
    returnedJulian = yday(returnedDate)
  )

removed_bad_dates <- tagReturns %>%
  mutate(
    returnedDate_parsed = ymd(returnedDate)
  ) %>%
  filter(is.na(returnedDate_parsed)) %>%
  mutate(
    RemovalReason = "Invalid returned Date"
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

#=====================================================
# RELEASE NAFO
#=====================================================
# 
# release_sf <- st_as_sf(
#   fullReturns,
#   coords = c("Lon","Lat"),
#   crs = 4326,
#   remove = FALSE
# )

# release_join <- st_join(
#   release_sf,
#   nafo_sf["Area"],
#   left = TRUE,
#   largest = TRUE
# )

fullReturns$ReleaseNAFO <- mapply(
  find_smallest_nafo,
  fullReturns$Lon,
  fullReturns$Lat,
  MoreArgs = list(
    nafo_sf = nafo_sf
  )
)

#=====================================================
# RETURN NAFO FROM RETURN COORDINATES
#=====================================================

fullReturns$ReturnNAFO <- NA_character_

has_return_coords <-
  !is.na(fullReturns$returnedLat) &
  !is.na(fullReturns$returnedLon)

return_sf <- st_as_sf(
  fullReturns[has_return_coords, ],
  coords = c(
    "returnedLon",
    "returnedLat"
  ),
  crs = 4326,
  remove = FALSE
)

return_join <- st_join(
  return_sf,
  nafo_sf["Area"],
  left = TRUE,
  largest = TRUE
)

fullReturns$ReturnNAFO <- mapply(
  find_smallest_nafo,
  fullReturns$returnedLon,
  fullReturns$returnedLat,
  MoreArgs = list(
    nafo_sf = nafo_sf
  )
)

#=====================================================
# FALLBACK TO WEIR LOCATIONS
#=====================================================

weir_lookup <- groundWeirMasterSheet %>%
  dplyr::select(
    Ground,
    Lat,
    Lon
  ) %>%
  filter(
    !is.na(Lat),
    !is.na(Lon)
  )

weir_lookup <- weir_lookup %>%
  mutate(
    Lon = -abs(Lon)
  )
`

missing_return <- is.na(
  fullReturns$ReturnNAFO
)

fallback <- fullReturns[
  missing_return,
] %>%
  left_join(
    weir_lookup,
    by = c(
      "returnedArea" = "Ground"
    )
  )

valid_fallback <-
  !is.na(fallback$Lat) &
  !is.na(fallback$Lon)

if(any(valid_fallback)){
  
  fallback_sf <- st_as_sf(
    fallback[valid_fallback, ],
    coords = c("Lon","Lat"),
    crs = 4326,
    remove = FALSE
  )
  
  fallback_join <- st_join(
    fallback_sf,
    nafo_sf["Area"],
    left = TRUE,
    largest = TRUE
  )
  
  fullReturns$ReturnNAFO[
    which(missing_return)[valid_fallback]
  ] <- mapply(
    find_smallest_nafo,
    fallback$Lon[valid_fallback],
    fallback$Lat[valid_fallback],
    MoreArgs = list(
      nafo_sf = nafo_sf
    )
  )
}


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
    ReleaseNAFO,
    Lat,
    Lon,
    returnedDate,
    returnedJulian,
    returnedArea,
    ReturnNAFO,
    returnedLat,
    returnedLon,
    GearType,
    Catch.t,
    daysAtLarge,
    dataorigin,
    Comments
  )

fullReturnsCSV <- fullReturnsCSV %>%
  mutate(across(everything(), as.character))

fullReturns <- fullReturns %>%
  mutate(across(everything(), as.character))

#Combine to FullReturnsCSV
fullReturnsCSV <- bind_rows(fullReturnsCSV, fullReturns)


#Write/Update Full Returns file
setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/"))
fullTagReturns <- write_csv(fullReturnsCSV, "Full Returns.csv" )


#Write/Update tags that were found but had to be removed for some reason.
removed_negative_days <- removed_negative_days %>%
  mutate(across(everything(), as.character))

removed_missing_tag <- removed_missing_tag %>%
  mutate(across(everything(), as.character))

removed_missing_location <- removed_missing_location %>%
  mutate(across(everything(), as.character))

removed_bad_dates <- removed_bad_dates %>%
  mutate(across(everything(), as.character))


removed_tags <- bind_rows(
  removed_negative_days,
  removed_missing_tag,
  removed_missing_location,
  removed_bad_dates,
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
