#=========================================================
# Assign NAFO Subunits to Full Tag Returns
#=========================================================

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

sf_use_s2(FALSE)

#=========================================================
# File locations
#=========================================================

data_dir <- "C:/Users/herri/Documents/GitHub/HerringScience.github.io"

timGrounds <- read.csv(
  file.path(data_dir, "Main Data", "timGrounds.csv")
)

groundWeirMasterSheet <- read.csv(
  file.path(data_dir, "Box Coordinates", "Grounds Weir Master Sheet.csv")
)

groundWeirMasterSheet$Ground <- str_trim(groundWeirMasterSheet$Ground)

returns <- read.csv(
  file.path(
    data_dir,
    "Source Data",
    "Tagging",
    "Tag Returns",
    "Full Returns.csv"
  ),
  stringsAsFactors = FALSE
)

#=========================================================
# Build Tim Grounds polygons
#=========================================================

ground_list <- split(timGrounds, timGrounds$Box)

ground_polys <- lapply(ground_list, function(x){
  
  coords <- as.matrix(x[, c("X", "Y")])
  
  if(!all(coords[1, ] == coords[nrow(coords), ])){
    coords <- rbind(coords, coords[1, ])
  }
  
  st_polygon(list(coords))
})

grounds_sf <- st_sf(
  Box = names(ground_list),
  geometry = st_sfc(ground_polys, crs = 4326)
)

grounds_sf <- st_make_valid(grounds_sf)

#=========================================================
# Ground -> NAFO lookup
#=========================================================

ground_lookup <- tibble(
  Ground = c(
    "NB Coastal",
    "Grand Manan",
    "Grand Manan Banks",
    "Long Island",
    "German Bank",
    "Seal Island",
    "Scots Bay",
    "Yankee Bank",
    "Trinity",
    "Browns Bank",
    "SW Grounds",
    "Gannet Dry Ledge",
    "Lurcher"
  ),
  NAFO = c(
    "4Xs",
    "4Xr",
    "4Xq",
    "4Xq",
    "4Xq",
    "4Xp",
    "4Xr",
    "4Xr",
    "4Xq",
    "4Xp",
    "4Xp",
    "4Xq",
    "4Xq"
  )
)


#=========================================================
# Weir -> NAFO lookup
#=========================================================

weir_lookup_df <- tibble(
  
  Location = c(
    
    #=====================================================
    # 4Xs
    #=====================================================
    
    "Back Bay","Blacks Harbour","Blisses","Campobello",
    "Chance Harbour","Chattis Point","Crow Harbour",
    "Crow Island","Curry Cove","Deadmans","Deep Cove",
    "Digdeguash Basin","Fairhaven","Friers Bay",
    "Fryes Island","Herring Cove","Indian Island",
    "Lawrence Cove","Leonardville","Lepreau",
    "Lords Cove","Meadow Brook","Mill Cove",
    "New River","Oak Bay","Red Head",
    "Round Meadow","Sand Beach","Sandy Cove",
    "Schooner Cove","Seelys Basin","Seelys Cove",
    "Seelys Head","Ship Beach","Spectacle",
    "Spider Cove","Spruce Cove","Tuckers Cove",
    "Wolves","Beaver Harbour","Crab Rock",
    "Eagle Island","Letang","L'etete",
    "Maces Bay","Passamaquoddy","St Andrews Bay",
    
    #=====================================================
    # 4Xr
    #=====================================================
    
    "Bradfords Cove",
    "Bradfords Cove Weir",
    "Cora Bell",
    "Iron Lady",
    "Money Cove",
    "Mumps",
    "Pipe Dream",
    "Prong",
    "Teardrop",
    "White Head",
    "Mystrey Island",
    "Sea Wall",
    "North Head",
    "Whale Cove",
    "Winner",
    
    #=====================================================
    # 4Xq
    #=====================================================
    
    "Long Island Shoal",
    "Long Island",
    "NE Banks",
    "Northeast Bank",
    "St Marys Bay",
    "Centerville",
    
    #=====================================================
    # 4Xp
    #=====================================================
    
    "East of Gully",
    "Horseshoe",
    "Port Mouton",
    "Roseway",
    "Shelburne",
    "Tusket Basin",
    "Western Head",
    
    #=====================================================
    # Offshore Banks
    #=====================================================
    
    "The Patch",
    "Offshore Banks",
    "Bullpen"
  ),
  
  NAFO = c(
    
    rep("4Xs", 47),
    
    rep("4Xr", 15),
    
    rep("4Xq", 6),
    
    rep("4Xp", 7),
    
    rep("4W", 3)
    
  )
)

## Use coordinates

ground_points <- groundWeirMasterSheet %>%
  left_join(
    weir_lookup_df,
    by = c("Ground" = "Location")
  ) %>%
  filter(
    !is.na(Lat),
    !is.na(Lon),
    !is.na(NAFO)
  )

ground_points_sf <- st_as_sf(
  ground_points,
  coords = c("Lon", "Lat"),
  crs = 4326
)


#=========================================================
# Release NAFO from Ground
#=========================================================

returns <- returns %>%
  left_join(
    ground_lookup,
    by = c("Ground" = "Ground")
  ) %>%
  rename(ReleaseNAFO = NAFO)


#=========================================================
# Use coordinates if ReleaseNAFO missing
#=========================================================

missing <- is.na(returns$ReleaseNAFO)

release_pts <- st_as_sf(
  returns[missing, ],
  coords = c("Lon","Lat"),
  crs = 4326,
  remove = FALSE
)

release_join <- st_join(
  release_pts,
  grounds_sf,
  left = TRUE,
  largest = TRUE
)

ground_to_nafo <- c(
  "NB Coastal"       = "4Xs",
  "Grand Manan"      = "4Xr",
  "Grand Manan Banks"= "4Xq",
  "Long Island"      = "4Xq",
  "German Bank"      = "4Xq",
  "Seal Island"      = "4Xp",
  "Scots Bay"        = "4Xr",
  "Yankee Bank"      = "4Xr",
  "Trinity"          = "4Xq",
  "Browns Bank"      = "4Xp",
  "SW Grounds"       = "4Xp",
  "Gannet Dry Ledge" = "4Xq",
  "Lurcher"          = "4Xq"
)

returns$ReleaseNAFO <- ifelse(
  is.na(returns$ReleaseNAFO),
  ground_to_nafo[returns$GroundPoly],
  returns$ReleaseNAFO
)

#=========================================================
# Return NAFO from returnedArea
#=========================================================

returns <- returns %>%
  left_join(
    weir_lookup_df,
    by = c("returnedArea" = "Location")
  )

returns <- returns %>%
  mutate(
    ReturnNAFO = case_when(
      
      # Existing grounds
      str_detect(returnedArea, regex("German Bank", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Northeast Bank", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("NE Banks", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Trinity", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Lurcher", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Gannet", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Long Island", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Long Island Shoal", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Long Island", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("NE Banks", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Northeast Bank", TRUE)) ~ "4Xq",
      str_detect(returnedArea, regex("Centerville", TRUE)) ~ "4Xq",
      
      str_detect(returnedArea, regex("Seal Island", TRUE)) ~ "4Xp",
      str_detect(returnedArea, regex("Browns", TRUE)) ~ "4Xp",
      str_detect(returnedArea, regex("SW Grounds", TRUE)) ~ "4Xp",
      str_detect(returnedArea, regex("Tusket Basin", TRUE)) ~ "4Xp",
      
      str_detect(returnedArea, regex("Grand Manan", TRUE)) ~ "4Xr",
      str_detect(returnedArea, regex("Scots Bay", TRUE)) ~ "4Xr",
      str_detect(returnedArea, regex("Yankee", TRUE)) ~ "4Xr",
      str_detect(returnedArea, regex("Bradfords", TRUE))      ~ "4Xr",
      str_detect(returnedArea, regex("Bradfords Cove Weir", TRUE)) ~ "4Xr",
      
      str_detect(returnedArea, regex("NB Coastal", TRUE)) ~ "4Xs",
      
      str_detect(returnedArea, regex("The Patch", TRUE)) ~ "4W",
      str_detect(returnedArea, regex("Bullpen", TRUE)) ~ "4W",
      
      TRUE ~ NA_character_
    )
  )


# use weir lookup where ground lookup failed

returns$ReturnNAFO <- ifelse(
  is.na(returns$ReturnNAFO),
  returns$NAFO,
  returns$ReturnNAFO
)

returns$NAFO <- NULL

#=========================================================
# Return coordinates fallback
#=========================================================

missing_return <- is.na(returns$ReturnNAFO) &
  !is.na(returns$returnedLat) &
  !is.na(returns$returnedLon)

return_pts <- st_as_sf(
  returns[missing_return, ],
  coords = c("returnedLon","returnedLat"),
  crs = 4326,
  remove = FALSE
)

return_join <- st_join(
  return_pts,
  grounds_sf,
  left = TRUE,
  largest = TRUE
)

returns$ReturnNAFO <- ifelse(
  is.na(returns$ReturnNAFO),
  ground_to_nafo[returns$ReturnGroundPoly],
  returns$ReturnNAFO
)

### Still unmatched

returns %>%
  filter(is.na(ReturnNAFO)) %>%
  count(returnedArea, sort = TRUE)

returns %>%
  filter(is.na(ReleaseNAFO)) %>%
  count(Ground, sort = TRUE)

#=========================================================
# Results
#=========================================================

table(returns$ReleaseNAFO, useNA = "ifany")
table(returns$ReturnNAFO, useNA = "ifany")

#=========================================================
# Export
#=========================================================

write.csv(
  returns,
  "FullReturns_with_NAFO.csv",
  row.names = FALSE
)