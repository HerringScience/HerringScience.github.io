# Global options
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

Tags <- read_csv(
  "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"
)

completeReturns <- read_csv(
  "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/complete.returns.csv"
)

#Survey boxes as per polygon_GB and polygon_SI
surveyBoxGB <- 
  st_polygon(list(matrix(
    c(
      -66.229, 43.567,
      -66.229, 43.233,
      -66.51, 43.233,
      -66.51, 43.567,
      -66.229, 43.567
    ),
    ncol = 2,
    byrow = TRUE
  )))

surveyBoxGB <- st_sf(
  geometry = st_sfc(surveyBoxGB, crs = 4326)
)

surveyBoxSI <-   
  st_polygon(list(matrix(
  c(
    -66.237, 43.51,
    -66.237, 43.24,
    -66.119, 43.24,
    -66.119, 43.51,
    -66.237, 43.51
  ),
  ncol = 2,
  byrow = TRUE
)))

surveyBoxSI <- st_sf(
  geometry = st_sfc(surveyBoxSI, crs = 4326)
)


# LAND DATA

can <- ne_states(
  country = "Canada",
  returnclass = "sf"
)

us <- ne_states(
  country = "United States of America",
  returnclass = "sf"
)

NBNS_sf <- bind_rows(can, us) %>%
  filter(
    name %in% c(
      "New Brunswick",
      "Nova Scotia",
      "Prince Edward Island",
      "Newfoundland and Labrador",
      "Quebec",
      "Maine"
    )
  )

#German Bank Boundary area (NOT survey area)

GB_bbox <- st_bbox(
  c(
    xmin = -66.5,
    xmax = -65.5,
    ymin = 43.2,
    ymax = 44.0
  ),
  crs = 4326
)

GB_box <- st_as_sfc(GB_bbox)

#German Bank Tags - to fall WITHIN survey area including Seal Island

GermanBankTags <- Tags %>%
  filter((
    Lon >= -66.51 &
    Lon <= -65.229 &
    Lat >= 43.223 &
    Lat <= 43.567
  ) |
    (Lon >= -66.237 & 
    Lon <= -66.119 &
    Lat >= 43.24 &
    Lat <= 43.51
    )
  )
   
GermanBankTagsBeforeAugust <- GermanBankTags %>%
  mutate(Date = ymd(Date)) %>%
  filter(
    month(Date) < 8 |
      (month(Date) == 8 & day(Date) == 1)
  )

returnedTagsGermanBank <- inner_join(
  GermanBankTagsBeforeAugust,
  completeReturns,
  by = c("Tag_Num" = "TAG_NUMBER")
) %>%
  mutate(TaggedArea = "German Bank") %>%
  rename(
    TagReturnArea = catchAREA,
    DateTagged = Date,
    TagReturnDate = DATE
  ) %>%
  select(
    Tag_Num,
    DateTagged,
    TaggedArea,
    TagReturnDate,
    TagReturnArea,
    Lon,
    Lat,
    id,
    dataorigin
  )

# CONVERT TAGS TO SF

GermanBankTags_sf <- st_as_sf(
  GermanBankTags,
  coords = c("Lon", "Lat"),
  crs = 4326,
  remove = FALSE
)

returnedTagsGermanBank <-st_as_sf(
  returnedTagsGermanBank,
  coords = c("Lon", "Lat"),
  crs = 4326,
  remove = FALSE
)

# MAP

GB_Map <- print( ggplot() +
  geom_sf(
    data = NBNS_sf,
    fill = "grey85",
    colour = "black",
    linewidth = 0.2
  ) +
  geom_sf(
    data = GB_box,
    fill = NA,
    colour = "red",
    linewidth = 1
  ) +
  geom_sf(
    data = surveyBoxGB,
    fill = NA,
    colour = "darkgreen",
    linewidth = 1
  ) +
  geom_sf(
    data = surveyBoxSI,
    fill = NA,
    colour = "darkgreen",
    linewidth = 1
  ) +
  geom_sf(
    data = returnedTagsGermanBank,
    colour = "orange",
    size = 1,
    alpha = 0.8
  ) +
  # geom_sf(
  #   data = GermanBankTags_sf,
  #   colour = "blue",
  #   size = 1,
  #   alpha = 0.8
  # ) +
  coord_sf(
    xlim = c(-67, -65),
    ylim = c(43, 44.5),
    expand = FALSE
  ) +
  labs(
    title = "German Bank Tag Deployment Locations",
    subtitle = paste("Tags =", nrow(GermanBankTags)),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw()
)


GermanBankpreAugust1 <- write_csv(returnedTagsGermanBank, paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/GermanBankpreAugust1.csv"))
