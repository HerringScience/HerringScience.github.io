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

# Scots Bay plankton and CTD box
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]

# Scots Bay
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

#German Bank CTD box
GBCTD=boxes[which(boxes$Box == "GBocean"), ]

# German Bank
SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")

# Seal Island
SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")

#Land Data

can <- geodata::gadm(
  country = "CAN",
  level = 1,
  path = tempdir()
)

can_sf <- st_as_sf(can)

NBNS <- can_sf %>%
  filter(
    NAME_1 %in% c(
      "New Brunswick",
      "Nova Scotia",
      "Prince Edward Island",
      "Newfoundland and Labrador",
      "Quebec"
    )
  )

study_area <- st_as_sfc(
  st_bbox(
    c(
      xmin = -67.6,
      xmax = -64.0,
      ymin = 43.0,
      ymax = 45.8
    ),
    crs = 4326
  )
)

maritimes_crop <- st_intersection(
  NBNS,
  study_area
)

# Run first few lines of taggingMaster first to load in relINFO

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Tagging"))

relINFO <- read.csv("relINFO.csv")

head(relINFO)

relINFO_map <- relINFO %>%
  filter(
    !is.na(X),
    !is.na(Y),
    between(X, -70, -60),
    between(Y, 40, 50)
  )

relINFO_sf <- st_as_sf(
  relINFO_map,
  coords = c("X", "Y"),
  crs = 4326
)
bad_coords <- relINFO %>%
  filter(
    is.na(X) |
      is.na(Y) |
      !between(X, -70, -60) |
      !between(Y, 40, 50)
  )

View(bad_coords)

boxes_sf <- boxes %>%
  group_by(Box) %>%
  summarise(
    do_union = FALSE
  ) %>%
  st_as_sf()
ggplot() +
  
  geom_sf(
    data = maritimes_crop,
    fill = "grey80",
    colour = "black"
  ) +
  
  geom_sf(
    data = relINFO_sf,
    aes(colour = Year),
    size = 1
  ) +
  
  theme_bw() +
  
  labs(
    title = "Tag Release Locations by Year"
  )

outside_sf <- st_as_sf(
  outside2,
  coords = c("X","Y"),
  crs = 4326
)

ggplot() +
  
  geom_sf(
    data = maritimes_crop,
    fill = "grey80"
  ) +
  
  geom_sf(
    data = outside_sf,
    aes(colour = Tagger),
    size = 3
  ) +
  
  theme_bw() +
  
  labs(
    title = "Potential Coordinate Errors"
  )

library(leaflet)

leaflet(two23) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X,
    lat = ~Y,
    color = "red",
    radius = 4,
    popup = ~paste(
      Tagger,
      "<br>",
      RELEASE_DATE,
      "<br>",
      no_tags
    )
  )

# ------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------

repo <- "C:/Users/herri/Documents/GitHub/HerringScience.github.io"

box_path <- file.path(repo, "Box Coordinates")
tag_path <- file.path(repo, "Source Data", "Tagging", "Tag Returns")
source_path <- file.path(repo, "Source Data")

# ------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------

boxes <- read.csv(
  file.path(box_path, "surveyBoxes.csv")
)

relINFO <- read.csv(
  file.path(tag_path, "relINFO.csv")
)

TaggingEvents <- read.csv(
  file.path(source_path, "TaggingEvents.csv")
)

# ------------------------------------------------------------------
# Format Dates
# ------------------------------------------------------------------

relINFO$RELEASE_DATE <- as.Date(relINFO$RELEASE_DATE)

relINFO <- relINFO %>%
  mutate(
    Year = as.character(Year),
    month = month(RELEASE_DATE)
  )

# ------------------------------------------------------------------
# Split Years
# ------------------------------------------------------------------

two18 <- filter(relINFO, Year == "2018")
two19 <- filter(relINFO, Year == "2019")
two20 <- filter(relINFO, Year == "2020")
two21 <- filter(relINFO, Year == "2021")
two22 <- filter(relINFO, Year == "2022")
two23 <- filter(relINFO, Year == "2023")
two24 <- filter(relINFO, Year == "2024")

# ------------------------------------------------------------------
# Year Summary
# ------------------------------------------------------------------

year_summary <- relINFO %>%
  group_by(Year) %>%
  summarise(
    TotalTags = sum(no_tags, na.rm = TRUE),
    Events = n(),
    .groups = "drop"
  )

print(year_summary)

datatable(year_summary)

# ------------------------------------------------------------------
# 2023 Summary
# ------------------------------------------------------------------

head(two23)

sum(two23$no_tags)

unique(two23$RELEASE_VESSEL)

# ------------------------------------------------------------------
# Vessel Summary
# ------------------------------------------------------------------

vessel_summary <- two23 %>%
  group_by(RELEASE_VESSEL) %>%
  summarise(
    TotalTags = sum(no_tags, na.rm = TRUE),
    Events = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalTags))

print(vessel_summary)

datatable(vessel_summary)

# ------------------------------------------------------------------
# Tags Per Month
# ------------------------------------------------------------------

tags_per_month <- two23 %>%
  group_by(month) %>%
  summarise(
    TotalTags = sum(no_tags, na.rm = TRUE),
    .groups = "drop"
  )

datatable(tags_per_month)

ggplot(tags_per_month,
       aes(month, TotalTags)) +
  geom_point(size = 5,
             colour = "red") +
  theme_bw(base_size = 16) +
  labs(
    title = "Tags Applied Per Month (2023)",
    x = "Month",
    y = "Tags"
  )

# ------------------------------------------------------------------
# Tags Per Tagger
# ------------------------------------------------------------------

tagger_summary <- two23 %>%
  group_by(Tagger) %>%
  summarise(
    TotalTags = sum(no_tags, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalTags))

datatable(tagger_summary)

ggplot(tagger_summary,
       aes(reorder(Tagger, TotalTags),
           TotalTags)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_bw(14) +
  labs(
    title = "Tags Applied Per Tagger",
    x = "",
    y = "Tags Applied"
  )

# ------------------------------------------------------------------
# Tags Per Year
# ------------------------------------------------------------------

year_totals <- relINFO %>%
  group_by(Year) %>%
  summarise(
    TotalTags = sum(no_tags, na.rm = TRUE),
    .groups = "drop"
  )

datatable(year_totals)

ggplot(year_totals,
       aes(Year, TotalTags)) +
  geom_col(fill = "blue") +
  theme_bw(base_size = 16) +
  labs(
    title = "Tags Applied Per Year",
    x = "Year",
    y = "Total Tags"
  )

# ------------------------------------------------------------------
# Tags Per Event
# ------------------------------------------------------------------

ggplot(two23,
       aes(Julian,
           no_tags,
           colour = Tagger)) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(150, 300, 10)
  ) +
  theme_bw() +
  labs(
    title = "2023 Tagging Events",
    x = "Julian Day",
    y = "Tags"
  )

# ------------------------------------------------------------------
# Coordinate QC
# ------------------------------------------------------------------

outliers <- relINFO %>%
  filter(
    X > -65.5 |
      Y < 44 |
      Y > 45.5
  )

datatable(outliers)

# ------------------------------------------------------------------
# Tagging Events
# ------------------------------------------------------------------

TaggingEvents$Date <- as.Date(TaggingEvents$Date)

TaggingEvents <- TaggingEvents %>%
  filter(Year == "2023") %>%
  mutate(
    Month = month(Date)
  )

# ------------------------------------------------------------------
# Events Per Month / Tagger
# ------------------------------------------------------------------

TaggingEventsStats <- TaggingEvents %>%
  group_by(Tagger, Month) %>%
  summarise(
    Events = n(),
    .groups = "drop"
  )

datatable(TaggingEventsStats)

ggplot(
  TaggingEventsStats,
  aes(
    x = Month,
    y = Events,
    fill = Tagger
  )
) +
  geom_col() +
  theme_bw() +
  labs(
    title = "Tagging Events by Month",
    x = "Month",
    y = "Events"
  )

