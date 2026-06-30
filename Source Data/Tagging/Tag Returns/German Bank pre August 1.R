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
library(sf)
library(rnaturalearth)
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

#Scots Bay main, northern and eastern box
surveyBoxSBEastern <-   
  st_polygon(list(matrix(
    c(
      -64.682, 45.218,
      -64.5499, 45.255,
      -64.549, 45.34,
      -64.765, 45.276,
      -64.682, 45.218
    ),
    ncol = 2,
    byrow = TRUE
  )))

surveyBoxSBEastern <- st_sf(
  geometry = st_sfc(surveyBoxSBEastern, crs = 4326)
)

surveyBoxSBNorthern <-   
  st_polygon(list(matrix(
    c(
      -65.0599, 45.293,
      -65.054, 45.247,
      -64.83, 45.314,
      -64.88, 45.344,
      -65.0599, 45.293
    ),
    ncol = 2,
    byrow = TRUE
  )))

surveyBoxSBNorthern <- st_sf(
  geometry = st_sfc(surveyBoxSBNorthern, crs = 4326)
)

surveyBoxSB <-   
  st_polygon(list(matrix(
    c(
      -64.673, 45.209,
      -65.82, 45.318,
      -65.239, 45.175,
      -65.239, 45.023,
      -64.673, 45.209
    ),
    ncol = 2,
    byrow = TRUE
  )))

surveyBoxSB <- st_sf(
  geometry = st_sfc(surveyBoxSB, crs = 4326)
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
  dplyr::select(
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

#Tags Returned to German Bank that were tagged Aug 1st or sooner

OtherTags <- Tags %>%
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
  dplyr::select(
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


#GermanBankpreAugust1 <- write_csv(returnedTagsGermanBank, paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/GermanBankpreAugust1.csv"))
GermanBankTagsBeforeAugust <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/GermanBankpreAugust1.csv")
# Remove records with missing return coordinates
tags_lines <- GermanBankTagsBeforeAugust %>%
  filter(
    !is.na(TaggedLon),
    !is.na(TaggedLat),
    !is.na(ReturnedLon),
    !is.na(ReturnedLat)
  )

# Create LINESTRING geometries
line_geom <- lapply(seq_len(nrow(tags_lines)), function(i) {
  st_linestring(matrix(
    c(
      tags_lines$TaggedLon[i],  tags_lines$TaggedLat[i],
      tags_lines$ReturnedLon[i], tags_lines$ReturnedLat[i]
    ),
    ncol = 2,
    byrow = TRUE
  ))
})

# Convert to sf object
tag_tracks <- st_sf(
  tags_lines,
  geometry = st_sfc(line_geom, crs = 4326)
)

# Plot
ggplot() +
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
  geom_sf(data = tag_tracks,
          colour = "blue",
          linewidth = 0.5,
          alpha = 0.6) +
  geom_point(data = tags_lines,
             aes(x = TaggedLon, y = TaggedLat),
             colour = "green", size = 2) +
  geom_point(data = tags_lines,
             aes(x = ReturnedLon, y = ReturnedLat),
             colour = "red", size = 2) +
  coord_sf(
    xlim = c(-65.3, -68),
    ylim = c(43, 45.4)
  ) +
  theme_minimal() +
  labs(
    title = "Tag Release and Return Locations",
    subtitle = "Lines connect tagging locations to return locations",
    x = "Longitude",
    y = "Latitude"
  )

#Individual maps per tag
# Create lines
tags_lines <- GermanBankTagsBeforeAugust %>%
  filter(
    !is.na(TaggedLon),
    !is.na(TaggedLat),
    !is.na(ReturnedLon),
    !is.na(ReturnedLat)
  )

line_geom <- lapply(seq_len(nrow(tags_lines)), function(i) {
  st_linestring(matrix(
    c(
      tags_lines$TaggedLon[i], tags_lines$TaggedLat[i],
      tags_lines$ReturnedLon[i], tags_lines$ReturnedLat[i]
    ),
    ncol = 2,
    byrow = TRUE
  ))
})

tag_tracks <- st_sf(
  tags_lines,
  geometry = st_sfc(line_geom, crs = 4326)
)

# Create one plot per tag
print(
  tag_maps <- split(tag_tracks, tag_tracks$Tag_Num) %>%
    imap(function(dat, tag_id) {
    
    ggplot() +
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
        data = dat,
        colour = "blue",
        linewidth = 0.8
      ) +
      geom_point(
        data = dat,
        aes(x = TaggedLon, y = TaggedLat, colour = "Tagged Locations"),
        size = 3
      ) +
      geom_point(
        data = dat,
        aes(x = ReturnedLon, y = ReturnedLat, colour = "Recaptured Location"),
        size = 3
      ) +
      coord_sf(
        xlim = c(-68, -65.3),
        ylim = c(43, 45.4)
      ) +
        theme_minimal() +
        theme(
          legend.position = "right"
        ) +
        labs(
          title = paste("Tag", tag_id),
          subtitle = paste0(
            "Tagged: ", format(dat$DateTagged[1], "%Y-%m-%d"),
            " | Returned: ", format(dat$TagReturnDate[1], "%Y-%m-%d")
          )
        )
}))

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
  dplyr::select(
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

#Tags Returned to German Bank that were tagged Aug 1st or sooner

OtherTags <- Tags %>%
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
  dplyr::select(
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