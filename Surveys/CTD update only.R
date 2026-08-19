# Remove everything from workspace
rm(list = ls())

# Libraries
library(tidyverse)
library(lubridate)
library(dplyr)

# Repository location
repo <- file.path(
  path.expand("~"),
  "GitHub",
  "HerringScience.github.io"
)

# -----------------------------
# Helper functions
# -----------------------------

mean_na <- function(x) {
  if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
}

sd_na <- function(x) {
  if (all(is.na(x))) NA else sd(x, na.rm = TRUE)
}

max_na <- function(x) {
  if (all(is.na(x))) NA else max(x, na.rm = TRUE)
}

min_na <- function(x) {
  if (all(is.na(x))) NA else min(x, na.rm = TRUE)
}

# -----------------------------
# Import CTD data
# -----------------------------

CTD <- read_csv(
  file.path(repo, "Source Data", "CTD_Raw.csv")
)

CTD <- CTD %>%
  select(
    -Pressure,
    -Conductivity,
    -Specific_conductance,
    -Sound_velocity,
    -Density,
    -plankton_ID
  )

CTD$Date <- ymd(CTD$Date)

CTD <- CTD %>%
  mutate(
    Julian = yday(Date)
  )

CTD <- CTD %>%
  rename(
    Ground = ground,
    ID = id
  )

# -----------------------------
# Join biomass data
# -----------------------------

Bio <- read_csv(
  file.path(repo, "Source Data", "Biomass.csv")
)

CTD <- left_join(
  CTD,
  Bio,
  by = "Date"
)

if ("Location" %in% names(CTD)) {
  CTD <- CTD %>% select(-Location)
}

CTD <- CTD %>%
  mutate(
    Month = factor(month(Date)),
    Year = factor(Year)
  )

# -----------------------------
# Weather placeholders
# -----------------------------

CTD$station_name <- NA_character_
CTD$mean_temp <- NA_real_
CTD$total_precip <- NA_real_
CTD$total_snow <- NA_real_
CTD$total_rain <- NA_real_
CTD$spd_max_gust <- NA_real_
CTD$min_temp <- NA_real_
CTD$max_temp <- NA_real_
CTD$heat_deg_days <- NA_real_
CTD$cool_deg_days <- NA_real_

CTD <- CTD %>%
  mutate(
    station_name = case_when(
      Ground == "Scots Bay" ~ "GREENWOOD A",
      Ground == "German Bank" ~ "YARMOUTH",
      TRUE ~ NA_character_
    )
  )

# -----------------------------
# In-box indicator
# -----------------------------

CTD <- CTD %>%
  mutate(
    In_Box = case_when(
      Ground == "Scots Bay" &
        between(Lat, 45.03, 45.08) &
        between(Lon, -65.3, -65.1) ~ "1",
      
      Ground == "German Bank" &
        between(Lat, 43.50, 43.60) &
        between(Lon, -66.4, -66.3) ~ "1",
      
      TRUE ~ "0"
    )
  )

CTD$In_Box <- factor(CTD$In_Box)

# -----------------------------
# SST (0-5m)
# -----------------------------

SST <- CTD %>%
  filter(between(Depth, 0, 5)) %>%
  group_by(
    Ground,
    Date,
    Year,
    Julian,
    Month,
    Survey,
    In_Box
  ) %>%
  summarize(
    TempSD = sd_na(Temperature),
    Temperature = mean_na(Temperature),
    Biomass = mean_na(Biomass),
    logTemp = log(Temperature),
    Lat = mean_na(Lat),
    Lon = mean_na(Lon),
    logBiomass = log(Biomass),
    SalinitySD = sd_na(Salinity),
    Salinity = mean_na(Salinity),
    mean_temp = mean_na(mean_temp),
    total_precip = mean_na(total_precip),
    total_snow = mean_na(total_snow),
    total_rain = mean_na(total_rain),
    spd_max_gust = max_na(spd_max_gust),
    min_temp = min_na(min_temp),
    max_temp = max_na(max_temp),
    heat_deg_days = mean_na(heat_deg_days),
    cool_deg_days = mean_na(cool_deg_days),
    .groups = "drop"
  )

SST <- SST %>%
  group_by(Year, Month, Ground) %>%
  mutate(
    Count = n()
  ) %>%
  ungroup()

# -----------------------------
# CTD 30m (28-32m)
# -----------------------------

CTD30 <- CTD %>%
  filter(between(Depth, 28, 32)) %>%
  group_by(
    Ground,
    Date,
    Year,
    Julian,
    Month,
    Survey,
    In_Box
  ) %>%
  summarize(
    TempSD = sd_na(Temperature),
    Temperature = mean_na(Temperature),
    Biomass = mean_na(Biomass),
    logTemp = log(Temperature),
    Lat = mean_na(Lat),
    Lon = mean_na(Lon),
    logBiomass = log(Biomass),
    SalinitySD = sd_na(Salinity),
    Salinity = mean_na(Salinity),
    mean_temp = mean_na(mean_temp),
    total_precip = mean_na(total_precip),
    total_snow = mean_na(total_snow),
    total_rain = mean_na(total_rain),
    spd_max_gust = max_na(spd_max_gust),
    min_temp = min_na(min_temp),
    max_temp = max_na(max_temp),
    heat_deg_days = mean_na(heat_deg_days),
    cool_deg_days = mean_na(cool_deg_days),
    .groups = "drop"
  )

CTD30 <- CTD30 %>%
  group_by(Year, Month, Ground) %>%
  mutate(
    Count = n()
  ) %>%
  ungroup()

# -----------------------------
# Stratification
# -----------------------------

SSTTemp <- SST %>%
  select(
    Date,
    Temperature,
    Salinity
  )

Strat <- left_join(
  CTD30,
  SSTTemp,
  by = "Date"
)

Strat <- Strat %>%
  rename(
    Temperature = Temperature.x,
    SST = Temperature.y,
    Salinity = Salinity.x,
    SurfaceSalinity = Salinity.y
  )

Strat <- Strat %>%
  mutate(
    StratTemp = SST - Temperature,
    StratSalt = Salinity - SurfaceSalinity
  )

CTD30 <- Strat %>%
  group_by(
    Ground,
    Date,
    Year,
    Julian,
    Month,
    Survey,
    In_Box
  ) %>%
  summarize(
    TempSD = sd_na(Temperature),
    Temperature = mean_na(Temperature),
    Biomass = mean_na(Biomass),
    logTemp = log(Temperature),
    Lat = mean_na(Lat),
    Lon = mean_na(Lon),
    logBiomass = log(Biomass),
    SalinitySD = sd_na(Salinity),
    Salinity = mean_na(Salinity),
    SST = mean_na(SST),
    SurfaceSalinity = mean_na(SurfaceSalinity),
    StratTemp = mean_na(StratTemp),
    StratSalt = mean_na(StratSalt),
    .groups = "drop"
  )

# -----------------------------
# Export files
# -----------------------------

# write_csv(
#   CTD,
#   file.path(repo, "Main Data", "CTD Full.csv")
# )
# 
# write_csv(
#   SST,
#   file.path(repo, "Main Data", "CTD SST.csv")
# )
# 
# write_csv(
#   CTD30,
#   file.path(repo, "Main Data", "CTD 30m.csv")
# )
# 
# cat("CTD Full, CTD SST, and CTD 30m successfully updated.\n")
