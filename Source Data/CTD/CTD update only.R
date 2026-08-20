# =====================================
# APPEND NEW CTD DATA ONLY
# =====================================


rm(list = ls())

library(tidyverse)
library(lubridate)

repo <- file.path(
  path.expand("~"),
  "GitHub",
  "HerringScience.github.io"
)

# =====================================
# Helper functions
# =====================================

mean_na <- function(x){
  if(all(is.na(x))) NA else mean(x, na.rm = TRUE)
}

sd_na <- function(x){
  if(all(is.na(x))) NA else sd(x, na.rm = TRUE)
}

# =====================================
# Import CTD Raw
# =====================================

CTD <- read_csv(
  file.path(repo, "Source Data", "CTD_Raw.csv")
)

CTD <- CTD %>%
  select(
    -any_of(c(
      "Pressure",
      "Conductivity",
      "Specific_conductance",
      "Sound_velocity",
      "Density",
      "plankton_ID"
    ))
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

CTD <- CTD %>%
  mutate(
    Month = month(Date),
    Year = as.numeric(Year)
  )


# =====================================
# Biomass
# =====================================

Bio <- read_csv(
  file.path(repo, "Source Data", "Biomass.csv")
)

CTD <- left_join(
  CTD,
  Bio,
  by = "Date"
)

if("Location" %in% names(CTD)){
  CTD <- CTD %>% select(-Location)
}

CTD <- CTD %>%
  mutate(
    Month = factor(month(Date)),
    Year = factor(Year)
  )

# =====================================
# Station names
# =====================================

CTD <- CTD %>%
  mutate(
    station_name = case_when(
      Ground == "Scots Bay" ~ "GREENWOOD A",
      Ground == "German Bank" ~ "YARMOUTH A",
      TRUE ~ NA_character_
    )
  )

# =====================================
# In Box
# =====================================

CTD <- CTD %>%
  mutate(
    In_Box = case_when(
      Ground == "Scots Bay" &
        between(Lat,45.03,45.08) &
        between(Lon,-65.3,-65.1) ~ "1",
      
      Ground == "German Bank" &
        between(Lat,43.50,43.60) &
        between(Lon,-66.4,-66.3) ~ "1",
      
      TRUE ~ "0"
    )
  )

# =====================================
# Load existing files
# =====================================

OldFull <- read_csv(
  file.path(repo, "Main Data", "CTD Full.csv")
)

OldSST <- read_csv(
  file.path(repo, "Main Data", "CTD SST.csv")
)

Old30 <- read_csv(
  file.path(repo, "Main Data", "CTD 30m.csv")
)


CTD <- CTD %>%
  mutate(
    Year = as.numeric(Year),
    Month = month(Date),
    In_Box = as.character(In_Box)
  )

# =====================================
# Bring historical weather data forward
# =====================================

Weather <- Old30 %>%
  select(
    Date,
    mean_temp,
    total_precip,
    total_snow,
    total_rain,
    spd_max_gust,
    min_temp,
    max_temp,
    heat_deg_days,
    cool_deg_days
  ) %>%
  distinct()

CTD <- CTD %>%
  left_join(Weather, by = "Date")

# =====================================
# Build SST
# =====================================

SST <- CTD %>%
  filter(between(Depth,0,5)) %>%
  filter(Ground %in% c("German Bank","Scots Bay")) %>%
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
    spd_max_gust = max(mean_temp, na.rm = TRUE),
    min_temp = min(mean_temp, na.rm = TRUE),
    max_temp = max(mean_temp, na.rm = TRUE),
    heat_deg_days = mean_na(heat_deg_days),
    cool_deg_days = mean_na(cool_deg_days),
    
    .groups = "drop"
  )

# =====================================
# Build CTD30
# =====================================

CTD30 <- CTD %>%
  filter(between(Depth,28,32)) %>%
  filter(Ground %in% c("German Bank","Scots Bay")) %>%
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
    spd_max_gust = mean_na(spd_max_gust),
    min_temp = mean_na(min_temp),
    max_temp = mean_na(max_temp),
    heat_deg_days = mean_na(heat_deg_days),
    cool_deg_days = mean_na(cool_deg_days),
    
    .groups = "drop"
  )

# =====================================
# Align columns to existing files
# =====================================

for(col in names(OldFull)){
  if(!col %in% names(CTD)){
    CTD[[col]] <- NA
  }
}
CTD <- CTD %>% select(all_of(names(OldFull)))

for(col in names(OldSST)){
  if(!col %in% names(SST)){
    SST[[col]] <- NA
  }
}
SST <- SST %>% select(all_of(names(OldSST)))

for(col in names(Old30)){
  if(!col %in% names(CTD30)){
    CTD30[[col]] <- NA
  }
}
CTD30 <- CTD30 %>% select(all_of(names(Old30)))

# =====================================
# Append only NEW records
# =====================================

NewFull <- anti_join(
  CTD,
  OldFull,
  by = c("ID","Depth","Date")
)

NewFull <- anti_join(
  CTD,
  OldFull,
  by = c("ID", "Depth", "Date")
)

for(col in intersect(names(OldFull), names(NewFull))) {
  
  if(is.numeric(OldFull[[col]])) {
    NewFull[[col]] <- suppressWarnings(
      as.numeric(as.character(NewFull[[col]]))
    )
  }
  
  if(is.character(OldFull[[col]])) {
    NewFull[[col]] <- as.character(NewFull[[col]])
  }
  
}


NewSST <- anti_join(
  SST,
  OldSST,
  by = c("Ground","Date","Survey")
)

for(col in names(OldSST)) {
  
  if(is.numeric(OldSST[[col]])) {
    NewSST[[col]] <- suppressWarnings(
      as.numeric(as.character(NewSST[[col]]))
    )
  }
  
  if(is.character(OldSST[[col]])) {
    NewSST[[col]] <- as.character(NewSST[[col]])
  }
  
}

New30 <- anti_join(
  CTD30,
  Old30,
  by = c("Ground","Date","Survey")
)

for(col in names(Old30)) {
  
  if(is.numeric(Old30[[col]])) {
    New30[[col]] <- suppressWarnings(
      as.numeric(as.character(New30[[col]]))
    )
  }
  
  if(is.character(Old30[[col]])) {
    New30[[col]] <- as.character(New30[[col]])
  }
  
}

UpdatedFull <- bind_rows(
  OldFull,
  NewFull
)

UpdatedSST <- bind_rows(
  OldSST,
  NewSST
)

Updated30 <- bind_rows(
  Old30,
  New30
)

# =====================================
# Write files
# =====================================

write_csv(
  UpdatedFull,
  file.path(repo,"Main Data","CTD Full.csv")
)

write_csv(
  UpdatedSST,
  file.path(repo,"Main Data","CTD SST.csv")
)

write_csv(
  Updated30,
  file.path(repo,"Main Data","CTD 30m.csv")
)

cat("New records appended. Existing records unchanged.\n")
`