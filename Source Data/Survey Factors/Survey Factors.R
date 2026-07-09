# remove everything in the workspace
rm(list = ls())

#Libraries
library(rlang)
library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
library(weathercan)
library(GGally)
library(psych)
library(sp)
library(raster)
library(PBSmapping)
library(rgeos) # replaced by terra and sf
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(readxl)
library(hms)
library(measurements)
library(ggplot2)
library(maps)
library(dplyr)
library(RColorBrewer)
library(AICcmodavg)
library(datasets)
library(terra)
library(multcompView)
library(sf)
library(zoo)
library(ggrepel)
library(tidyr)
library(suncalc)
library(jsonlite)
library(httr2)
library(rvest)
library(purrr)
library(xml2)
library(oce)
library(jsonlite)
library(httr)

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Survey Factors")

#Updating survey Factors with rest of info

Survey_Factors <- read_csv("surveyFactorsAll_Tracey with SSB data.csv") %>%
  mutate(
    Survey_Date = as.Date(Survey_Date),
    Julian = yday(Survey_Date),
    Survey_Number = as.character(Survey_Number)
  )

#Use these dataframes to update survey factors.

SSBEstimates <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/SSB Estimates.csv") %>%
  mutate(
    Survey_Number = as.character(Survey_Number),
    Ground = case_when(
      Ground == "Scots Bay" ~ "SB",
      Ground == "German Bank" ~ "GB"
    )) %>%
      filter(Ground != "Seal Island")
  

Survey_Data <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv") %>%
  mutate(
    Survey.No = as.character(Survey.No),
  )
Survey_Data$Date <- dmy(Survey_Data$Date)

Survey_Data2 <- Survey_Data %>%
  rename(
    Survey_Number = Survey.No,
    Survey_Date = Date,
    No_of_Vessels = Vessel.No,
    Survey_Start = StartTime
  ) %>%
  mutate(
    Survey_Number = as.character(Survey_Number),
    No_of_Vessels = as.numeric(No_of_Vessels),
    Ground = recode(
      Ground,
      "Scots Bay" = "SB",
      "German Bank" = "GB"
    )
  )


#Add new information

# Find surveys present in SSBEstimates but not in Survey_Factors
NewRows <- SSBEstimates %>%
  anti_join(
    Survey_Factors,
    by = c(
      "Year",
      "Survey_Date"
    )
  )

RowsToAdd <- Survey_Factors[rep(1, nrow(NewRows)), ]

RowsToAdd[,] <- NA
names(RowsToAdd) <- names(Survey_Factors)


# Fill columns that exist in SSBEstimates
RowsToAdd$Survey_Date <- NewRows$Survey_Date
RowsToAdd$Survey_Number <- NewRows$Survey_Number
RowsToAdd$Year <- NewRows$Year
RowsToAdd$Ground <- NewRows$Ground
RowsToAdd$HSC_Estimate <- NewRows$HSC_Estimate
RowsToAdd$DFO_Estimate <- NewRows$DFO_Estimate
RowsToAdd$DFO_Turnover_Adjusted <- NewRows$DFO_Turnover_Adjusted
RowsToAdd$HSC_Turnover <- NewRows$HSC_Turnover_Adjusted


## Append to Survey_Factors
Survey_Factors <- bind_rows(
  Survey_Factors,
  RowsToAdd
)


## Add ID to rows
max_id <- max(Survey_Factors$id, na.rm = TRUE)

missing_rows <- which(is.na(Survey_Factors$id))

Survey_Factors$id[missing_rows] <- seq(
  from = max_id + 1,
  length.out = length(missing_rows)
)

#This will delete the extra tow from surveys within Survey_Data2
Survey_Data2 <- Survey_Data2 %>%
  filter(Tow_No == 1)

# Fill columns that exist in Survey Data 

Survey_Factors <- Survey_Factors %>%
  left_join(
    Survey_Data2 %>%
      dplyr::select(
        Year,
        Ground,
        Survey_Number,
        Survey_Date,
        No_of_Vessels,
        Survey_Start
      ),
    by = c(
      "Year",
      "Ground",
      "Survey_Number",
      "Survey_Date"
    ),
    suffix = c("", "_new")
  ) %>%
  mutate(
    No_of_Vessels = coalesce(
      No_of_Vessels,
      No_of_Vessels_new
    ),
    Survey_Start = coalesce(
      as.character(Survey_Start),
      as.character(Survey_Start_new)
    )
    ) %>%
  dplyr::select(-No_of_Vessels_new, -Survey_Start_new)
  

#This is if all new surveys added are STRUCTURED.

Survey_Factors <- Survey_Factors %>%
  mutate(
    Survey_Type = ifelse(
      is.na(Survey_Type),
      "Structured",
      Survey_Type
    )
  )

#Add in Survey Started Column which is a combo of date and start time.

Survey_Factors <- Survey_Factors %>%
  mutate(
    `Survey Started` = if_else(
      is.na(`Survey Started`),
      paste(Survey_Date, Survey_Start),
      as.character(`Survey Started`)
    )
  )

#Add Julian Dates
Survey_Factors <- Survey_Factors %>%
  mutate(
    Julian = coalesce(
      Julian,
      yday(Survey_Date)
    )
  )



#Sunset times

# Coordinates
# Margaretsville NS (Scots Bay)
SB_lat <- 45.048029
SB_lon <- -65.064777

# Yarmouth NS (German Bank)
GB_lat <- 43.8378
GB_lon <- -66.1174


# Create lookup table of unique survey dates and grounds
SunsetLookup <- Survey_Factors %>%
  dplyr::select(Survey_Date, Ground) %>%
  distinct() %>%
  mutate(
    lat = case_when(
      Ground == "SB" ~ SB_lat,
      Ground == "GB" ~ GB_lat
    ),
    lon = case_when(
      Ground == "SB" ~ SB_lon,
      Ground == "GB" ~ GB_lon
    )
  )

# Calculate sunset for each date/location
SunsetTimes <- SunsetLookup %>%
  rowwise() %>%
  mutate(
    sunset = getSunlightTimes(
      date = Survey_Date,
      lat = lat,
      lon = lon,
      keep = c("sunset"),
      tz = "America/Halifax"
    )$sunset
  ) %>%
  ungroup() %>%
  mutate(
    Sunset_Time_New = format(sunset, "%H:%M:%S")
  ) %>%
  dplyr::select(
    Survey_Date,
    Ground,
    Sunset_Time_New
  )


SunsetTimes <- SunsetTimes %>%
  mutate(
    Sunset_Time_New = as.POSIXct(
      paste(Survey_Date, Sunset_Time_New),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "America/Halifax"
    )
  )

#Missing Sunset times

SunsetTimes <- SunsetTimes %>%
  mutate(
    Sunset_Time_New = format(
      Sunset_Time_New,
      "%H:%M:%S"
    )
  )

Survey_Factors$Sunset_Time <- as.character(Survey_Factors$Sunset_Time)

Survey_Factors <- Survey_Factors %>%
  left_join(
    SunsetTimes,
    by = c("Survey_Date", "Ground")
  ) %>%
  mutate(
    Sunset_Time = coalesce(
      Sunset_Time,
      Sunset_Time_New
    )
  ) %>%
  dplyr::select(-Sunset_Time_New)

#Add in Survey Started Column which is a combo of date and start time.

Survey_Factors <- Survey_Factors %>%
  mutate(
    `Sunset Time` = coalesce(
      as.character(`Sunset Time`),
      paste(as.character(Survey_Date), as.character(Sunset_Time))
    )
  )

#sunset difference and relative
Survey_Factors <- Survey_Factors %>%
  mutate(
    Survey_Started_dt = suppressWarnings(
      parse_date_time(
        `Survey Started`,
        orders = c("Ymd HMS", "Ymd HM"),
        tz = "America/Halifax"
      )
    ),
    
    Sunset_dt = suppressWarnings(
      parse_date_time(
        `Sunset Time`,
        orders = c("Ymd HMS", "Ymd HM"),
        tz = "America/Halifax"
      )
    )
  ) %>%
  mutate(
    Sunset_Diff_Secs = as.numeric(
      difftime(
        Sunset_dt,
        Survey_Started_dt,
        units = "secs"
      )
    ),
    
    Sunset_Difference = case_when(
      is.na(Sunset_Difference) &
        !is.na(Sunset_Diff_Secs) ~
        
        sprintf(
          "%s%02d:%02d:%02d",
          ifelse(Sunset_Diff_Secs < 0, "-", "+"),
          floor(abs(Sunset_Diff_Secs) / 3600),
          floor((abs(Sunset_Diff_Secs) %% 3600) / 60),
          floor(abs(Sunset_Diff_Secs) %% 60)
        ),
      
      TRUE ~ as.character(Sunset_Difference)
    ),
    
    Sunset_Relative = case_when(
      is.na(Sunset_Relative) &
        !is.na(Sunset_Diff_Secs) ~
        
        as.character(
          round(Sunset_Diff_Secs / 3600, 2)
        ),
      
      TRUE ~ as.character(Sunset_Relative)
    )
  ) %>%
  dplyr::select(
    -Survey_Started_dt,
    -Sunset_dt,
    -Sunset_Diff_Secs
  )


##Tide information

# Start and end date

na_dates <- Survey_Factors %>%
  filter(is.na(High_Tide)) %>%
  pull(Survey_Date)

start_dt <- as.POSIXct(
  min(na_dates),
  tz = "America/Halifax"
)

end_dt <- as.POSIXct(
  max(na_dates) + 2,
  tz = "America/Halifax"
) - 1 

start_date <- format(
  start_dt,
  "%Y-%m-%dT%H:%M:%S%z"
)

end_date <- format(
  end_dt,
  "%Y-%m-%dT%H:%M:%S%z"
)

# Station IDs
marg_id <- "5cebf1df3d0f4a073c4bbc68"

yarm_id <- "5cebf1df3d0f4a073c4bbc8d"

# Import station info from CHS website

get_tides <- function(station_id,
                      start_date,
                      end_date,
                      series = "wlp-hilo") {
  
  url <- paste0(
    "https://api-iwls.dfo-mpo.gc.ca/api/v1/stations/",
    station_id,
    "/data?",
    "time-series-code=", series,
    "&from=", start_date,
    "&to=", end_date
  )
  
  res <- httr::GET(url)
  
  if (httr::status_code(res) != 200) {
    stop(httr::content(res, "text", encoding = "UTF-8"))
  }
  
  jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )
}

#Download tides
SB_tides <- get_tides(
  station_id = marg_id,
  start_date = start_date,
  end_date = end_date,
  series = "wlp-hilo"
)

GB_tides <- get_tides(
  station_id = yarm_id,
  start_date = start_date,
  end_date = end_date,
  series = "wlp-hilo"
)

#Convert to Dataframes

SB_tides <- as.data.frame(SB_tides)
GB_tides <- as.data.frame(GB_tides)

SB_tides$Ground <- "SB"
GB_tides$Ground <- "GB"

tides <- bind_rows(SB_tides, GB_tides)

tides$eventDate <- lubridate::ymd_hms(
  tides$eventDate,
  tz = "UTC"
)


#Convert timestamps
tides$eventDate_Local <- with_tz(
  tides$eventDate,
  tzone = "America/Halifax"
)

tides$Survey_Date <- as.Date(tides$eventDate_Local)

# Keep all high tides

high_tides <- tides %>%
  arrange(Ground, eventDate_Local) %>%
  group_by(Ground) %>%
  filter(
    value > lag(value) &
      value > lead(value)
  ) %>%
  ungroup() %>%
  dplyr::select(
    Ground,
    High_Tide_Time = eventDate_Local,
    High_Tide_Height = value
  )

# Identify high tides as local maxima
high_tides <- tides %>%
  arrange(Ground, eventDate_Local) %>%
  group_by(Ground) %>%
  mutate(
    High_Flag =
      value > lag(value) &
      value > lead(value)
  ) %>%
  ungroup() %>%
  filter(High_Flag) %>%
  dplyr::select(
    Ground,
    High_Tide_Time = eventDate_Local,
    High_Tide_Height = value
  )

missing_surveys <- Survey_Factors %>%
  filter(is.na(High_Tide)) %>%
  mutate(
    Survey_Started = parse_date_time(
      paste(Survey_Date, Survey_Start),
      orders = c("Ymd HMS", "Ymd HM"),
      tz = "America/Halifax"
    )
  )

nearest_high_tide <- function(survey_time, ground, high_tides_df) {
  
  candidate_tides <- high_tides_df %>%
    filter(Ground == ground)
  
  idx <- which.min(
    abs(
      as.numeric(
        difftime(
          candidate_tides$High_Tide_Time,
          survey_time,
          units = "mins"
        )
      )
    )
  )
  
  candidate_tides[idx, ]
}

nearest_high <- purrr::map2_dfr(
  missing_surveys$Survey_Started,
  missing_surveys$Ground,
  ~nearest_high_tide(.x, .y, high_tides)
)

missing_surveys <- bind_cols(
  missing_surveys,
  nearest_high
)

missing_surveys <- missing_surveys %>%
  mutate(
    Hours_From_High_Tide =
      as.numeric(
        difftime(
          Survey_Started,
          High_Tide_Time,
          units = "hours"
        )
      )
  )


#Update Survey_Factors

Survey_Factors <- Survey_Factors %>%
  left_join(
    missing_surveys %>%
      dplyr::select(
        id,
        High_Tide_Time
      ),
    by = "id"
  )

Survey_Factors <- Survey_Factors %>%
  mutate(
    `High Tide` = if_else(
      is.na(`High Tide`) & !is.na(High_Tide_Time),
      format(High_Tide_Time, "%Y-%m-%d %H:%M:%S"),
      `High Tide`
    )
  ) %>%
  dplyr::select(-High_Tide_Time)

Survey_Factors <- Survey_Factors %>%
  mutate(
    High_Tide = if_else(
      is.na(High_Tide),
      substr(`High Tide`, 12, 19),
      High_Tide
    )
  )

#Tide-Difference and Tide_Relative

Survey_Factors <- Survey_Factors %>%
  mutate(
    Tide_Difference = as.character(Tide_Difference),
    Tide_Relative = as.character(Tide_Relative)
  )


Survey_Factors <- Survey_Factors %>%
  mutate(
    Survey_Started_dt = suppressWarnings(
      parse_date_time(
        `Survey Started`,
        orders = c("Ymd HMS", "Ymd HM"),
        tz = "America/Halifax"
      )
    ),
    
    High_Tide_dt = suppressWarnings(
      parse_date_time(
        `High Tide`,
        orders = c("Ymd HMS", "Ymd HM"),
        tz = "America/Halifax"
      )
    )
  ) %>%
  mutate(
    Tide_Diff_Secs = as.numeric(
      difftime(
        High_Tide_dt,
        Survey_Started_dt,
        units = "secs"
      )
    ),
    
    Tide_Difference = if_else(
      is.na(Tide_Difference) &
        !is.na(Tide_Diff_Secs),
      
      sprintf(
        "%s%02d:%02d:%02d",
        if_else(Tide_Diff_Secs < 0, "-", "+"),
        floor(abs(Tide_Diff_Secs) / 3600),
        floor((abs(Tide_Diff_Secs) %% 3600) / 60),
        floor(abs(Tide_Diff_Secs) %% 60)
      ),
      
      Tide_Difference
    ),
    
    Tide_Relative = if_else(
      is.na(Tide_Relative) &
        !is.na(Tide_Diff_Secs),
      
      as.character(
        round(Tide_Diff_Secs / 3600, 2)
      ),
      
      Tide_Relative
    )
  ) %>%
  dplyr::select(
    -Survey_Started_dt,
    -High_Tide_dt,
    -Tide_Diff_Secs
  )


#Write new dataframe

write.csv(
  Survey_Factors,
  "surveyFactorsAll.csv",
  row.names = FALSE
)
