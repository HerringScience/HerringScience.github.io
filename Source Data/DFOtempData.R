

# Load DFO data and format to get ready to add to CTD_Raw
# turn into a function once complete and QCed

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
library(oce)
library(gsw)
library(dplyr)


# this is CTD_Raw

CTD = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv"))

#load DFO temp data from 2009 to start
# this is all data, not isolated to the boxes
DFO = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/herringLarvalSurvey2009Data.csv"))


DFO$latitude = as.numeric(DFO$latitude)
DFO$longitude = as.numeric(DFO$longitude)
DFO$Lat = DFO$latitude
DFO$Lat = as.numeric(DFO$Lat)
DFO$Lon = DFO$longitude
DFO$Lon = as.numeric(DFO$Lon)

DFO$latitude = NULL
DFO$longitude = NULL
DFO$time = as.POSIXct(DFO$time)
DFO$Date <- paste(DFO$year, DFO$month, DFO$day, sep = "-")
DFO$year = NULL
DFO$month = NULL
DFO$day = NULL
DFO$Date = as.Date(DFO$Date)
DFO$datatype = as.factor(DFO$datatype)
DFO$cruise_id = as.factor(DFO$cruise_id)
DFO$Depth = swDepth(DFO$pressure, DFO$Lat)
DFO$Temperature = DFO$temperature
DFO$temperature = NULL
DFO$Salinity = DFO$salinity
DFO$salinity = NULL
DFO$Pressure = DFO$pressure
DFO$pressure = NULL

electrical_conductivity <- function(T, S, depth, lat) {
  # depth: meters
  # latitude: degrees (needed for pressure conversion)
  
  # Step 1: depth → pressure (dbar)
  P <- swPressure(depth, lat)
  
  # Step 2: conductivity ratio (dimensionless)
  R <- swCSTp(S, T, P)
  
  # Step 3: convert ratio → electrical conductivity (mS/cm)
  # Standard seawater conductivity = 42.914 mS/cm
  C_mScm <- R * 42.914
  
  # Return conductivity in µS/cm (most CTDs use this)
  C_uScm <- C_mScm * 1000
  
  return(C_uScm)
}


DFO$Conductivity <- electrical_conductivity(DFO$Temperature, DFO$Salinity, DFO$Depth, DFO$Lat)

DFO$id = paste(DFO$cruise_id, DFO$stn_id, sep = "-")
DFO$Year = year(DFO$Date)
DFO$Sound_velocity = swSoundSpeed(salinity=DFO$Salinity, temperature = DFO$Temperature, pressure = DFO$Pressure)

DFO$Density= gsw_rho(DFO$Salinity, DFO$Temperature, DFO$Pressure)

DFO$Specific_conductance = gsw_C_from_SP(DFO$Salinity, DFO$Temperature, DFO$Pressure)*1000
  # Returns conductivity [mS/cm]


# Now need to add ground to the data
# Options are Scots Bay, German Bank or Other

# load two polygons
#Scots Bay

# Scots Bay CTD Box
df <- tribble(
  ~id, ~Box, ~Y, ~X,
  53, "Scots Bay", 44.7500, -65.61,
  54, "Scots Bay", 45.000, -64.67,
  55, "Scots Bay", 45.500, -64.67,
  56, "Scots Bay", 45.5, -65.61
)

sf_points <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)

# Ensure points are in the correct order
poly_coords <- sf_points %>%
  arrange(id) %>%              # or whatever ordering defines the boundary
  st_coordinates() %>%
  as.matrix()

# Close the polygon by repeating the first coordinate
poly_coords <- rbind(poly_coords, poly_coords[1, ])

# Build polygon
SB_poly <- st_polygon(list(poly_coords)) |> 
  st_sfc(crs = 4326) |> 
  st_sf(Box = unique(sf_points$Box), geometry = _)

#SB_poly


# German Bank CTD Box
df <- tribble(
  ~id, ~Box, ~Y, ~X,
  34, "German Bank", 43.7, -66.229,
  35 , "German Bank",43.56667, -66.229,
  36 , "German Bank",43.56667, -66.075,
  37, "German Bank", 43.233, -66.075,
  38, "German Bank", 43.233, -66.55,
  39, "German Bank", 43.7, -66.55,
  40, "German Bank", 43.7, -66.229   # closes the polygon
)

# Convert to polygon
poly_coords <- df %>%
  arrange(id) %>%
  select(X, Y) %>%
  as.matrix()

GermanBank_sf <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "German Bank", geometry = _)


#GermanBank_sf



### split up data into grounds. Function


label_scotsbay_germanbank <- function(df,
                                      lon = "lon", lat = "lat",
                                      ScotsBay, GermanBank,
                                      out_col = "region",
                                      crs_points = 4326,
                                      predicate = c("intersects", "within")) {
  predicate <- match.arg(predicate)
  
  # Points to sf
  pts <- st_as_sf(df, coords = c(lon, lat), crs = crs_points, remove = FALSE)
  
  # Ensure CRS matches
  if (st_crs(ScotsBay) != st_crs(pts)) ScotsBay <- st_transform(ScotsBay, st_crs(pts))
  if (st_crs(GermanBank) != st_crs(pts)) GermanBank <- st_transform(GermanBank, st_crs(pts))
  
  # Choose predicate
  pred_fun <- if (predicate == "within") st_within else st_intersects
  
  in_scots <- lengths(pred_fun(pts, ScotsBay)) > 0
  in_german <- lengths(pred_fun(pts, GermanBank)) > 0
  
  # Priority rule if overlap occurs: ScotsBay wins (you can swap if desired)
  df[[out_col]] <- ifelse(in_scots, "ScotsBay",
                          ifelse(in_german, "GermanBank", "Other"))
  df
}


DFO <- label_scotsbay_germanbank(
  DFO,
  lon = "Lon",
  lat = "Lat",
  ScotsBay = SB_poly,
  GermanBank = GermanBank_sf,
  out_col = "polygon"
)

DFO$ground = DFO$polygon 
DFO$polygon = NULL
DFO$ground = as.factor(DFO$ground)

levels(DFO$ground)[levels(DFO$ground) == "ScotsBay"] <- "Scots Bay"
levels(DFO$ground)[levels(DFO$ground) == "GermanBank"] <- "German Bank"

DFO$plankton_ID = NA
DFO$Survey = DFO$cruise_id
DFO$cruise_id = NULL


# Now some mods for CTD data
CTD$cruise_time = NA
CTD$datatype = "CD"

# Can add this in at some point
x =  aggregate(Depth ~ id, data = CTD, max, na.rm = TRUE)
colnames(x) = c("id", "maximum_depth")
CTD = merge(CTD, x, by = "id")

DFO$flag = NULL
CTD$stn_id = NA
CTD$time = NA

CTD$Source = "HSC"
DFO$Source = "DFO"

# finally, rbind


Ocean = rbind(CTD, DFO)

Oceans = rbind(Ocean, Ocean1)

dim(Oceans)
file="Oceans.RData"
save(Oceans, file="Oceans.RData", compress=T)

Ocean <- Ocean %>%
  mutate(JulianDay = lubridate::yday(as.Date(Date)))



