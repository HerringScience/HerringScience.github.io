

# Load Prince 5 Data from 
# https://www.frdr-dfdr.ca/repo/dataset/167e21b2-7eb9-466a-9b80-3980de093b15

# create the Prince5 modern dataset.

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/_Scripts")

library(ncdf4)
library(maps)
library(dplyr)
library(ggplot2)
library(oce)
library(sf)
library(rnaturalearth)

source("read_char_var.R")
source("pad_range.R")

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/.nc")

years <- 2017:2024
files <- paste0(years, ".nc")
out_dir <- "Casts"
dir.create(out_dir, showWarnings = FALSE)

# Store outputs
all_dates <- data.frame()
all_casts <- list()
counter <- 1

## Just data exploration - can skip:

                  #Look at what variables exist in the .nc files:
              
                          for (f in files) {
                            
                            cat("\n========================\n")
                            cat("FILE:", f, "\n")
                            cat("========================\n")
                            
                            nc <- nc_open(f)
                            
                            # List variable names
                            cat("\nVariables:\n")
                            print(names(nc$var))
                            
                            # List dimensions
                            cat("\nDimensions:\n")
                            print(names(nc$dim))
                            
                            nc_close(nc)
                          }
            
                                # density doesn't exist so you have to create it.
                                    
                                    # What ar the station names?
                                    
                                    nc <- nc_open("1978.nc")
                                    
                                    # See available variables
                                    names(nc$var)
                                    
                                    # Extract station IDs
                                    station_ids <- ncvar_get(nc, "station_ID")
                                    
                                    # Unique values
                                    unique(station_ids)

#Cont here:     


# Polygon coordinates (longitude, latitude)
# German/Seal
polygon_coords <- matrix(
  c(
    -66.229, 43.70000,
    -66.229, 43.56667,
    -66.075, 43.56667,
    -66.075, 43.23300,
    -66.550, 43.23300,
    -66.550, 43.70000,
    -66.229, 43.70000
  ),
  ncol = 2,
  byrow = TRUE
)

germanseal_poly <- st_polygon(list(polygon_coords)) |>
  st_sfc(crs = 4326)

# Scots Bay
SB_box <- matrix(
c(
  -65.61, 44.7500,
  -64.67, 45.000,
  -64.67,45.500,
  -65.61, 45.5,
  -65.61, 44.7500
),
ncol = 2,
byrow = TRUE
)

SB_poly<- st_polygon(list(SB_box)) |>
  st_sfc(crs = 4326)


# Combine into 1:
search_polys <- st_sf(
  area_poly = c("German Bank", "Scots Bay"),
  geometry = c(germanseal_poly, SB_poly)
)

############ LOOP:


#### LOOP - load year files in .nc folder                  

for (f in files) {
  
  cat("\n--- Processing:", f, "---\n")
  
  nc <- nc_open(f)
  
  # Load variables
  temp <- ncvar_get(nc, "temperature")
  sal  <- ncvar_get(nc, "salinity")
  lat  <- ncvar_get(nc, "latitude")
  lon  <- ncvar_get(nc, "longitude")
  # this is depth
  level <- nc$dim$level$vals
  station_id <- ncvar_get(nc, "station_ID")
  time_raw <- ncvar_get(nc, "time")
  
  # Convert time
  origin <- as.POSIXct("1900-01-01", tz = "UTC")
  time <- origin + time_raw
  
  # Extract month
  month <- as.numeric(format(time, "%m"))
  
  
  
  # --------------------------------------------------
  # Select Prince-5 casts AND casts inside your polygon
  # --------------------------------------------------
  
  # If station_ID comes in as a matrix from NetCDF, convert it to character
  if (is.matrix(station_id)) {
    station_id <- apply(station_id, 2, paste0, collapse = "")
  }
  
  station_id <- trimws(as.character(station_id))
  
  # Prince-5 matching; catches Prince-5, Prince 5, Prince5
  idx_prince5 <- grep("Prince[- ]?5", station_id, ignore.case = TRUE)
  
  # Convert all cast locations in this file to sf points
  cast_locations <- data.frame(
    cast = seq_along(lat),
    latitude = lat,
    longitude = lon
  )
  
  cast_locations_sf <- st_as_sf(
    cast_locations,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  # Find casts inside or touching your polygon
  # Find casts inside or touching either search polygon
  inside_polygon <- st_intersects(
    cast_locations_sf,
    search_polys,
    sparse = FALSE
  )
  
  # Logical vectors for each polygon
  inside_germanseal <- inside_polygon[, search_polys$area_poly == "German Bank"]
  inside_SB         <- inside_polygon[, search_polys$area_poly == "Scots Bay"]
  
  # Cast indices by polygon
  idx_germanseal <- cast_locations$cast[inside_germanseal]
  idx_SB         <- cast_locations$cast[inside_SB]
  
  # Any polygon
  idx_polygon <- unique(c(idx_germanseal, idx_SB))
  
  # Combine Prince-5 and polygon casts
  idx <- unique(c(idx_prince5, idx_polygon))
  
  # Apply June-November filter to both groups
  idx <- idx[month[idx] >= 6 & month[idx] <= 11]
  
  cat("Prince-5 casts:", length(idx_prince5), "\n")
  cat("German Bank polygon casts:", length(idx_germanseal), "\n")
  cat("Scots Bay polygon casts:", length(idx_SB), "\n")
  cat("Any polygon casts:", length(idx_polygon), "\n")
  cat("Combined casts after June-Nov filter:", length(idx), "\n")
  
  if (length(idx) == 0) {
    nc_close(nc)
    next
  }
  
  # Depth range for profiles
  depth_idx <- which(level >= 0 & level <= 200)
  
  # Map limits for this file using filtered Prince-5 casts
  map_xlim <- pad_range(lon[idx])
  map_ylim <- pad_range(lat[idx])
  
  for (i in idx) {
    
    # Keep depths where either temp or salinity has data
    valid_any <- !is.na(temp[depth_idx, i]) | !is.na(sal[depth_idx, i])
    
    if (!any(valid_any)) next
    
    depth <- level[depth_idx][valid_any]
    temperature <- temp[depth_idx, i][valid_any]
    salinity <- sal[depth_idx, i][valid_any]
    
    # Cast-level variables
    cast_date_char <- format(time[i], "%Y-%m-%d")
    cast_date <- as.Date(time[i])
    lat_i <- lat[i]
    lon_i <- lon[i]
    station_i <- station_id[i]
    
    area_i <- case_when(
      i %in% idx_prince5 & i %in% idx_germanseal & i %in% idx_SB ~ "Prince5_GermanSeal_ScotsBay",
      i %in% idx_prince5 & i %in% idx_germanseal ~ "Prince5_GermanSeal",
      i %in% idx_prince5 & i %in% idx_SB ~ "Prince5_ScotsBay",
      i %in% idx_germanseal & i %in% idx_SB ~ "GermanSeal_ScotsBay",
      i %in% idx_prince5 ~ "Prince5",
      i %in% idx_germanseal ~ "German Bank",
      i %in% idx_SB ~ "Scots Bay",
      TRUE ~ "Other"
    )
    
    # Store date table
    all_dates <- rbind(all_dates, data.frame(
      file = f,
      cast = i,
      date = cast_date,
      station_id = station_i,
      latitude = lat_i,
      longitude = lon_i,
      area = area_i,
      is_prince5 = i %in% idx_prince5,
      in_germanseal = i %in% idx_germanseal,
      in_scotsbay = i %in% idx_SB
    ))
    
    
    # Store full cast dataframe
    cast_df <- data.frame(
      file = f,
      cast = i,
      date = cast_date,
      station_id = station_i,
      latitude = lat_i,
      longitude = lon_i,
      area = area_i,
      is_prince5 = i %in% idx_prince5,
      in_germanseal = i %in% idx_germanseal,
      in_scotsbay = i %in% idx_SB,
      depth = depth,
      temperature = temperature,
      salinity = salinity
    )
    
    all_casts[[counter]] <- cast_df
    counter <- counter + 1
    
    # Save combined plot
    png(filename = file.path(
      out_dir,
      paste0(gsub(".nc", "", f),
             "_", cast_date_char,
             "_cast_", i,
             "_temp_sal_map.png")
    ),
    width = 1400,
    height = 600)
    
    layout(matrix(c(1, 2, 3), nrow = 1), widths = c(1, 1, 1.1))
    
    par(mar = c(5, 5, 4, 2))
    
    # -------------------------
    # Panel 1: Temperature
    # -------------------------
    valid_temp <- !is.na(temperature)
    
    if (any(valid_temp)) {
      plot(temperature[valid_temp], depth[valid_temp],
           type = "l",
           ylim = c(100, 0),
           xlab = "Temperature",
           ylab = "Depth (m)",
           main = "Temperature",
           col = "firebrick",
           lwd = 2)
    } else {
      plot(NA, NA,
           xlim = c(0, 1),
           ylim = c(100, 0),
           xlab = "Temperature",
           ylab = "Depth (m)",
           main = "Temperature")
      text(0.5, 50, "No temperature data")
    }
    
    # -------------------------
    # Panel 2: Salinity
    # -------------------------
    valid_sal <- !is.na(salinity)
    
    if (any(valid_sal)) {
      plot(salinity[valid_sal], depth[valid_sal],
           type = "l",
           ylim = c(100, 0),
           xlab = "Salinity",
           ylab = "Depth (m)",
           main = "Salinity",
           col = "dodgerblue4",
           lwd = 2)
    } else {
      plot(NA, NA,
           xlim = c(0, 1),
           ylim = c(100, 0),
           xlab = "Salinity",
           ylab = "Depth (m)",
           main = "Salinity")
      text(0.5, 50, "No salinity data")
    }
    
    # -------------------------
    # Panel 3: Location map-style plot
    # -------------------------
    # -------------------------
    # Panel 3: Map with land
    # -------------------------
    
    par(mar = c(5, 5, 4, 2))
    
    # ✅ Zoom OUT slightly so coastline is visible
    lon_range <- range(lon[idx], na.rm = TRUE)
    lat_range <- range(lat[idx], na.rm = TRUE)
    
    xlim <- c(lon_range[1] - 0.5, lon_range[2] + 0.5)
    ylim <- c(lat_range[1] - 0.5, lat_range[2] + 0.5)
    
    # ✅ Draw coastline (Nova Scotia region)
    map("world",
        xlim = xlim,
        ylim = ylim,
        col = "lightgrey",
        fill = TRUE,
        bg = "lightblue",
        mar = rep(0, 4))
    
    # ✅ Add all Prince-5 locations
    # Add polygon outline
    # Add polygon outlines
    plot(st_geometry(germanseal_poly), add = TRUE, border = "purple", lwd = 2)
    plot(st_geometry(SB_poly), add = TRUE, border = "darkgreen", lwd = 2)
    
    # Add all selected locations
    point_cols <- case_when(
      idx %in% idx_prince5 ~ "black",
      idx %in% idx_germanseal ~ "purple",
      idx %in% idx_SB ~ "darkgreen",
      TRUE ~ "grey40"
    )
    
    points(lon[idx], lat[idx],
           pch = 16,
           col = point_cols)
    
    # ✅ Highlight current cast
    points(lon_i, lat_i,
           pch = 19,
           col = "red",
           cex = 1.5)
    
    # legend
    legend("topright",
           legend = c("Prince-5", "German Bank polygon", "Scots Bay polygon", "Current cast"),
           col = c("black", "purple", "darkgreen", "red"),
           pch = c(16, NA, NA, 19),
           lty = c(NA, 1, 1, NA),
           lwd = c(NA, 2, 2, NA),
           bty = "n",
           cex = 0.8)
    
    # Label
    text(lon_i, lat_i,
         labels = paste("Cast", i),
         pos = 4,
         cex = 0.8)
    
    title("Cast Location")
    
    
    # Overall title
    mtext(paste(area_i, "|", cast_date_char, "| Cast", i, "|", f),
          outer = TRUE,
          line = -1.5,
          cex = 1.2,
          font = 2)
    
    dev.off()
  }
  
  nc_close(nc)
}




# Combine all casts into a df
if (length(all_casts) > 0) {
  final_casts <- do.call(rbind, all_casts)
} else {
  final_casts <- data.frame()
}


# Clean date table
if (nrow(all_dates) > 0) {
  all_dates <- unique(all_dates)
  all_dates <- all_dates[order(all_dates$date), ]
}


# Quick Health check of the dataset
# Summary
cat("\n--- SUMMARY ---\n")
cat("Rows in final_casts:", nrow(final_casts), "\n")
cat("Number of cast records:", nrow(all_dates), "\n")

if (nrow(all_dates) > 0) {
  cat("Date range:",
      as.character(min(all_dates$date)),
      "to",
      as.character(max(all_dates$date)),
      "\n")
}

# Useful checks
unique_dates <- sort(unique(all_dates$date))
print(unique_dates)

dim(final_casts)
unique(final_casts$area)

write.csv(final_casts, "final_casts.csv")

### Look at German Bank/Seal Island data:
polygon_casts <- final_casts %>%
  filter(area == "German Bank")

prince5_casts <- final_casts %>%
  filter(area == "Prince5")

scots_casts <- final_casts %>%
  filter(area == "Scots Bay")


### Add HSC data:
# Load HSC data
HSC = readRDS("Oceans.rds")
HSC=HSC[which(HSC$Source == "HSC"), ]


# Plot Coastline
coast <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)


# Create plotting dataframes

HSC_locations <- HSC %>%
  distinct(stn_id, Lat, Lon) %>%
  mutate(Source = "HSC")
          
        hsc_df <- HSC_locations %>%
            transmute(
              longitude = Lon,
              latitude = Lat, Source = Source
            )

        hsc_df <- hsc_df %>%
          filter(!is.na(longitude),
                 !is.na(latitude))
        
prince5_df <- prince5_casts %>%
  distinct(latitude, longitude) %>%
  mutate(Source = "prince5")

head(prince5_df)

lurcher_df <- polygon_casts %>%
distinct(longitude, latitude) %>%
mutate(Source = "German Bank")

scots_df <- scots_casts %>%
  distinct(longitude, latitude) %>%
  mutate(Source = "Scots Bay")

                            
plot_df <- bind_rows(lurcher_df, hsc_df, prince5_df, scots_df)
                
head(plot_df)                    
unique(plot_df$Source)                                    


ggplot() +
  geom_sf(
    data = coast,
    fill = "grey90",
    colour = "grey60",
    linewidth = 0.3
  ) +
  
  geom_point(
    data = hsc_df,
    aes(longitude, latitude,
        colour = Source,
        shape = Source),
    alpha = 0.6,
    size = 2
  ) +
  
  geom_point(
    data = prince5_df,
    aes(longitude, latitude,
        colour = Source,
        shape = Source),
    size = 3
  ) +
  
  geom_point(
    data = lurcher_df,
    aes(longitude, latitude,
        colour = Source,
        shape = Source),
    size = 3
  ) +
  
  geom_point(
    data = scots_df,
    aes(longitude, latitude,
        colour = Source,
        shape = Source),
    size = 3
  ) +
  
  scale_colour_manual(
    values = c(
      "German Bank" = "#D55E00",
      "HSC" = "#0072B2",
      "prince5" = "#009E73",
      "Scots Bay" = "#CC79A7"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "German Bank" = 16,
      "HSC" = 17,
      "prince5" = 15,
      "Scots Bay" = 18
    )
  ) +
  
  coord_sf(
    xlim = c(-67.5, -63),
    ylim = c(46, 42),
    expand = FALSE
  ) +
  
  labs(
    x = "Longitude (°W)",
    y = "Latitude (°N)",
    colour = NULL,
    shape = NULL
  ) +
  
  ggtitle("Cast Locations for Modern Comparison")+
  
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.2
    ),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      colour = "black",
      linewidth = 0.6
    ),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.position = c(0.82, 0.22),
    legend.background = element_rect(
      fill = "white",
      colour = "grey70"
    ),
    legend.text = element_text(size = 8),
    legend.title = element_blank()
  )


# Need to combine HSC and final_casts ###################################################

head(HSC)
HSC$area = HSC$ground
head(final_casts)

HSC2 <- HSC %>%
  transmute(
    date = Date,
    station_id = stn_id,
    latitude = Lat,
    longitude = Lon,
    area = area,
    depth = Depth,
    temperature = Temperature,
    salinity = Salinity,
    Year = Year,
    cast = id,
    Source = Source
  )

HSC2 <- HSC2 %>%
  mutate(
    file = NA_character_,
    is_prince5 = area == "Prince5",
    in_germanseal = area == "German Bank",
    in_scotsbay = area == "Scots Bay"
  )


head(HSC2)
write.csv(HSC2, "HSC.csv")

final_casts2 <- final_casts %>%
  mutate(
    Year = lubridate::year(date),
    Source = "DFO"
  )


common_cols <- c(
  "file",
  "cast",
  "date",
  "station_id",
  "latitude",
  "longitude",
  "area",
  "is_prince5",
  "in_germanseal",
  "in_scotsbay",
  "depth",
  "temperature",
  "salinity",
  "Year",
  "Source"
)

final_casts2 <- final_casts %>%
  mutate(
    cast = as.character(cast),
    Year = lubridate::year(date),
    Source = "DFO"
  )


all_casts <- bind_rows(
  HSC2[, common_cols],
  final_casts2[, common_cols]
)

dim(all_casts)

unique(all_casts$area)
unique(all_casts$Source)


saveRDS(all_casts, "all_casts.rds")

















#### Continue with .nc dataset

# Cut top 5m
all_casts_deep <- all_casts[all_casts$depth >= 5, ]
all_casts_deep <- all_casts_deep[all_casts_deep$depth <= 30, ]

min(all_casts_deep$depth)
max(all_casts_deep$depth)

# Make sure date is a Date
all_casts_deep$date <- as.Date(all_casts_deep$date)

final_casts_deep = all_casts_deep

# create density:

# add mean density

final_casts_deep <- final_casts_deep %>%
  mutate(
    density = swRho(
      salinity = salinity,
      temperature = temperature,
      pressure = depth
    )
  )

head(final_casts_deep)

# -----------------------------
# 2. Add year/month fields to cast-level averages
# -----------------------------

final_casts_deep <- final_casts_deep %>%
  mutate(
    month = as.numeric(format(date, "%m")),
    month_name = format(date, "%B")
  )

head(final_casts_deep)


## Some of the DFO Lurcher data doesn not have salinity so can only contribute to temperature analyses.

# -----------------------------
# 1. Average each individual cast across depth
# -----------------------------

cast_averages <- final_casts_deep %>%
  group_by(file, cast, date, area, latitude, longitude, Year, month, Source) %>%
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    mean_density = mean(density, na.rm = TRUE),
    
    # temp strat
    min_temperature = min(temperature, na.rm = TRUE),
    max_temperature = max(temperature, na.rm = TRUE),
    stratification = max_temperature - min_temperature,
    
    # density strat
    
    min_density = min(density, na.rm = TRUE),
    max_density = max(density, na.rm = TRUE),
    density_stratification = max_density - min_density,
    
    
    min_depth = min(depth, na.rm = TRUE),
    max_depth = max(depth, na.rm = TRUE),
    n_depths = n(),
    .groups = "drop"
  )


head(cast_averages)


# -----------------------------
# 3. Combine casts within same year/month
#    This gives ONE value per month per year
# -----------------------------

monthly_cast_averages <- cast_averages %>%
  group_by(Year, month, area) %>%
  summarise(
    
    # temperature mean over the entire WC
    monthly_mean_temperature = mean(mean_temperature, na.rm = TRUE),
    monthly_sd_temperature = sd(mean_temperature, na.rm = TRUE),
    monthly_se_temperature = monthly_sd_temperature / sqrt(sum(!is.na(mean_temperature))),
    
    # salinity mean over the entire WC
    monthly_mean_salinity = mean(mean_salinity, na.rm = TRUE),
    monthly_sd_salinity = sd(mean_salinity, na.rm = TRUE),
    monthly_se_salinity = monthly_sd_salinity / sqrt(sum(!is.na(mean_salinity))),
    
    # density over the entire WC
    monthly_mean_density = mean(mean_density, na.rm = TRUE),
    monthly_sd_density = sd(mean_density, na.rm = TRUE),
    monthly_se_density = monthly_sd_density / sqrt(sum(!is.na(mean_density))),
    
    # temperature stratification 
    monthly_mean_stratification = mean(stratification, na.rm = TRUE),
    monthly_sd_stratification = sd(stratification, na.rm = TRUE),
    monthly_se_stratification = monthly_sd_stratification / sqrt(sum(!is.na(stratification))),
    
    # density stratification 
    monthly_mean_density_stratification = mean(density_stratification, na.rm = TRUE),
    monthly_sd_density_stratification = sd(density_stratification, na.rm = TRUE),
    monthly_se_density_stratification = monthly_sd_density_stratification / sqrt(sum(!is.na(density_stratification))),
    
    
    # Meta Data
    n_casts = n(),
    mean_latitude = mean(latitude, na.rm = TRUE),
    mean_longitude = mean(longitude, na.rm = TRUE),
    min_depth = min(min_depth, na.rm = TRUE),
    max_depth = max(max_depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, month)


colnames(monthly_cast_averages)
unique(monthly_cast_averages$area)
unique(monthly_cast_averages$Year)



# Optional: save the monthly averaged dataframe
write.csv(
  monthly_cast_averages,
  "Modern_monthly_cast_averages_between5-30.csv",
  row.names = FALSE
)




# -----------------------------
# 4. Create Plots: Create output folder inside Casts
# currently doesn't include density or densirty stratification but could be added.
# -----------------------------

names(monthly_cast_averages)

year_dir <- file.path("Casts", "Year")
dir.create(year_dir, showWarnings = FALSE, recursive = TRUE)

# Months you are working with: June to November
months_to_plot <- 6:11

# -----------------------------
# 5. Make one temperature and one salinity plot per month
# -----------------------------

for (m in months_to_plot) {
  
  month_data <- monthly_cast_averages %>%
    filter(month == m)
  
  # Skip month if no data
  if (nrow(month_data) == 0) next
  
  this_month <- unique(month_data$month_name)[1]
  
  # -----------------------------
  # Temperature plot
  # -----------------------------
  
  p_temp <- ggplot(month_data, aes(x = year, y = monthly_mean_temperature)) +
    geom_point(size = 3, colour = "firebrick") +
    geom_line(aes(group = 1), colour = "firebrick", linewidth = 0.8) +
    geom_text(aes(label = n_casts), vjust = -1, size = 3) +
    labs(
      title = paste("Prince-5 Monthly Mean Temperature by Year -", this_month),
      subtitle = "Numbers above points show number of casts combined",
      x = "Year",
      y = "Monthly mean temperature below 5 m"
    ) +
    scale_x_continuous(breaks = sort(unique(month_data$year))) +
    theme_bw()
  
  ggsave(
    filename = file.path(
      year_dir,
      paste0("monthly_temperature_year_effect_", this_month, ".png")
    ),
    plot = p_temp,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  # -----------------------------
  # Salinity plot
  # -----------------------------
  
  p_sal <- ggplot(month_data, aes(x = year, y = monthly_mean_salinity)) +
    geom_point(size = 3, colour = "dodgerblue4") +
    geom_line(aes(group = 1), colour = "dodgerblue4", linewidth = 0.8) +
    geom_text(aes(label = n_casts), vjust = -1, size = 3) +
    labs(
      title = paste("Prince-5 Monthly Mean Salinity by Year -", this_month),
      subtitle = "Numbers above points show number of casts combined",
      x = "Year",
      y = "Monthly mean salinity below 5 m"
    ) +
    scale_x_continuous(breaks = sort(unique(month_data$year))) +
    theme_bw()
  
  ggsave(
    filename = file.path(
      year_dir,
      paste0("monthly_salinity_year_effect_", this_month, ".png")
    ),
    plot = p_sal,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  # -----------------------------
  # Stratification plot
  # -----------------------------
  
  p_strat <- ggplot(month_data, aes(x = year, y = monthly_mean_stratification)) +
    geom_point(size = 3, colour = "darkorchid4") +
    geom_line(aes(group = 1), colour = "darkorchid4", linewidth = 0.8) +
    geom_text(aes(label = n_casts), vjust = -1, size = 3) +
    labs(
      title = paste("Prince-5 Monthly Mean Temperature Stratification by Year -", this_month),
      subtitle = "Stratification = max temperature - min temperature; numbers show number of casts combined",
      x = "Year",
      y = "Monthly mean temperature stratification below 5 m"
    ) +
    scale_x_continuous(breaks = sort(unique(month_data$year))) +
    theme_bw()
  
  ggsave(
    filename = file.path(
      year_dir,
      paste0("monthly_stratification_year_effect_", this_month, ".png")
    ),
    plot = p_strat,
    width = 8,
    height = 5,
    dpi = 300
  )
}



# -----------------------------
# 6. Quick checks
# -----------------------------

print(monthly_cast_averages)

saveRDS(monthly_cast_averages, "monthly_cast_averages.rds")

combined_monthly=monthly_cast_averages

head(combined_monthly)


unique(combined_monthly$Year)



##########################Continue analysis

# Get coastline / land polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert your dataframe to spatial points
combined_monthly_sf <- combined_monthly %>%
  filter(!is.na(mean_latitude), !is.na(mean_longitude)) %>%
  st_as_sf(
    coords = c("mean_longitude", "mean_latitude"),
    crs = 4326
  )

# Plot
ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey60") +
  geom_sf(
    data = combined_monthly_sf,
    aes(color = area, size = n_casts),
    alpha = 0.8
  ) +
  coord_sf(
    xlim = range(combined_monthly$mean_longitude, na.rm = TRUE) + c(-0.2, 0.2),
    ylim = range(combined_monthly$mean_latitude, na.rm = TRUE) + c(-0.2, 0.2),
    expand = FALSE
  ) +
  scale_size_continuous(name = "Number of casts") +
  labs(
    title = "Spatial distribution of casts",
    subtitle = "Points show monthly mean cast locations",
    x = "Longitude",
    y = "Latitude",
    color = "Area"
  ) +
  theme_minimal()




############################# Individual factor plots:

head(combined_monthly)


## Temperature:

ggplot(
  combined_monthly,
  aes(
    x = month,
    y = monthly_mean_temperature,
    color = area,
    group = area
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~Year) +
  labs(
    title = "Monthly Mean Temperature: German Bank, Scots Bay and Prince-5",
    x = "Month",
    y = "Monthly Mean Temperature",
    color = "Dataset / Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Salinity:

ggplot(combined_monthly,
       aes(x = month,
           y = monthly_mean_salinity,
           color = area,
           group = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year) +
  labs(
    title = "Monthly Mean Salinity: HSC Grounds vs Prince-5",
    x = "Month",
    y = "Monthly Mean Salinity",
    color = "Dataset / Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

### Density
ggplot(combined_monthly,
       aes(x = month,
           y = monthly_mean_density,
           color = area,
           group = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year) +
  labs(
    title = "Monthly Mean Density: HSC Grounds vs Prince-5",
    x = "Month",
    y = "Monthly Mean Density",
    color = "Dataset / Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Temperature Stratification:

ggplot(combined_monthly,
       aes(x = month,
           y = monthly_mean_stratification,
           color = area,
           group = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year) +
  labs(
    title = "Monthly Mean Stratification: HSC Grounds vs Prince-5",
    x = "Month",
    y = "Monthly Mean Stratification",
    color = "Dataset / Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


### Density strat:
ggplot(combined_monthly,
       aes(x = month,
           y = monthly_mean_density_stratification,
           color = area,
           group = area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year) +
  labs(
    title = "Monthly Mean Density Stratification: HSC Grounds vs Prince-5",
    x = "Month",
    y = "Monthly Mean Density Stratification",
    color = "Dataset / Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



###################### Analysis::::: ----------------------------------->
### COntinue here tomorrow:


###### Compare average monthly between grounds








# --------------------------------------------------
# 3. Join HSC grounds to Prince-5 by Year + month
# --------------------------------------------------
# This keeps only months where that HSC ground AND Prince-5 both exist.

comparison <- hsc_vars %>%
  inner_join(prince5_vars, by = c("Year", "month")) %>%
  mutate(
    # Temperature difference
    temp_diff = HSC_temp - Prince5_temp,
    temp_abs_diff = abs(temp_diff),
    temp_diff_se = sqrt(HSC_temp_se^2 + Prince5_temp_se^2),
    
    # Salinity difference
    sal_diff = HSC_sal - Prince5_sal,
    sal_abs_diff = abs(sal_diff),
    sal_diff_se = sqrt(HSC_sal_se^2 + Prince5_sal_se^2),
    
    # Density difference
    den_diff = HSC_den - Prince5_den,
    den_abs_diff = abs(den_diff),
    den_diff_se = sqrt(HSC_den_se^2 + Prince5_den_se^2),
    
    # Stratification difference
    strat_diff = HSC_strat - Prince5_strat,
    strat_abs_diff = abs(strat_diff),
    strat_diff_se = sqrt(HSC_strat_se^2 + Prince5_strat_se^2),
    
    # Density Stratification difference
    Dstrat_diff = HSC_Dstrat - Prince5_Dstrat,
    Dstrat_abs_diff = abs(Dstrat_diff),
    Dstrat_diff_se = sqrt(HSC_Dstrat_se^2 + Prince5_Dstrat_se^2),
    
    month_name = month.name[month]
  ) %>%
  arrange(area, Year, month)

# View matched monthly comparisons
print(comparison)

stats_summary <- comparison %>%
  group_by(area) %>%
  summarise(
    n_matched_months = n(),
    
    # -----------------------------
    # Temperature stats
    # -----------------------------
    mean_HSC_temp = mean(HSC_temp, na.rm = TRUE),
    mean_Prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    
    mean_temp_difference_HSC_minus_Prince5 = mean(temp_diff, na.rm = TRUE),
    median_temp_difference = median(temp_diff, na.rm = TRUE),
    sd_temp_difference = sd(temp_diff, na.rm = TRUE),
    mean_absolute_temp_difference = mean(temp_abs_diff, na.rm = TRUE),
    temp_RMSE = sqrt(mean(temp_diff^2, na.rm = TRUE)),
    min_temp_difference = min(temp_diff, na.rm = TRUE),
    max_temp_difference = max(temp_diff, na.rm = TRUE),
    temp_correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs"),
    
    # -----------------------------
    # Salinity stats
    # -----------------------------
    mean_HSC_sal = mean(HSC_sal, na.rm = TRUE),
    mean_Prince5_sal = mean(Prince5_sal, na.rm = TRUE),
    
    mean_sal_difference_HSC_minus_Prince5 = mean(sal_diff, na.rm = TRUE),
    median_sal_difference = median(sal_diff, na.rm = TRUE),
    sd_sal_difference = sd(sal_diff, na.rm = TRUE),
    mean_absolute_sal_difference = mean(sal_abs_diff, na.rm = TRUE),
    sal_RMSE = sqrt(mean(sal_diff^2, na.rm = TRUE)),
    min_sal_difference = min(sal_diff, na.rm = TRUE),
    max_sal_difference = max(sal_diff, na.rm = TRUE),
    sal_correlation = cor(HSC_sal, Prince5_sal, use = "complete.obs"),
    
    # -----------------------------
    # Stratification stats
    # -----------------------------
    mean_HSC_strat = mean(HSC_strat, na.rm = TRUE),
    mean_Prince5_strat = mean(Prince5_strat, na.rm = TRUE),
    
    mean_strat_difference_HSC_minus_Prince5 = mean(strat_diff, na.rm = TRUE),
    median_strat_difference = median(strat_diff, na.rm = TRUE),
    sd_strat_difference = sd(strat_diff, na.rm = TRUE),
    mean_absolute_strat_difference = mean(strat_abs_diff, na.rm = TRUE),
    strat_RMSE = sqrt(mean(strat_diff^2, na.rm = TRUE)),
    min_strat_difference = min(strat_diff, na.rm = TRUE),
    max_strat_difference = max(strat_diff, na.rm = TRUE),
    strat_correlation = cor(HSC_strat, Prince5_strat, use = "complete.obs"),
    
    .groups = "drop"
  )

print(stats_summary)
View(stats_summary)


### STATS

## Q: are the monthly values at this ground statistically different from Prince 5 when matched by year and month? (t-test and wilcoxon)


paired_tests <- comparison %>%
  group_by(area) %>%
  summarise(
    n_matched_months = n(),
    
    # -----------------------------
    # Temperature tests
    # -----------------------------
    temp_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_temp, Prince5_temp, paired = TRUE)$p.value,
      NA
    ),
    
    temp_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_temp, Prince5_temp, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    ### Neither ground is statistically significant different from Prince 5 in terms of temperature. Temperature is well represented by Prince 5.
    
    
    # -----------------------------
    # Salinity tests
    # -----------------------------
    sal_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_sal, Prince5_sal, paired = TRUE)$p.value,
      NA
    ),
    
    sal_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_sal, Prince5_sal, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    ##### Salinity is also well represented by Prince 5 (not statistically significant)
    
    ## density TESTS
    
    den_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_den, Prince5_den, paired = TRUE)$p.value,
      NA
    ),
    
    den_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_den, Prince5_den, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    
    
    # -----------------------------
    # Stratification tests
    # -----------------------------
    strat_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_strat, Prince5_strat, paired = TRUE)$p.value,
      NA
    ),
    
    strat_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_strat, Prince5_strat, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    
    ### Density stratification:
    Dstrat_paired_t_test_p_value = {
      valid <- !is.na(HSC_Dstrat) & !is.na(Prince5_Dstrat)
      diffs <- HSC_Dstrat[valid] - Prince5_Dstrat[valid]
      
      if (sum(valid) >= 2 && sd(diffs, na.rm = TRUE) > 0) {
        t.test(
          HSC_Dstrat[valid],
          Prince5_Dstrat[valid],
          paired = TRUE
        )$p.value
      } else {
        NA_real_
      }
    },
    
    Dstrat_wilcoxon_p_value = {
      valid <- !is.na(HSC_Dstrat) & !is.na(Prince5_Dstrat)
      
      if (sum(valid) >= 2) {
        wilcox.test(
          HSC_Dstrat[valid],
          Prince5_Dstrat[valid],
          paired = TRUE,
          exact = FALSE
        )$p.value
      } else {
        NA_real_
      }
    },
    
    .groups = "drop"
  )

#### German Bank IS statistically different than Prince 5 for stratification
#### Scots Bay is not different

print(paired_tests)
View(paired_tests)

## Key Result

#The sources/datasets seem broadly comparable for average temperature, salinity, and density, but they may differ in how well they capture vertical structure/stratification, especially on German Bank.

#In Scots Bay, mean environmental conditions appear comparable between paired observations. Evidence for differences in stratification is weaker than on German Bank, although the Wilcoxon test suggests that density stratification may differ between sources.

###Paired comparisons were conducted using matched year-month observations for German Bank and Scots Bay. Across both grounds, monthly mean temperature, salinity, and density did not differ significantly between paired observations, with p-values consistently above 0.05 for both paired t-tests and Wilcoxon signed-rank tests. This suggests that average hydrographic conditions were broadly comparable between sources for matched months.
#In contrast, stratification metrics showed stronger evidence of source-related differences, particularly on German Bank. At German Bank, both temperature stratification and density stratification differed significantly between paired observations, with highly significant results from both paired t-tests and Wilcoxon tests. This indicates that although average conditions were similar, the vertical structure of the water column differed between sources.
#For Scots Bay, there was no significant difference in temperature stratification, and evidence for density stratification differences was weaker. The paired t-test for density stratification was not significant, but the Wilcoxon test was significant, suggesting a possible non-normal or skewed pattern in paired differences. Overall, Scots Bay showed less evidence of source-related differences than German Bank.
#These results suggest that source comparisons are more robust for monthly mean temperature, salinity, and density than for stratification metrics. Stratification should be interpreted cautiously, particularly for German Bank, because it is likely more sensitive to differences in cast depth, vertical resolution, and sampling coverage.































scots_vs_prince5 <- comparison %>%
  filter(ground == "Scots Bay")

german_vs_prince5 <- comparison %>%
  filter(ground == "German Bank")

print(scots_vs_prince5)
print(german_vs_prince5)





#### test seasonality versus ground effect:
## is there a true ground effect or season effect?

comparison_summer <- comparison %>%
  filter(month %in% c(7, 8, 9))  # July–September

# same as above, just using overlapping months
paired_tests <- comparison_summer %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    
    # -----------------------------
    # Temperature tests
    # -----------------------------
    temp_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_temp, Prince5_temp, paired = TRUE)$p.value,
      NA
    ),
    
    temp_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_temp, Prince5_temp, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    
    # -----------------------------
    # Salinity tests
    # -----------------------------
    sal_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_sal, Prince5_sal, paired = TRUE)$p.value,
      NA
    ),
    
    sal_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_sal, Prince5_sal, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    
    # -----------------------------
    # Stratification tests
    # -----------------------------
    strat_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_strat, Prince5_strat, paired = TRUE)$p.value,
      NA
    ),
    
    strat_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_strat, Prince5_strat, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    .groups = "drop"
  )


view(paired_tests)



#### There is a real ground effect in stratification at German Bank relative to Prince‑5


##Differences in stratification between Prince‑5 and German Bank persist even when controlling for seasonal effects, indicating that these differences reflect genuine spatial variability rather than sampling timing alone.










#### Descriptive performance metrics - not hypotheses tests
### They answer - How close is Prince 5 to this ground on average? Or how big is the difference?
# mean difference - average bias, directional difference
# median difference - less biased version of above, average bias (less influence of outliers)
# Error Size: 1. mean_absolute_difference and 2. RMSE
# 1. average magnitude of difference (ignores sign)
# 2. like mean absolute error but penalizes large error more
# RMSE is the best single 'how different are they metric'
# sd_difference - how consistent the differences are
# Relationship: correlation, do the time series move together? Do they track each other?
# high correlation and low RMSE = strong representativeness

stats_summary <- comparison %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    
    # -----------------------------
    # Temperature
    # -----------------------------
    mean_HSC_temp = mean(HSC_temp, na.rm = TRUE),
    mean_Prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    mean_temp_difference = mean(temp_diff, na.rm = TRUE),
    median_temp_difference = median(temp_diff, na.rm = TRUE),
    sd_temp_difference = sd(temp_diff, na.rm = TRUE),
    mean_abs_temp_difference = mean(temp_abs_diff, na.rm = TRUE),
    temp_RMSE = sqrt(mean(temp_diff^2, na.rm = TRUE)),
    temp_correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs"),
    
    # -----------------------------
    # Salinity
    # -----------------------------
    mean_HSC_sal = mean(HSC_sal, na.rm = TRUE),
    mean_Prince5_sal = mean(Prince5_sal, na.rm = TRUE),
    mean_sal_difference = mean(sal_diff, na.rm = TRUE),
    median_sal_difference = median(sal_diff, na.rm = TRUE),
    sd_sal_difference = sd(sal_diff, na.rm = TRUE),
    mean_abs_sal_difference = mean(sal_abs_diff, na.rm = TRUE),
    sal_RMSE = sqrt(mean(sal_diff^2, na.rm = TRUE)),
    sal_correlation = cor(HSC_sal, Prince5_sal, use = "complete.obs"),
    
    # -----------------------------
    # Stratification
    # -----------------------------
    mean_HSC_strat = mean(HSC_strat, na.rm = TRUE),
    mean_Prince5_strat = mean(Prince5_strat, na.rm = TRUE),
    mean_strat_difference = mean(strat_diff, na.rm = TRUE),
    median_strat_difference = median(strat_diff, na.rm = TRUE),
    sd_strat_difference = sd(strat_diff, na.rm = TRUE),
    mean_abs_strat_difference = mean(strat_abs_diff, na.rm = TRUE),
    strat_RMSE = sqrt(mean(strat_diff^2, na.rm = TRUE)),
    strat_correlation = cor(HSC_strat, Prince5_strat, use = "complete.obs"),
    
    .groups = "drop"
  )

view(stats_summary)

#Across both German Bank and Scots Bay, Prince‑5 reproduces temperature and salinity patterns well, but shows weaker agreement for stratification at German Bank, indicating that vertical structure at that site differs from Prince‑5 despite similar broader hydrographic conditions.


getwd()
### PLOTS monthly differences

library(grid)

comparison_long <- comparison %>%
  select(
    Year,
    month,
    ground,
    temp_diff,
    sal_diff,
    strat_diff
  ) %>%
  pivot_longer(
    cols = c(temp_diff, sal_diff, strat_diff),
    names_to = "variable",
    values_to = "difference"
  ) %>%
  mutate(
    variable = recode(
      variable,
      temp_diff = "Temperature",
      sal_diff = "Salinity",
      strat_diff = "Stratification"
    )
  )

ggplot(comparison_long,
       aes(x = month,
           y = difference,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.65) +
  facet_grid(variable ~ Year, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Differences: HSC Grounds minus Prince-5",
    subtitle = "Positive values mean HSC ground values are greater than Prince-5",
    x = "Month",
    y = "Difference",
    color = "Ground"
  ) + theme_bw() +
  theme(
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10),
    panel.spacing = unit(1, "lines")
  )

### temp plots with error bars:
ggplot(comparison,
       aes(x = month,
           y = temp_diff,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(
    aes(
      ymin = temp_diff - temp_diff_se,
      ymax = temp_diff + temp_diff_se
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 4) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Temperature Difference: HSC Grounds minus Prince-5",
    subtitle = "Error bars show ±1 SE of the difference; positive values mean HSC ground is warmer than Prince-5",
    x = "Month",
    y = "Temperature Difference (°C)",
    color = "Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10)
  )

#### error bars for stratification:
ggplot(comparison,
       aes(x = month,
           y = strat_diff,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(
    aes(
      ymin = strat_diff - strat_diff_se,
      ymax = strat_diff + strat_diff_se
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 4) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Stratification Difference: HSC Grounds minus Prince-5",
    subtitle = "Error bars show ±1 SE of the difference; positive values mean HSC ground is more stratified than Prince-5",
    x = "Month",
    y = "Stratification Difference",
    color = "Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10)
  )

















# save the stats:
dir.create("Casts/HSC_Prince5_Comparison", showWarnings = FALSE, recursive = TRUE)

write.csv(
  temp_compare,
  "Casts/HSC_Prince5_Comparison/monthly_temperature_matched_comparisons.csv",
  row.names = FALSE
)

write.csv(
  temp_stats,
  "Casts/HSC_Prince5_Comparison/monthly_temperature_summary_stats.csv",
  row.names = FALSE
)

write.csv(
  paired_tests,
  "Casts/HSC_Prince5_Comparison/monthly_temperature_paired_tests.csv",
  row.names = FALSE
)




############# text for document:
# Scots Bay monthly temperatures were compared with Prince-5 using matched year-month observations available for Scots Bay, primarily June–September. German Bank monthly temperatures were compared with Prince-5 using matched year-month observations available for German Bank, primarily August–November.


# Ground specific comparison to Prince 5 using all available matched months
temp_stats_all_available <- temp_compare %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    months_sampled = paste(sort(unique(month_name)), collapse = ", "),
    
    mean_HSC_temp = mean(HSC_temp, na.rm = TRUE),
    mean_Prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    
    mean_difference_HSC_minus_Prince5 = mean(temp_difference, na.rm = TRUE),
    median_difference = median(temp_difference, na.rm = TRUE),
    sd_difference = sd(temp_difference, na.rm = TRUE),
    mean_absolute_difference = mean(abs_difference, na.rm = TRUE),
    RMSE = sqrt(mean(temp_difference^2, na.rm = TRUE)),
    
    .groups = "drop"
  )

print(temp_stats_all_available)




############### Clean minimla summary code STATS:
representativeness_stats <- temp_compare %>%
  group_by(ground) %>%
  summarise(
    n_months = n(),
    
    mean_bias = mean(temp_difference, na.rm = TRUE),
    mean_absolute_error = mean(abs_difference, na.rm = TRUE),
    RMSE = sqrt(mean(temp_difference^2, na.rm = TRUE)),
    
    correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs"),
    
    min_diff = min(temp_difference, na.rm = TRUE),
    max_diff = max(temp_difference, na.rm = TRUE),
    
    .groups = "drop"
  )

print(representativeness_stats)

### Conclusions:

## Scots Bay, 27 month matches, Scots Bay is on average only 0.2 degrees warmer than Prince 5. Prince 5 is typically off by 0.6 degrees. Overall error is relatively low (0.744). Very strong seasonal/monthly tracking (0.935). Sometimes Scots Bay is cooler, sometimes it is warmer.
# Prince 5 is a strong temperature proxy for Scots Bay. They are not systematically different, they move very closely over time and typical monthly differnce is less than a degree.
#Prince‑5 appears to represent average monthly temperature conditions in Scots Bay reasonably well, with very small average bias and strong temporal agreement.
# Is Prince 5 representative? Yes, fairly strong.




## German Bank, 19 month matches. German Bank is 0.35 degrees cooler than Prince 5 on average. Prince 5 is typically off by 0.9 degrees. Overall error is higher (1.14). Weak tracking (0.102). German Bank can be about 2 degrees warmer or cooler. Prince‑5 does not appear to represent German Bank as reliably but the mean bias is not huge, -0.353. So on average, Prince‑5 is only about 0.35°C warmer than German Bank. The bigger issue is the correlation (0.102). That means Prince‑5 and German Bank are not tracking each other well month-to-month/year-to-year, and the error is larger (0.929, 1.14). 
#Prince‑5 shows only weak representativeness for German Bank monthly average temperatures. Although the average bias is modest, the low correlation and higher error indicate that Prince‑5 does not reliably track German Bank temperature variability.
# Is Prince 5 representative? Only weakly/cautiously. It may approximate the average temperature sometimes but it does not track German Bank well. Mean bias is ok - but it can hide the wide flucatations, 1.74 to -2.22



#Overall:
##Prince‑5 was more representative of Scots Bay than German Bank for monthly average temperature. Scots Bay showed a small average temperature difference from Prince‑5 (+0.21°C), low mean absolute error (0.58°C), and strong correlation (r = 0.94), indicating that Prince‑5 closely tracked Scots Bay monthly temperatures. German Bank had a modest average bias relative to Prince‑5 (-0.35°C), but higher error (MAE = 0.93°C; RMSE = 1.14°C) and very weak correlation (r = 0.10), suggesting Prince‑5 does not reliably capture German Bank monthly temperature variability.


### take away:My recommendation
#For your conclusion, I would separate average bias from representativeness:

#Scots Bay: low bias and strong representativeness
#German Bank: modest average bias but poor representativeness/tracking

#That distinction is really important.


ggplot(temp_compare,
       aes(x = Prince5_temp,
           y = HSC_temp,
           color = ground)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "grey40") +
  facet_wrap(~ ground) +
  labs(
    title = "Monthly Average Temperature: HSC Grounds vs Prince-5",
    subtitle = "Dashed line shows perfect 1:1 agreement",
    x = "Prince-5 monthly mean temperature (°C)",
    y = "HSC ground monthly mean temperature (°C)",
    color = "Ground"
  ) +
  theme_bw()



#### Look monthly

ggplot(temp_compare,
       aes(x = month,
           y = temp_difference,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "grey40") +
  geom_point(size = 3, alpha = 0.8)  +
  facet_wrap(~ ground) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Temperature Bias: HSC Grounds minus Prince-5",
    subtitle = "Positive values mean the HSC ground was warmer than Prince-5",
    x = "Month",
    y = "Temperature difference (°C)",
    color = "Ground"
  ) +
  theme_bw()


# by year and month:

ggplot(temp_compare,
       aes(x = month,
           y = temp_difference,
           color = ground,
           group = interaction(ground, Year))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "grey40") +
  geom_line(alpha = 0.6) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Temperature Difference by Year",
    subtitle = "HSC ground monthly average minus Prince-5 monthly average",
    x = "Month",
    y = "Temperature difference (°C)",
    color = "Ground"
  ) +
  theme_bw()


### Look at error:

library(tidyr)

rep_stats_long <- representativeness_stats %>%
  select(ground, mean_absolute_error, RMSE) %>%
  pivot_longer(
    cols = c(mean_absolute_error, RMSE),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      mean_absolute_error = "Mean absolute error",
      RMSE = "RMSE"
    )
  )

ggplot(rep_stats_long,
       aes(x = ground,
           y = value,
           fill = metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "Prince-5 Representativeness Error by Ground",
    subtitle = "Lower values indicate better agreement with HSC ground temperatures",
    x = "Ground",
    y = "Temperature error (°C)",
    fill = "Metric"
  ) +
  theme_bw()

## correlation bar plot:

ggplot(representativeness_stats,
       aes(x = ground,
           y = correlation,
           fill = ground)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0,
             color = "grey40") +
  ylim(-1, 1) +
  labs(
    title = "Correlation Between Prince-5 and HSC Ground Temperatures",
    subtitle = "Higher values mean Prince-5 tracks monthly temperature patterns better",
    x = "Ground",
    y = "Correlation"
  ) +
  theme_bw() +
  theme(legend.position = "none")


### last one:
library(patchwork)
library(patchwork)

p1 <- ggplot(temp_compare,
             aes(x = Prince5_temp,
                 y = HSC_temp,
                 color = ground)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "grey40") +
  facet_wrap(~ ground) +
  labs(
    title = "A. Matched monthly temperatures",
    x = "Prince-5 temperature (°C)",
    y = "HSC ground temperature (°C)",
    color = "Ground"
  ) +
  theme_bw()

p2 <- ggplot(temp_compare,
             aes(x = month,
                 y = temp_difference,
                 color = ground,
                 group = ground)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "grey40") +
  geom_point(size = 3, alpha = 0.8)  +
  facet_wrap(~ ground) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "B. Monthly bias",
    x = "Month",
    y = "HSC minus Prince-5 (°C)",
    color = "Ground"
  ) +
  theme_bw()

p3 <- ggplot(rep_stats_long,
             aes(x = ground,
                 y = value,
                 fill = metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "C. Error metrics",
    x = "Ground",
    y = "Temperature error (°C)",
    fill = "Metric"
  ) +
  theme_bw()

combined_plot <- (p1 / p2 / p3)

combined_plot







## Which months matter most?

largest_temp_differences <- temp_compare %>%
  arrange(desc(abs_difference)) %>%
  select(
    ground,
    Year,
    month,
    month_name,
    HSC_temp,
    Prince5_temp,
    temp_difference,
    abs_difference
  )

print(largest_temp_differences)

