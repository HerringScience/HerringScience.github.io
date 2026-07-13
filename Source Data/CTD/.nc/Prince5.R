

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

source("read_char_var.R")
source("pad_range.R")


### June 29, 2026 Update:

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/.nc")

years <- 2017:2024
files <- paste0(years, ".nc")
out_dir <- "Casts"
dir.create(out_dir, showWarnings = FALSE)

# Store outputs
all_dates <- data.frame()
all_casts <- list()
counter <- 1

# Polygon coordinates (longitude, latitude) for extracting later

germanseal_coords <- matrix(
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

GS_poly <- st_polygon(list(germanseal_coords)) |>
  st_sfc(crs = 4326)

                                




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
  
  # Extract only Prince-5 - we also want Lurcher!   
  # Prince-5 matching; catches Prince-5, Prince 5, Prince5
  #idx <- grep("Prince[- ]?5", station_id, ignore.case = TRUE)
  
  # Filter May to November (this is what overlaps with the HSC data collection)
  #idx <- idx[month[idx] >= 6 & month[idx] <= 11]
  
  idx <- which(month >= 6 & month <= 11)
  
  cat("Prince-5 casts after June-Nov filter:", length(idx), "\n")
  
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
    
    # Store date table
    all_dates <- rbind(all_dates, data.frame(
      file = f,
      cast = i,
      date = cast_date,
      station_id = station_i,
      latitude = lat_i,
      longitude = lon_i
    ))
    
    # Store full cast dataframe
    cast_df <- data.frame(
      file = f,
      cast = i,
      date = cast_date,
      station_id = station_i,
      latitude = lat_i,
      longitude = lon_i,
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
    points(lon[idx], lat[idx],
           pch = 16,
           col = "black")
    
    # ✅ Highlight current cast
    points(lon_i, lat_i,
           pch = 19,
           col = "red",
           cex = 1.5)
    
    # Label
    text(lon_i, lat_i,
         labels = paste("Cast", i),
         pos = 4,
         cex = 0.8)
    
    title("Cast Location")
    
    
    # Overall title
    mtext(paste("Prince-5 |", cast_date_char, "| Cast", i, "|", f),
          outer = TRUE,
          line = -1.5,
          cex = 1.2,
          font = 2)
    
    dev.off()
  }
  
  nc_close(nc)
}

                                
                                
############################## Combine all casts into a df
if (length(all_casts) > 0) {
 
final_casts <- do.call(rbind, all_casts)

# Create unique cast locations
cast_locations <- final_casts %>%
  distinct(cast, latitude, longitude)


# Convert cast locations to sf
casts_sf <- st_as_sf(
  cast_locations,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Find casts inside polygon
inside <- st_within(
  casts_sf,
  GS_poly,
  sparse = FALSE
)

# Create table of casts inside polygon
germanseal_casts <- cast_locations[inside[,1], ]

# Now subset the full profile data
germanseal_profiles <- final_casts %>%
  filter(cast %in% germanseal_casts$cast)


  } else {
  final_casts <- data.frame()
}


# Clean date table
if (nrow(all_dates) > 0) {
  all_dates <- unique(all_dates)
  all_dates <- all_dates[order(all_dates$date), ]
}


head(final_casts) 
unique(final_casts$station_id)
                                
                                
                                ### extract from data
                                
                                casts_sf <- st_as_sf(all_casts,coords = c("longitude", "latitude"),crs = 4326)
                                
                                inside <- st_within(
                                  casts_sf,
                                  lurcher_poly,
                                  sparse = FALSE
                                )
                                
                                lurcher_casts <- all_casts[inside[,1], ]
          
# Save outputs
write.csv(all_dates, "Prince5_cast_dates.csv", row.names = FALSE)
write.csv(final_casts, "Prince5_all_casts.csv", row.names = FALSE)



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

# QC
# where is 2024? Missing




## Next Step ------>
## Take the dataset and create an average per month per year to compare with HSC

######## Make changes to create comparability to HSC data:


# Cut top 5m
final_casts_deep <- final_casts[final_casts$depth >= 5, ]
final_casts_deep <- final_casts_deep[final_casts_deep$depth <= 30, ]

min(final_casts_deep$depth)
max(final_casts_deep$depth)



# Calculate monthly averages for salinity and temperature and also stratification
# If there is more than one cast in a month/year, combine them into one monthly average

# Make sure date is a Date
final_casts_deep$date <- as.Date(final_casts_deep$date)


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
# 1. Average each individual cast across depth
# -----------------------------

cast_averages <- final_casts_deep %>%
  group_by(file, cast, date, station_id, latitude, longitude) %>%
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

# -----------------------------
# 2. Add year/month fields to cast-level averages
# -----------------------------

cast_averages <- cast_averages %>%
  mutate(
    year = as.numeric(format(date, "%Y")),
    month = as.numeric(format(date, "%m")),
    month_name = format(date, "%B")
  )




# -----------------------------
# 3. Combine casts within same year/month
#    This gives ONE value per month per year
# -----------------------------



monthly_cast_averages <- cast_averages %>%
  group_by(year, month, month_name, station_id) %>%
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
  arrange(year, month)

# Optional: save the monthly averaged dataframe
write.csv(
  monthly_cast_averages,
  "Prince5_monthly_cast_averages_between5-30.csv",
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

# Show months where more than one cast was combined
monthly_cast_averages %>%
  filter(n_casts > 1)
