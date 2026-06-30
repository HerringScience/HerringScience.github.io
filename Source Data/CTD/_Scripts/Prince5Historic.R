


setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/.nc")

library(ncdf4)
library(maps)
library(dplyr)
library(ggplot2)

# helper function
read_char_var <- function(nc, varname) {
  raw <- ncvar_get(nc, varname)
  
  if (is.matrix(raw)) {
    return(trimws(apply(raw, 2, paste, collapse = "")))
  } else {
    return(trimws(raw))
  }
}

years <- c(1977, 1978, 1979, 1980, 1989, 1990, 1993, 2009)
files <- paste0(years, ".nc")

out_dir <- "Casts"
dir.create(out_dir, showWarnings = FALSE)

# Store outputs
      all_dates <- data.frame()
      all_casts <- list()
      counter <- 1
      
      # Small helper for map axis padding
      pad_range <- function(x, pad = 0.05) {
        x <- x[!is.na(x)]
        
        if (length(x) == 0) return(c(NA, NA))
        
        if (length(unique(x)) == 1) {
          return(c(x[1] - 0.05, x[1] + 0.05))
        }
        
        r <- range(x, na.rm = TRUE)
        buffer <- diff(r) * pad
        c(r[1] - buffer, r[2] + buffer)
      }      

      # LOOP - take files in .nc folder                  
      
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
        
        # Extract only Prince-5    
        # Prince-5 matching; catches Prince-5, Prince 5, Prince5
        idx <- grep("Prince[- ]?5", station_id, ignore.case = TRUE)
        
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

      # Save outputs
      write.csv(all_dates, "Prince5_cast_datesHistoric.csv", row.names = FALSE)
      write.csv(final_casts, "Prince5_all_castsHistoric.csv", row.names = FALSE)

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

      
      min(final_casts$depth)
      max(final_casts$depth)
      hist(final_casts$depth)

      # Make sure date is a Date
      final_casts$date <- as.Date(final_casts$date)
      head(final_casts)
      
      hist(final_casts$depth)
      hist(final_casts$cast)
      unique(final_casts$cast)
      
      final_casts <- final_casts %>%
        group_by(cast) %>%
        mutate(n_rows_in_cast = n()) %>%
        ungroup()
      
      library(lubridate)
      
        final_casts$Year = year(final_casts$date)      
            head(final_casts)
                hist(final_casts$n_rows_in_cast)     
                
                
                
###################### Plot - average number of rows per cast
                ### 
                
          # First data prep
                cast_row_counts <- final_casts %>%
                  group_by(Year, cast) %>%
                  summarise(
                    n_rows_in_cast = n(),
                    .groups = "drop"
                  )
                
                avg_rows_per_cast_year <- cast_row_counts %>%
                  group_by(Year) %>%
                  summarise(
                    mean_rows_per_cast = mean(n_rows_in_cast, na.rm = TRUE),
                    sd_rows_per_cast = sd(n_rows_in_cast, na.rm = TRUE),
                    n_casts = n(),
                    .groups = "drop"
                  )
          
            # Run plot:
                ggplot(avg_rows_per_cast_year, aes(x = Year, y = mean_rows_per_cast)) +
                  geom_col(fill = "steelblue") +
                  geom_errorbar(
                    aes(
                      ymin = mean_rows_per_cast - sd_rows_per_cast,
                      ymax = mean_rows_per_cast + sd_rows_per_cast
                    ),
                    width = 0.2
                  ) +
                  geom_text(
                    aes(label = round(mean_rows_per_cast, 1)),
                    vjust = -0.5,
                    size = 3
                  ) +
                  labs(
                    x = "Year",
                    y = "Average number of rows per cast",
                    title = "Average number of rows per cast by year",
                    subtitle = "Error bars show ±1 SD"
                  ) +
                  theme_minimal()
                
                
                
                
      

                
# Load in DFO larval data;
      
                
DFO <- readRDS("OceansAllDepths.rds")
  DFO=DFO[which(DFO$Source == "DFO"), ]
    head(DFO)      
        
        write.csv(
          DFO,
          "DFOHist.csv",
          row.names = FALSE
        )
      
      hist(DFO$Depth)      
      
# remove IDs with casts that have less than 5 measurements
      DFO_filtered <- DFO %>%
        filter(n_per_id >= 5)

      # Summarize average number of rows per cast per year
        avg_rows_per_cast_year_DFO <- DFO_filtered %>%
        distinct(Year, id, n_per_id) %>%   # one row per cast/id
        group_by(Year) %>%
        summarise(
          mean_rows_per_cast = mean(n_per_id, na.rm = TRUE),
          sd_rows_per_cast = sd(n_per_id, na.rm = TRUE),
          n_casts = n(),
          .groups = "drop"
        )
      
      # Plot -average number of measurements per cast
      ggplot(avg_rows_per_cast_year_DFO, aes(x = Year, y = mean_rows_per_cast)) +
        geom_col(fill = "steelblue") +
        geom_errorbar(
          aes(
            ymin = mean_rows_per_cast - sd_rows_per_cast,
            ymax = mean_rows_per_cast + sd_rows_per_cast
          ),
          width = 0.2
        ) +
        geom_text(
          aes(label = round(mean_rows_per_cast, 1)),
          vjust = -0.5,
          size = 3
        ) +
        labs(
          x = "Year",
          y = "Average number of rows per cast",
          title = "Average number of rows per cast by year",
          subtitle = "DFO data; error bars show ±1 SD"
        ) +
        theme_minimal()
      
## Plot to show the actual span of data, not averages:

      DFO_unique <- DFO_filtered %>%
        distinct(Year, id, n_per_id)
      
      ggplot(DFO_unique, aes(x = Year, y = n_per_id)) +
        geom_point(alpha = 0.5, color = "steelblue") +
        labs(
          x = "Year",
          y = "Number of rows per cast (n_per_id)",
          title = "Distribution of rows per cast by year (DFO)"
        ) +
        theme_minimal()
    
      
# select only GB for now
      DFO_german_bank <- DFO_filtered %>%
        filter(ground == "German Bank")
      
# what months?
      head(DFO_german_bank)
      # add month variable:
            DFO_german_bank$month = month(DFO_german_bank$Date)
              # what months do we have?
                    unique(DFO_german_bank$month)
                      # July, August, October and November
                    
  
                    
      hist(DFO_german_bank$Depth)
      
      
# average temperature and salinity per cast and stratification

      
      
#####plan forwards ---->
      
      # filter to comparable depth range
      # compute cast level summaries
      # Then monthly averages
      # compare grounds using matched months