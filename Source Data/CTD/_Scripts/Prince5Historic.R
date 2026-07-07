

## cut top 5m to create the same comparison as modern?
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/.nc")

library(ncdf4)
library(maps)
library(dplyr)
library(lubridate)
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


#1977 1978 1979 1980 1982 1985 1989 1990 1992 1993 1998 2009

years <- c(1977, 1978, 1979, 1980, 1982, 1985, 1989, 1990, 1992, 1993, 1998, 2009)
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
      
########################## Need to determine what instrument types are listed and under what variable : use instrument_ID_manual

      # LOOP - take files in .nc folder                  
      
      for (f in files) {
        
        cat("\n--- Processing:", f, "---\n")
        
        nc <- nc_open(f)
        
        # List variable names
        var_names <- names(nc$var)
        print(var_names)
    
        
        
        # Load variables
        temp <- ncvar_get(nc, "temperature")
        sal  <- ncvar_get(nc, "salinity")
        lat  <- ncvar_get(nc, "latitude")
        lon  <- ncvar_get(nc, "longitude")
        instrument <- ncvar_get(nc, "instrument_ID_manual")
        
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
          instrument_type_i <- instrument[i]
          
          # Store date table
          all_dates <- rbind(all_dates, data.frame(
            file = f,
            cast = i,
            date = cast_date,
            station_id = station_i,
            latitude = lat_i,
            longitude = lon_i,
            instrumentType = instrument_type_i
          ))
          
          # Store full cast dataframe
          cast_df <- data.frame(
            file = f,
            cast = i,
            date = cast_date,
            station_id = station_i,
            latitude = lat_i,
            longitude = lon_i,
            instrumentType = instrument_type_i,
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
      
      unique(final_casts$instrumentType)
      
      # BO = bottle, CT= CTD
                final_casts %>%
                      count(date, instrumentType)
      
# the change from bottles to CTD came between October/November 1990 for Prince 5

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
      
        final_casts$Year = year(final_casts$date)      
            head(final_casts)
                hist(final_casts$n_rows_in_cast)     
                
                final_casts$month = month(final_casts$date)
                
                
### Plot - average number of rows per cast
                
          # First data prep
                cast_row_counts <- final_casts %>%
                  group_by(Year, month, cast) %>%
                  summarise(
                    n_rows_in_cast = n(),
                    .groups = "drop"
                  )
                
                write.csv(cast_row_counts, "Prince5_castRowCounts.csv", row.names = FALSE)
                
                
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
                
                ### data pre 1990 seems to use the same equipment
                      ## 1990 has more data points
                          # 1993 even more
                            # 2009 was CTD.
                                ## need equipment type..only compare the same type.
                # Could look at the 1970/1980s? 
                
# I don't think it matters as they should be the same? for Prince 5, the data changed between October 1990 and November 1990. 
                

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
            head(DFO)      

          hist(DFO$n_per_id)            
            
# remove IDs with casts that have less than 5 measurements
      DFO_filtered <- DFO %>%
        filter(n_per_id >= 5)
      
      

      # Summarize average number of rows per cast per year
      # add month variable:
      DFO_filtered$month = month(DFO_filtered$Date)
      
      cast_row_counts_DFO <- DFO_filtered %>%
        distinct(Year, month, id, n_per_id) %>%
        rename(
          cast = id,
          n_rows_in_cast = n_per_id
        ) %>%
        arrange(Year, month)
      
      write.csv(cast_row_counts_DFO, "DFO_castRowCounts.csv", row.names = FALSE)
      
      
      
        avg_rows_per_cast_year_DFO <- DFO_filtered %>%
        distinct(Year, id, n_per_id, month) %>%   # one row per cast/id
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

      unique(DFO$Year)    
                
      
# select only GB for now
      DFO_german_bank <- DFO_filtered %>%
        filter(ground == "German Bank")
      
      unique(DFO_german_bank$Year)    
          #how many years?  1977, 1978, 1980, 1982, 1985, 1989  (pre 1990)  
              # 1989 looks like it has some CTD potentially. 1977-1985 safer for one set of comparisons
      
      
      ### use data before 1986 - NO KEEP ALL
      #####DFO2 <- DFO_german_bank %>%
        ###filter(Year < 1986)
      
            DFO2 = DFO_german_bank
# what months?
      head(DFO2)
    
              # what months do we have?
                    unique(DFO2$month)
                      # July, August, October and November
                    
  
                    
      hist(DFO2$Depth)
      hist(final_casts$depth)
            head(final_casts)
            
            
            # explore max depths for Prince 5
            max_depths <- final_casts %>%
              group_by(cast) %>%
              summarize(max_depth = max(depth, na.rm = TRUE))
            
            hist(
              max_depths$max_depth,
              breaks = 30,
              main = "Distribution of Maximum Cast Depths",
              xlab = "Maximum Depth (m)",
              xaxt = "n"
            )
            
            axis(
              1,
              at = seq(
                floor(min(max_depths$max_depth, na.rm = TRUE)),
                ceiling(max(max_depths$max_depth, na.rm = TRUE)),
                by = 10
              )
            )
      
              ## 70-over 100m
            
            head(DFO2)
            
            #explore max depth for DFO
            max_depths <- DFO2 %>%
              group_by(id) %>%
              summarize(max_depth = max(Depth, na.rm = TRUE))
            
            hist(
              max_depths$max_depth,
              breaks = 30,
              main = "Distribution of Maximum Cast Depths",
              xlab = "Maximum Depth (m)"
            )
            
            ## 2m-over 100m
            
            length(unique(final_casts$cast))
            length(unique(DFO2$id))
            
            
            
            # Filter Prince 5 for the 4 months there is GB data: July, August, October and November
            
            head(final_casts)
            final_casts$month = month(final_casts$date)
              
            final_casts <- final_casts %>%
              filter(month %in% c(7, 8, 10, 11))
            
            
            ## How do the number of casts compare when month is synced?
            
            length(unique(final_casts$cast))
            length(unique(DFO2$id))
            
            # not bad...
      
            # 70m seems to be a consistent depth...might go a little shallower for the GB data - can try different things and see how they effect n (maybe try 30m to start to match what I've done for the modern)
            
# add max depth to Prince5 :  final_casts
            final_casts <- final_casts %>%
              group_by(cast) %>%
              mutate(maxDepth = max(depth, na.rm = TRUE)) %>%
              ungroup()

            # remove casts with maximum depth < 70m, and then chop casts that are deeper to 70m for all.
            
            x = 30
            
            final_casts_Depth <- final_casts %>%
              filter(maxDepth >= x) %>%  # remove shallow casts
              filter(depth <= x)         # keep only 0-70 m
            
            DFO_Depth = DFO2 %>% 
              filter(maximum_depth >= x) %>%
              filter(Depth <= x)
            
            
            
            length(unique(final_casts_Depth$cast))
            length(unique(DFO_Depth$id))
            # good - 69 versus 67 casts
            
            final_casts_Depth$id = final_casts_Depth$cast 
            
            
            
            
    ## lets see the overlap (monthly):
            
            casts_by_month <- bind_rows(
              final_casts_Depth %>%
                distinct(month, id) %>%
                count(month, name = "n_casts") %>%
                mutate(dataset = "final_casts_Depth"),
              
              DFO_Depth %>%
                distinct(month, id) %>%
                count(month, name = "n_casts") %>%
                mutate(dataset = "DFO_Depth")
            )
            
            ggplot(casts_by_month, aes(x = month, y = n_casts, fill = dataset)) +
              geom_col(position = "dodge") +
              scale_x_continuous(breaks = 1:12) +
              labs(
                title = "Number of Casts per Month",
                x = "Month",
                y = "Number of unique casts",
                fill = "Dataset"
              ) +
              theme_bw()
            

## create 1 df with both:
            
            
head(final_casts_Depth)
head(DFO_Depth)            

# Rename DFO columns to match final_casts_70m
final_clean <- final_casts_Depth %>%
  transmute(
    source = "Final",
    cast = as.character(id),   # or cast if you prefer
    date,
    Year,
    month,
    station = station_id,
    latitude,
    longitude,
    depth,
    temperature,
    salinity
  )

DFO_clean <- DFO_Depth %>%
  transmute(
    source = "DFO",
    cast = as.character(id),
    date = Date,
    Year,
    month,
    station = stn_id,
    latitude = Lat,
    longitude = Lon,
    depth = Depth,
    temperature = Temperature,
    salinity = Salinity
  )

final_clean <- final_clean %>%
  mutate(source = "Prince5")


combined_df <- bind_rows(final_clean, DFO_clean)


head(combined_df)
unique(combined_df$source)

combined_df <- combined_df %>%
  group_by(cast) %>%
  mutate(
    maxDepth = max(depth, na.rm = TRUE)
  ) %>%
  ungroup()


hist(combined_df$maxDepth)

# remove casts with maxdepth less than 20

combined_df <- combined_df %>%
  filter(maxDepth >= 20)


head(combined_df)

hist(combined_df$maxDepth)

## calculate cast level metrics:

mean_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

cast_metrics <- combined_df %>%
  filter(maxDepth >= 20) %>%
  group_by(source, cast, Year, month, date, station, latitude, longitude, maxDepth) %>%
  summarise(
    n_depths = n(),
    
    mean_temp = mean_na(temperature),
    mean_salinity = mean_na(salinity),
    
    surface_temp = mean_na(temperature[depth <= 10]),
    bottom_temp  = mean_na(temperature[depth >= maxDepth - 10]),
    
    surface_salinity = mean_na(salinity[depth <= 10]),
    bottom_salinity  = mean_na(salinity[depth >= maxDepth - 10]),
    
    temp_stratification = surface_temp - bottom_temp,
    salinity_stratification = bottom_salinity - surface_salinity,
    
    .groups = "drop"
  )


head(cast_metrics)

cast_metrics %>%
  count(source)

# Compare only matched months/years:
matched_cast_metrics <- cast_metrics %>%
  group_by(Year, month) %>%
  filter(n_distinct(source) == 2) %>%
  ungroup()

matched_cast_metrics <- matched_cast_metrics %>%
  mutate(
    source = factor(
      source,
      levels = c("Prince5", "DFO")
    )
  )

model_temp_strat <- lm(
  temp_stratification ~ source + factor(month) + factor(Year),
  data = matched_cast_metrics
)

summary(model_temp_strat)
anova(model_temp_strat)


### Result - - - > Temperature stratification varied strongly with season (month), but there was no detectable difference between the Prince5 and DFO grounds after accounting for seasonal and interannual variability.

# cast level temperature stratification by source:
ggplot(matched_cast_metrics,
       aes(x = source,
           y = temp_stratification,
           fill = source)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
  labs(
    x = "Ground / Source",
    y = "Temperature stratification",
    title = "Cast-level temperature stratification by source"
  ) +
  theme_bw()



## Sample size issue?
matched_cast_metrics %>%
  count(source)

matched_cast_metrics %>%
  count(Year, month, source) %>%
  arrange(Year, month, source)


write.csv(x = matched_cast_metrics, "sampleSizes.csv")


cast_metrics %>%
  group_by(source) %>%
  summarise(
    mean_depth = mean(maxDepth),
    sd_depth = sd(maxDepth),
    min_depth = min(maxDepth),
    max_depth = max(maxDepth)
  )


ggplot(cast_metrics,
       aes(source, maxDepth, fill = source)) +
  geom_boxplot() +
  theme_bw()

# differences in max depth an issue?
ggplot(cast_metrics,
       aes(maxDepth, temp_stratification,
           colour = source)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

cor.test(
  cast_metrics$maxDepth,
  cast_metrics$temp_stratification
)

model_depth <- lm(
  temp_stratification ~ source +
    maxDepth +
    factor(month) +
    factor(Year),
  data = matched_cast_metrics
)

summary(model_depth)
anova(model_depth)

### conclusion: There is little evidence that differences in maximum cast depth are driving the stratification patterns.

### Maximum cast depth was similar between grounds (mean depth 26.7 m for DFO and 27.4 m for Prince5) and was not significantly related to temperature stratification. Adding maximum cast depth as a covariate did not materially alter the estimated source effect. Temperature stratification varied strongly among months (p < 0.001), whereas neither ground/source nor year explained a significant proportion of variation in stratification


# Seasonal variation is the dominant driver of temperature stratification, and there is little evidence for a strong difference between the Prince5 and DFO grounds in the 20–30 m water column sampled.



################### density stratification:


## calculate density for every observation:
# install.packages("gsw")
library(gsw)

density_df <- combined_df %>%
  filter(
    !is.na(temperature),
    !is.na(salinity),
    !is.na(depth)
  ) %>%
  mutate(
    SA = gsw_SA_from_SP(
      salinity,
      depth,
      longitude,
      latitude
    ),
    
    CT = gsw_CT_from_t(
      SA,
      temperature,
      depth
    ),
    
    sigma0 = gsw_sigma0(
      SA,
      CT
    )
  )


## calculate per cast:
mean_na <- function(x) {
  if(all(is.na(x))) NA_real_
  else mean(x, na.rm = TRUE)
}

density_cast_metrics <- density_df %>%
  group_by(
    source,
    cast,
    Year,
    month,
    maxDepth
  ) %>%
  summarise(
    
    surface_density =
      mean_na(sigma0[depth <= 10]),
    
    bottom_density =
      mean_na(
        sigma0[
          depth >= (maxDepth - 10)
        ]
      ),
    
    density_stratification =
      bottom_density -
      surface_density,
    
    .groups = "drop"
  )

# only matched
matched_density <- density_cast_metrics %>%
  group_by(Year, month) %>%
  filter(n_distinct(source) == 2) %>%
  ungroup()


## stats:

table(matched_density$source)
table(matched_density$Year)

table(matched_density$month)

matched_density <- matched_density %>%
  mutate(
    source = factor(
      source,
      levels = c("Prince5", "DFO")
    )
  )

model_density <- lm(
  density_stratification ~ source + factor(Year),
  data = matched_density
)

summary(model_density)
anova(model_density)

matched_density %>%
  group_by(source) %>%
  summarise(
    mean = mean(density_stratification, na.rm = TRUE),
    sd = sd(density_stratification, na.rm = TRUE),
    n = n()
  )



###Temperature structure is broadly similar between the grounds, but incorporation of salinity reveals persistent differences in density stratification. This suggests that salinity contributes importantly to water-column structure and that density-based metrics may better distinguish the Prince5 and DFO grounds than temperature alone.


### density strat. plots:

#1.)
ggplot(matched_density,
       aes(x = factor(Year),
           y = density_stratification,
           fill = source)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    alpha = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(colour = source),
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.8
    ),
    alpha = 0.6,
    size = 2
  ) +
  labs(
    x = "Year",
    y = expression("Density stratification " * Delta * sigma[0]),
    fill = "Source",
    colour = "Source",
    title = "Density stratification by year and source",
    subtitle = "October matched casts only"
  ) +
  theme_bw()


#2.)

density_summary <- matched_density %>%
  group_by(Year, source) %>%
  summarise(
    mean_density_strat = mean(density_stratification, na.rm = TRUE),
    sd_density_strat = sd(density_stratification, na.rm = TRUE),
    n = sum(!is.na(density_stratification)),
    se = sd_density_strat / sqrt(n),
    .groups = "drop"
  )

ggplot(density_summary,
       aes(x = factor(Year),
           y = mean_density_strat,
           colour = source,
           group = source)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_density_strat - se,
      ymax = mean_density_strat + se
    ),
    width = 0.15
  ) +
  labs(
    x = "Year",
    y = expression("Mean density stratification " * Delta * sigma[0]),
    colour = "Source",
    title = "Mean density stratification by year and source",
    subtitle = "Error bars show ±1 SE; October matched casts only"
  ) +
  theme_bw()

#3.
ggplot(matched_density,
aes(x = source,
    y = density_stratification,
    fill = source)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 4,
    fill = "white"
  ) +
  facet_wrap(~ Year) +
  labs(
    x = "Source",
    y = expression("Density stratification " * Delta * sigma[0]),
    fill = "Source",
    title = "Density stratification by source within each year",
    subtitle = "White diamonds show yearly source means"
  ) +
  theme_bw()


########################## Continued sample size concerns:

matched_density %>%
  count(Year, source) %>%
  arrange(Year, source)


## yearly source means instead:

density_yearly <- matched_density %>%
  group_by(Year, source) %>%
  summarise(
    mean_density_strat = mean(density_stratification, na.rm = TRUE),
    sd_density_strat = sd(density_stratification, na.rm = TRUE),
    n_casts = sum(!is.na(density_stratification)),
    .groups = "drop"
  )

density_yearly

density_yearly <- density_yearly %>%
  mutate(
    source = factor(source, levels = c("Prince5", "DFO"))
  )

model_density_yearly <- lm(
  mean_density_strat ~ source + factor(Year),
  data = density_yearly
)

summary(model_density_yearly)
anova(model_density_yearly)

### DFO-Prince 5 within each year:
library(tidyr)

density_diff <- density_yearly %>%
  select(Year, source, mean_density_strat, n_casts) %>%
  pivot_wider(
    names_from = source,
    values_from = c(mean_density_strat, n_casts)
  ) %>%
  mutate(
    diff_DFO_minus_Prince5 =
      mean_density_strat_DFO - mean_density_strat_Prince5
  )

density_diff

t.test(density_diff$diff_DFO_minus_Prince5)

wilcox.test(
  density_diff$diff_DFO_minus_Prince5,
  exact = FALSE
)

library(ggplot2)

ggplot(density_diff,
       aes(x = factor(Year),
           y = diff_DFO_minus_Prince5)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey50") +
  geom_col(fill = "steelblue") +
  labs(
    x = "Year",
    y = expression("Density stratification difference: DFO - Prince5 " * Delta * sigma[0]),
    title = "Paired yearly differences in density stratification",
    subtitle = "Positive values mean DFO is more density-stratified"
  ) +
  theme_bw()


### robustenss test
set.seed(123)

balanced_density <- matched_density %>%
  group_by(Year) %>%
  group_modify(~ {
    n_min <- .x %>%
      count(source) %>%
      summarise(n_min = min(n)) %>%
      pull(n_min)
    
    .x %>%
      group_by(source) %>%
      slice_sample(n = n_min) %>%
      ungroup()
  }) %>%
  ungroup()

balanced_density %>%
  count(Year, source)
balanced_density <- balanced_density %>%
  mutate(
    source = factor(source, levels = c("Prince5", "DFO"))
  )

model_density_balanced <- lm(
  density_stratification ~ source + factor(Year),
  data = balanced_density
)

summary(model_density_balanced)
anova(model_density_balanced)

# run many times:
set.seed(123)

balance_results <- replicate(1000, {
  
  balanced_density <- matched_density %>%
    group_by(Year) %>%
    group_modify(~ {
      n_min <- .x %>%
        count(source) %>%
        summarise(n_min = min(n)) %>%
        pull(n_min)
      
      .x %>%
        group_by(source) %>%
        slice_sample(n = n_min) %>%
        ungroup()
    }) %>%
    ungroup() %>%
    mutate(source = factor(source, levels = c("Prince5", "DFO")))
  
  mod <- lm(
    density_stratification ~ source + factor(Year),
    data = balanced_density
  )
  
  coef(summary(mod))["sourceDFO", "Estimate"]
})

summary(balance_results)
quantile(balance_results, c(0.025, 0.5, 0.975))

##Your original density model said:DFO has density stratification about 0.114 kg m⁻³ higher than Prince5 after accounting for year.Your balanced resampling says:Even when DFO is sample-size matched to Prince5 within years, the estimated DFO effect remains positive and similar in magnitude.So your concern about DFO having many more points is valid, but this result suggests the density stratification difference is not being driven solely by unequal sample size.


### Because DFO had more casts than Prince5 within several matched years, I performed a balanced resampling analysis in which DFO casts were randomly downsampled to match the number of Prince5 casts within each year. Across 1000 resampling iterations, the estimated DFO effect remained consistently positive, with a median effect of 0.116 kg m⁻³ and a 95% resampling interval of 0.069 to 0.186 kg m⁻³. This suggests that greater density stratification at DFO was robust to sample-size imbalance.


ggplot(data.frame(source_effect = balance_results),
       aes(x = source_effect)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    colour = "white",
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "red",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = median(balance_results),
    colour = "black",
    linewidth = 1
  ) +
  labs(
    x = expression("Balanced source effect: DFO - Prince5 " * Delta * sigma[0]),
    y = "Number of resampling iterations",
    title = "Balanced resampling estimates of DFO density stratification effect",
    subtitle = "DFO was downsampled to match Prince5 sample size within each year"
  ) +
  theme_bw()

### check means and sample sizes:

matched_cast_metrics %>%
  group_by(source) %>%
  summarise(
    mean = mean(temp_stratification, na.rm = TRUE),
    sd = sd(temp_stratification, na.rm = TRUE),
    n = n(),
    se = sd/sqrt(n),
    lower = mean - 1.96*se,
    upper = mean + 1.96*se
  )

#Mean temperature stratification was higher for DFO casts than Prince5 casts in the matched dataset, with DFO averaging 0.265°C and Prince5 averaging 0.110°C. However, confidence intervals overlapped and the source effect was not statistically significant after accounting for month and year. Sampling effort was unequal, with more DFO casts than Prince5 casts, so the source comparison should be interpreted cautiously.




# with both Prince5 and DFO
              ungroup()
                      # Plot
            ggplot(monthly_strat_matched,
                   aes(x = factor(month),
                       y = mean_stratification,
                       colour = source,
                       group = source)) +
              geom_point(size = 2.5) +
              facet_wrap(~ Year) +
              labs(
                x = "Month",
                y = "Mean stratification",
                colour = "Source",
                title = "Monthly stratification comparison for matched Year-Months"
              ) +
              theme_bw()
                              
            monthly_strat_matched %>%
              select(Year, month, source, n_casts) %>%
              arrange(Year, month, source)
            
            cast_strat <- combined_df %>%
              group_by(source, Year, month, cast) %>%
              summarise(
                surface_temp = mean(temperature[depth <= 10], na.rm = TRUE),
                bottom_temp = mean(temperature[depth >= 60], na.rm = TRUE),
                stratification = (surface_temp - bottom_temp) / 70,
                .groups = "drop"
              )
            
            
            ggplot(monthly_strat,
                   aes(month, mean_stratification,
                       colour = source,
                       group = source))  +
              geom_point() +
              labs(y = "Mean stratification (°C)",
                   x = "Month")
            
            length(unique(combined_df$cast))

            
            # how many casts per source?
            combined_df %>%
              group_by(source) %>%
              summarise(
                n_casts = n_distinct(cast),
                .groups = "drop"
              )
            
cast_strat
            
            #### monthly average/ground
                      ### plot monthly ground means with uncertainity
            
            

# response ~ ground + month or (* month if difference depends on month)

            # average temperature and salinity per cast and stratification

      
      
#####plan forwards ---->
      
      # filter to comparable depth range
      # compute cast level summaries
      # Then monthly averages
      # compare grounds using matched months