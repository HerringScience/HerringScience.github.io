
# Load Prince 5 Data from 
# https://www.frdr-dfdr.ca/repo/dataset/167e21b2-7eb9-466a-9b80-3980de093b15


getwd()
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/.nc")

library(ncdf4)

years <- 2017:2024
files <- paste0(years, ".nc")


nc_list <- lapply(files, nc_open)

# Get variable names from first file
var_names <- names(nc_list[[1]]$var)

# Create a list to store ALL variables
all_data <- list()

for (var in var_names) {
  
  all_data[[var]] <- lapply(nc_list, function(nc) {
    ncvar_get(nc, var)
  })
  
}



# Combine variables from the different years:

for (var in var_names) {
  
  # Check if variable is 2D (like temp/sal)
  if (length(dim(all_data[[var]][[1]])) == 2) {
    all_data[[var]] <- do.call(cbind, all_data[[var]])
  } else {
    all_data[[var]] <- unlist(all_data[[var]])
  }
}


time_raw <- all_data$time

origin <- as.POSIXct("1900-01-01", tz = "UTC")
time_all <- origin + time_raw


# close files
lapply(nc_list, nc_close)






#load data
# One cast per month

nc <- nc_open("1993.nc")
print(nc)
names(nc$var)
names(nc$dim)

temp <- ncvar_get(nc, "temperature")
sal <- ncvar_get(nc, "salinity")
lat <- ncvar_get(nc, "latitude")
lon <- ncvar_get(nc, "longitude")
instrument_ID_manual <- ncvar_get(nc, "instrument_ID")
instrument_ID <- ncvar_get(nc, "instrument_ID_manual")
source <- ncvar_get(nc, "source")
depth <- ncvar_get(nc, "sounder_depth")
station_id <- ncvar_get(nc, "station_ID")
trip_id <- ncvar_get(nc, "trip_ID")

level <- nc$dim$level$vals
time <- nc$dim$time$vals

i <- 1  # cast number

profile <- temp[, i]
depth <- level
plot(profile, depth,
     type = "l",
     ylim = rev(range(depth, na.rm = TRUE)),
     xlab = "Temperature",
     ylab = "Depth",
     main = paste("Cast", i))

length(temp[, 1]) == length(level)

plot(temp[, 10], level,
     type = "l",
     ylim = rev(range(level)))

surface_temp <- temp[1, ]

depth_index <- which.min(abs(level - 50))
temp_50m <- temp[depth_index, ]


# only CTD casts
ctd_index <- which(instrument == "CT")
temp_ctd <- temp[, ctd_index]

station_id <- ncvar_get(nc, "station_ID")
unique(station_id)

# filter out for Prince5
idx <- which(station_id == "Prince-5")
length(idx)

temp_p5 <- temp[, idx]
sal_p5  <- sal[, idx]
lat_p5 <- lat[idx]
lon_p5 <- lon[idx]


depth_idx <- which(level >= 0 & level <= 100)
matplot(temp[depth_idx, idx], level[depth_idx],
        type = "l",
        lty = 1,
        ylim = c(100, 0),
        xlab = "Temperature",
        ylab = "Depth (m)",
        main = "Prince-5 (0–100 m)")
        main = "Prince-5 Profiles")

range(level[depth_idx])
summary(temp[, idx])

plot(temp[, idx[1]], level,
     type = "l",
     ylim = c(100, 0),
     main = "Single Cast - Prince-5")

range(level)
head(level)

apply(!is.na(temp), 2, sum)


# how deep does the data go?

# Prince-5 subset
temp_p5 <- temp[, idx]
sal_p5  <- sal[, idx]

# maximum sampled depth for temperature per cast
max_depth_temp <- apply(temp_p5, 2, function(x) {
  if (all(is.na(x))) NA else max(level[!is.na(x)])
})

# maximum sampled depth for salinity per cast
max_depth_sal <- apply(sal_p5, 2, function(x) {
  if (all(is.na(x))) NA else max(level[!is.na(x)])
})

# combine into one depth coverage summary
depth_coverage <- data.frame(
  cast_index = idx,
  max_depth_temp = max_depth_temp,
  max_depth_sal = max_depth_sal,
  max_depth_any = pmax(max_depth_temp, max_depth_sal, na.rm = TRUE)
)

head(depth_coverage)
summary(depth_coverage$max_depth_any)
sum(depth_coverage$max_depth_any >= 10, na.rm = TRUE)
sum(depth_coverage$max_depth_any >= 25, na.rm = TRUE)
sum(depth_coverage$max_depth_any >= 50, na.rm = TRUE)
sum(depth_coverage$max_depth_any >= 100, na.rm = TRUE)

hist(depth_coverage$max_depth_any,
     breaks = 20,
     xlab = "Maximum sampled depth / pressure level",
     main = "Depth coverage of Prince-5 casts")
depth_idx <- which(level >= 0 & level <= 100)


deep_p5_idx <- idx[depth_coverage$max_depth_any >= 50]

length(deep_p5_idx)
matplot(temp[depth_idx, deep_p5_idx], level[depth_idx],
        type = "l",
        lty = 1,
        ylim = c(100, 0),
        xlab = "Temperature",
        ylab = "Depth / pressure level",
        main = "Prince-5 casts reaching 100 m")

table(cut(depth_coverage$max_depth_any,
          breaks = c(0, 10, 25, 50, 75, 100)))


depth_idx <- which(level >= 0 & level <= 100)

plot(NA, NA,
     xlim = range(temp[depth_idx, idx], na.rm = TRUE),
     ylim = c(100, 0),
     xlab = "Temperature",
     ylab = "Depth / pressure level",
     main = "Prince-5 casts, 0–100 m")

for (i in idx) {
  lines(temp[depth_idx, i], level[depth_idx],
        col = rgb(0, 0, 1, 0.25))
}

i <- idx[1]

valid <- !is.na(temp[, i])

plot(temp[valid, i], level[valid],
     type = "l",
     ylim = c(100, 0),
     xlab = "Temperature",
     ylab = "Depth / pressure level",
     main = paste("Prince-5 cast", i))




#### Plots
sapply(idx_temp, function(i) max(level[!is.na(temp[, i])], na.rm = TRUE))

# Depth range to plot
depth_idx <- which(level >= 0 & level <= 100)

# Prince-5 cast indices
idx <- which(station_id == "Prince-5")

# Optional: only use casts with at least some temperature data in 0-100 m
idx_temp <- idx[sapply(idx, function(i) any(!is.na(temp[depth_idx, i])))]

# Set x-axis range from only valid Prince-5 data in 0-100 m
xrange <- range(temp[depth_idx, idx_temp], na.rm = TRUE)

# Blank plotting window
plot(NA, NA,
     xlim = xrange,
     ylim = c(100, 0),
     xlab = "Temperature",
     ylab = "Depth / pressure level",
     main = "Prince-5 temperature profiles, 0-100 m")

# Add each cast manually
for (i in idx_temp) {
  valid_full <- !is.na(temp[, i])
  valid <- valid_full[depth_idx]
  
  lines(temp[depth_idx, i][valid],
        level[depth_idx][valid],
        col = rgb(0, 0, 1, 0.35),
        lwd = 1)
}





## only 12 temp casts....
max_depth_temp <- apply(temp[, idx], 2, function(x) {
  if (all(is.na(x))) NA else max(level[!is.na(x)])
})

max_depth_sal <- apply(sal[, idx], 2, function(x) {
  if (all(is.na(x))) NA else max(level[!is.na(x)])
})

summary(max_depth_temp)
summary(max_depth_sal)

sum(max_depth_temp >= 75, na.rm = TRUE)
sum(max_depth_sal >= 75, na.rm = TRUE)



####
depth_idx <- which(level >= 0 & level <= 100)

plot(NA, NA,
     xlim = range(temp[depth_idx, idx], na.rm = TRUE),
     ylim = c(100, 0),
     xlab = "Temperature",
     ylab = "Depth",
     main = "Prince-5 (0–100 m)")

for (i in idx) {
  
  valid_full <- !is.na(temp[, i])
  valid <- valid_full[depth_idx]
  
  if (any(valid)) {
    lines(temp[depth_idx, i][valid],
          level[depth_idx][valid],
          col = rgb(0, 0, 1, 0.3))
  }
}


## QC 12 plots...
length(idx)

depth_idx <- which(level >= 0 & level <= 100)

plot(NA, NA,
     xlim = range(temp[depth_idx, idx], na.rm = TRUE),
     ylim = c(100, 0),
     xlab = "Temperature",
     ylab = "Depth / pressure level",
     main = "Prince-5 temperature profiles, 0-100 m")

n_plotted <- 0

cols <- rainbow(length(idx))

for (k in seq_along(idx)) {
  
  i <- idx[k]
  valid <- !is.na(temp[depth_idx, i])
  
  if (any(valid)) {
    
    lines(temp[depth_idx, i][valid],
          level[depth_idx][valid],
          col = cols[k],
          lwd = 2)
    
    n_plotted <- n_plotted + 1
    print(paste("Plotted cast:", i))
  }
}

print(paste("Total plotted:", n_plotted))

# how many unique casts
depth_idx <- which(level >= 0 & level <= 100)

# Extract Prince-5 temp matrix for 0-100 m
temp_p5_0_100 <- temp[depth_idx, idx]

# Transpose so each row is one cast/profile
profiles <- t(temp_p5_0_100)

# Count unique profiles
nrow(unique(profiles))

# 24 casts but only 12 unique

#plot with better visual
depth_idx <- which(level >= 0 & level <= 100)

plot(NA, NA,
     xlim = range(temp[depth_idx, idx], na.rm = TRUE),
     ylim = c(100, 0),
     xlab = "Temperature",
     ylab = "Depth / pressure level",
     main = "Prince-5 temperature profiles, 0-100 m")

cols <- rainbow(length(idx))

for (k in seq_along(idx)) {
  
  i <- idx[k]
  valid <- !is.na(temp[depth_idx, i])
  
  if (any(valid)) {
    
    x <- temp[depth_idx, i][valid]
    y <- level[depth_idx][valid]
    
    lines(x, y, col = cols[k], lwd = 2)
    
    # mark deepest plotted point
    points(tail(x, 1), tail(y, 1),
           col = cols[k],
           pch = 16,
           cex = 1.2)
  }
}



# when were these casts taken?
time_raw <- ncvar_get(nc, "time")
nc$dim$time$units
origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
time_converted <- origin + time_raw
time_p5 <- time_converted[idx]
time_p5

table(time_p5)
