

# Blaxter–Alderdice incubation model
# T = temperature in °C
# Returns predicted days to hatch

herring_incubation <- function(T, DD = 160, T0 = 1) {
  if (any(T <= T0)) {
    stop("Temperature must be greater than developmental zero (T0).")
  }
  days <- DD / (T - T0)
  return(days)
}

CTDd = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/CTD 30m.csv"))

head(CTDd)

CTDd$IncubationTinD = herring_incubation(CTDd$Temperature)


### Incorporate the NA's

# df must contain:
#   ground  = spawning ground ID (factor or character)
#   date    = POSIXct or Date
#   temp    = temperature in °C (may contain NA)

# Blaxter–Alderdice incubation model:
# Days = DD / (T - T0)
# Defaults: DD = 160, T0 = 1°C

incubation_with_fallback <- function(date, ground, temp, DD = 160, T0 = 1) {
  
  # Ensure date is Date class
  date <- as.Date(date)
  
  n <- length(temp)
  incubation_days <- numeric(n)
  
  for (i in seq_len(n)) {
    
    this_ground <- ground[i]
    this_date   <- date[i]
    this_temp   <- temp[i]
    
    # If temperature is available, use it
    if (!is.na(this_temp)) {
      incubation_days[i] <- DD / (this_temp - T0)
      next
    }
    
    # If temperature is NA, find nearest date on same ground
    same_ground <- which(ground == this_ground & !is.na(temp))
    
    if (length(same_ground) == 0) {
      incubation_days[i] <- NA
      next
    }
    
    # Find closest date
    idx <- same_ground[which.min(abs(date[same_ground] - this_date))]
    fallback_temp <- temp[idx]
    
    incubation_days[i] <- DD / (fallback_temp - T0)
  }
  
  return(incubation_days)
}




# now need to link this CTD data with Larval
# the average temperature is already within the Larval Sum data. Using just the deepest temperature would be better but with the short time available for now I will just use the average cast temperature. When there is no cast, the closest temperature in time for that ground will be used.

          ## Load larval data:
          # The larval data
Larval = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Full Larval.csv"))

          head(Larval)
          
          #original was from Main Data, Jan 2025
          LarvalSum = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/LarvalSum Jan 2025.csv"))
          
          head(LarvalSum)
          
          LarvalSum$Year <- as.factor(LarvalSum$Year)
          Larval$Date <- lubridate::ymd(Larval$Date)
          Larval <- dplyr::arrange(Larval, Date)
          Larval$Year <- as.factor(Larval$Year)
          Larval$category <- as.factor(Larval$category)
          Larval$Survey.No <- as.factor(Larval$Survey.No)
          Larval$MonthDay <- format(Larval$Date, "%m-%d")
          
          #Changed to X and Y to fit in better with compendium code. These are the tow start and finish coordinates.
          
          names(Larval)[names(Larval) =="Lon1"] <- "X"
          names(Larval)[names(Larval) =="Lat1"] <- "Y"
          names(Larval)[names(Larval) =="Lon2"] <- "Xend"
          names(Larval)[names(Larval) =="Lat2"] <- "Yend"
          
          #Seal Island Larval
          LarvalSI = filter(Larval, Ground == "SI")
          LarvalSI = merge(LarvalSI, LarvalSum[,c("id", "TowReplicate", "TowID")], by = "id")
          
          Larval = merge(Larval, LarvalSum[,c("id", "TowReplicate", "TowID")], by = "id")
          
          head(Larval)

             
          
          
          
          
          
          
          
          Larval$incubationTinD = incubation_with_fallback(Larval$Date, Larval$Ground, Larval$CTDAvgTemp)
          
         
             Larval$id
             dim(Larval)
             
             newL = data.frame(Larval$id, Larval$incubationTinD)
              
             colnames(newL) = c("id", "incubationTinD")
             
             dim(newL)
             
             #save only unique values
             
             newL <- distinct(newL)
             dim(newL)
             
             save(newL, file = "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/newL.RData")
             
             
             
             
             # Plot 
          
          
             temps <- seq(2, 15, by = 0.1)
             curve_days <- 160 / (temps - 1)
             
             plot(temps, curve_days, type = "l", lwd = 2,
                  xlab = "Temperature (°C)",
                  ylab = "Days to Hatch",
                  main = "Blaxter–Alderdice Curve with Observations")
             
             points(Larval$CTDAvgTemp, Larval$incubationTinD,
                    pch = 19, col = "red")      
             
             
             # save the updated Larval dataframe.
            
             save(Larval, file = "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval.RData")
