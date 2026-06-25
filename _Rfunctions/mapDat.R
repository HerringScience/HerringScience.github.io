mapDat = function(x) {
  
  
  # Add variable id
  x$echo_id = c(1:nrow(x))
  
  # Create unique trans name
  x$Survey_date = substring(x$EV_filename, 4,13)
  x$Region_name = as.character(paste(x$Survey_date, x$Region_name, sep = ""))
  x$Dist_T = (x$Dist_E - x$Dist_S)/1000
  
  x$Process_ID = NULL
  x$Ping_S = NULL
  x$Ping_E = NULL
  x$Region_class = NULL
  x$Interval = NULL
  x$Layer = NULL
  x$Dist_S = NULL
  x$Dist_E = NULL
  x$Date_S = NULL
  x$Time_S = NULL
  x$Date_E = NULL
  x$Time_E = NULL
  x$Program_version = NULL
  x$Processing_date = NULL
  x$Region_notes = NULL
  x$Processing_time = NULL
  x$PRC_NASC = NULL

  x$Y = x$Lat_S
  x$X = x$Lon_S
  x$Yend = x$Lat_E
  x$Xend = x$Lon_E
  
  x$Lat_S = NULL
  x$Lon_S = NULL
  x$Lat_E = NULL
  x$Lon_E = NULL
  
  x$Vessel = substring(x$EV_filename, 1,2)  
  
  # Year
  x$Year = substring(x$EV_filename, 10,13)  
  
  # Density
  x$TargetStrength = -35.51
  x$calc = 10^((x$TargetStrength-(x$PRC_ABC))/10)
  x$density = (x$calc) * x$Dist_T
  
  # transect
  x$Transect_No = substring(x$Region_name, 14,16)
  
  return(x)
}