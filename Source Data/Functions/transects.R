transects = function(x, TS38, TS50) {
  
  # Add variable id
  #x$echo_id = c(1:nrow(x))
  
  # Create unique trans name
  x$RegionName = x$Region_name
  
  x$Survey_date = substring(x$EV_filename, 4,13)
  x$Region_name = as.character(paste(x$Survey_date, x$Region_name, sep = ""))

  # Change to numeric
  x$Process_ID = as.numeric(x$Process_ID)
  x$Ping_S = as.numeric(x$Ping_S)
  x$Ping_E = as.numeric(x$Ping_E)
  
  # Change to time
  # First Start Date/Time
  
      # Changed format with Echoview v11
  t = parse_date_time(x$Time_S, orders="HMS", tz = "America/Moncton")
#TL added this as the hours kept getting dropped.  
  t=format(t, "%H:%M:%S")
  y = parse_date_time(x$Date_S, orders= "ymd", tz = "America/Moncton")
  
  z = paste(y, t)
#this removes the hours.
  # w = substring(as.character (t), 12,50)
  # z = paste(y, w)
  
  z = as.POSIXct(z)
  x$Date_Time_S  = z
  x$Date_S = NULL
  x$Time_S = NULL
  
  # Now End Date/Time
  t = parse_date_time(x$Time_E, orders="HMS", tz = "America/Moncton")
#TL added this as the hours kept getting dropped.
  t=format(t, "%H:%M:%S")
  y = parse_date_time(x$Date_E, orders= "ymd", tz = "America/Moncton")
  
  z= paste(y,t)
  
#This was removing the hours and just leaving the minutes and seconds.  
  # w  = substring(as.character (t), 12,50)
  # z = paste(y, w)
  
  
  z = as.POSIXct(z)
  x$Date_Time_E  = z
  x$Date_E = NULL
  x$Time_E = NULL
  
  # Year
  x$Year = substring(x$EV_filename, 10,13)  
  
  # Transect_No
  x$Transect_No = substring(x$Region_name, 14,16)
  
  # Create a total distance variable (Dist_T)
  x$Dist_T = (x$Dist_E - x$Dist_S)/1000

  x$Y = x$Lat_S
  x$Yend = x$Lat_E
  x$X = x$Lon_S
  x$Xend = x$Lon_E
  
  x$Lat_S = NULL
  x$Lat_E = NULL
  x$Lon_S = NULL
  x$Lon_E = NULL
  
  x$Vessel = substring(x$EV_filename, 1,2)  
  x$linearBackscatter = 10^(x$Area_Backscatter_Strength/10)

  x$Editor = x$Editor
  
  
  # Create unique transect and boat
  
  
  # Columns for Biomass Calcs
  # If there are ever any other frequency used other than 38 and 50, this statement will have to be edited
  # Standard TS (first number is for 50 kHz, second is for 38 kHz)
     #x$TS<-ifelse((x$Frequency>38), -35.609, -35.5)
    # Adjust for TS adjustment for each survey
        # Aug 21, 2017 
              #x$TS<-ifelse((x$Frequency>38), -34.823, -34.716)
        # Sep 8, 2017
            #x$TS<-ifelse((x$Frequency>38), -34.737, -34.630)
        # Sep 17, 2017
             #x$TS<-ifelse((x$Frequency>38), -34.694, -34.586)
        # Jul 29, 2017
            # x$TS<-ifelse((x$Frequency>38), -35.158, -35.051)
        
  # For Scots Bay with no Tasha Marie, or those GB without TM
    #x$TS = TS

    x$TS<-ifelse((x$Frequency>38), TS50, TS38)  
  
  
  
  
  x$biomass_density = 10^((x$Area_Backscatter_Strength-(x$TS))/10)
  
  return(x)
}
