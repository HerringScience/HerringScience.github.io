

# Format exports of detail fishing catches from Derek so I can map and explore data

detCatches = function(x) {
  
  
  # Convert Latitude
  x$keep1 = substr(x$LAT, 1, 2)
  x$keep2 = substr(x$LAT, 3, 7)
  
  x$LAT1 = paste(x$keep1, x$keep2)
  x$LAT = measurements::conv_unit(x$LAT1, from = 'deg_dec_min', to = 'dec_deg')
  
  # Convert Longitude
  x$keep3 = substr(x$LON, 1, 2)
  x$keep4 = substr(x$LON, 3, 7)
  
  x$LON1 = paste(x$keep3, x$keep4)
  x$LON = measurements::conv_unit(x$LON1, from = 'deg_dec_min', to = 'dec_deg')
  
      # Convert Lat and Lon to numeric, and add a minus to the lon data
  
            x$LON = as.numeric(x$LON)
            x$LON = x$LON * -1 
            x$LAT = as.numeric(x$LAT)

            # Remove columns that aren't needed
              x$keep1 = NULL
              x$keep2 = NULL
              x$keep3 = NULL
              x$keep4 = NULL
              
              x$LAT1 = NULL
              x$LON1 = NULL    
              
              
              # Formating other variables
                        
                  # Date
                   x$Date = as.Date(x$SDATE, "%d-%b-%y")
                   x$month = month(x$Date)
                   
                   x$SDATE=NULL
                    x$SEX = as.factor(x$SEX)
                    x$GCODE = as.factor(x$GCODE)                
                    x$MAT = as.factor(x$MAT)
                    x$AGE = as.factor(x$AGE)                    
                    x$LEN = as.numeric(x$LEN)
                    
                    return(x)
}
                    