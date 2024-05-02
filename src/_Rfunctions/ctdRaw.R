

# Function to format CTD raw data

ctdRaw = function(x) {
  
  
  # Format data
  x$Date =as.Date(x$Date, "%Y-%m-%d")
  x$ground =as.factor(x$ground)
  x$id =as.factor(x$id)
  x$Year = year(x$Date)
  x$Year = as.factor(x$Year)
  x$julianDay = yday(x$Date)
  x$week = week(x$Date)
  x$week=as.factor(x$week)
  
  # create the column called "device" as we have multiple CTDs starting in 2019
  x$device = substring(as.character(x$id), 1,9)
  
  return(x)
  
}