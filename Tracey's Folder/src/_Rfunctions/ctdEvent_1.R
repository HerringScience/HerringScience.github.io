

# Function to format the CTD event data


ctdEvent_1 = function(x) {
  
  x = events
  
  # Format events
  x$Date =as.Date(x$Date, "%Y-%m-%d")
  x$ground =as.factor(x$ground)
  x$id =as.factor(x$id)
  x$Year = year(x$Date)
  x$Year = as.factor(x$Year)
  x$julianDay = yday(x$Date)
  x$week = week(x$Date)
  x$week=as.factor(x$week)
  
  # create device variable
  x$device = substring(as.character(x$id), 1,9)
  
  return(x)
}