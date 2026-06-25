

tagEve = function(rel) {
  
  
  
  rel$no = (1:1)
  
  # Creates a unique id for each tagging event based on the lat/lon and date
  rel$id = paste(rel$X, rel$Y, rel$RELEASE_DATE, sep=" ")

  head(rel)
  # Unique locations
  # need to use GPS location and date, as sometimes the GPS location is the same between dates
  # Create the df x so that the variable set can be created. Each tag within the same tagging event has the same set. Create set within x then merge with rel to create release
  
    x = unique(rel[c("X", "Y", "RELEASE_DATE")])
    r = dim(x)
    r = as.data.frame(r)
    v  = r[1,1]
    
    x$set = 1:v
    x$id = paste(x$X, x$Y, x$RELEASE_DATE, sep=" ")
    
    # create a variable that contains all three variables; X,Y and date to form a unique character to link the two data frames.
    
    release = merge(rel, x, by = "id")
    release$X = release$X.x 
    release$Y = release$Y.x 
    release$X.x = NULL
    release$X.y = NULL
    release$Y.x = NULL
    release$Y.y = NULL
    release$RELEASE_DATE.x = NULL
    release$RELEASE_DATE = release$RELEASE_DATE.y  
    release$RELEASE_DATE.y = NULL
    
    head(release)
    
    # Now the raw data can be summarized - create summary which is a list with the number of tags per event
    
    summary = with(release, tapply(no, list(set), sum))
    summary = as.data.frame(summary)
    summary$set = 1:v
    
    head(summary)
    
    # again check number of tags
    x = as.data.frame(unique(release[c("X", "Y", "set", "RELEASE_DATE", "RELEASE_VESSEL", "Tagger", "CTD.File")]))
    
    head(x)
    
    relINFO = merge(summary, x, by = "set" )
    relINFO$no_tags = relINFO$summary
    relINFO$summary = NULL
    
    
    str(relINFO)
    
    # Format Data
        
        relINFO$RELEASE_DATE =as.Date(relINFO$RELEASE_DATE, "%Y-%m-%d")
        relINFO$Year = as.numeric(format(relINFO$RELEASE_DATE, "%Y"))
        relINFO$Year  = as.factor(relINFO$Year)
        relINFO$no_tags = as.numeric(relINFO$no_tags)
        relINFO$set = as.factor(relINFO$set)  
        relINFO$month = month(relINFO$RELEASE_DATE)
        relINFO$month =as.factor(relINFO$month)
        
        relINFO$CTD.File = as.factor(relINFO$CTD.File)    
        
        
    # add julian day, week
    relINFO$Julian = format(relINFO$RELEASE_DATE, "%j")                   
    relINFO$Julian = as.factor(relINFO$Julian)
    relINFO$week = week(relINFO$RELEASE_DATE)

    head(relINFO)
    
    
    return (relINFO)
  

  
  
  
}