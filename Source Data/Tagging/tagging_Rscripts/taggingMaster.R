


RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel")

loadfunctions( "acousticHerring")

# Produce all tag return DF's

# First load the release data

rel = read.csv("TaggingEvents_.csv")
rel = read.csv("TaggingEvents2021.csv")


# Summarize into events
  relINFO = tagEve(rel = rel )
  head(relINFO)
  dim(relINFO)
  

  unique(relINFO$RELEASE_DATE)
  
  save(relINFO, file="relINFO.RData", compress=T)
  
  # To export relINFO - with all tagging events  
  write.table(relINFO, file= "relINFO.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
  
  
  
  
  ### Older: this script is mainly for relINFO....
  
  
  
  
  
  # Total number of tags          
  sum(relINFO$no_tags)
  
  # Create a function that will export all tables and figures needed for report in terms of tagging events
  # Need to have base mapdata ran
  
  # Mapping Data
  # include eastern boxes
  # CP <- as(extent(-67.6, -61.8, 43, 45.8), "SpatialPolygons")
  # include eastern boxes:
  # boxes = read.csv("catchBoxes.csv")
  
  # need to find the coastal NB events
            # coastal=x[which(x$Y > 44.8), ]
            # coastal_ = coastal[which(coastal$X < -66), ]
            
            
# Function that runs all the tables and figures for the report for tagging events
  eventSummaries(x = relINFO)
  
  x = relINFO
  dim(rel)

  # Figures
  # Mapping
  
  can<-getData('GADM', country="CAN", level=1) # provinces
  NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
  CP <- as(extent(-67.6, -61.8, 43, 45.8), "SpatialPolygons")
  proj4string(CP) <- CRS(proj4string(NBNS))
  out <- gIntersection(NBNS, CP, byid=TRUE)
  
  boxes = read.csv("catchBoxes_.csv")
  
  
  
  # Tag Returns:
  
  # Tag release DF look at tag returns from the same day
  # load the data first
  
  relDat = read.csv("releasesDate.csv")
  retAdd = read.csv("returnsAdd.csv")
  
  # these two DF's have different GPS coordinates and dates
  # what are the columns id, group and div?
    # div is a unique number for each tagging event where there were returns. Tag returns from the same tagging event have the same div number. Returns don't have a div number.
    # id is just a unique number for each cell, could represent a return or release event
    # group is a number that matches each return to the release event, unique for each return, corresponding to a unique return
  
  r = tagRet (r1 = relDat, r2 = retAdd )
  
  # If you need r separated into release and returns;
        r1=r[which(r$type == "release"), ]
        r2=r[which(r$type == "return"), ]
  
  # Create a unique column that will be udes to combine relINFO and r1
  
            relINFO$unique = paste(relINFO$X,relINFO$Y, relINFO$RELEASE_DATE, sep=',')
            r1$unique = paste(r1$X,r1$Y,r1$date, sep=',')
            
            unique(r1$date)
            unique(relINFO$RELEASE_DATE)
            
            # Create m which gives you all the info you need regarding tagging events - you get the tag events which has returns with  
            m =merge(r1, relINFO, by = "unique")
            
                  # no longer need unique
                  m$unique = NULL
                  m$Y.x = NULL
                  m$date = NULL
                  m$X.x = NULL
                  m$Release_X = m$X.y
                  m$X.y = NULL
                  m$Release_Y = m$Y.y
                  m$Y.y = NULL
                  
                  head(m)
            # Make sure the dates are the same , i.e. date and RELEASE - did this in excel with EXACT function,       couldnt figure it out in R
            
            # Order m1 by div.
            m = m[order(m$group),]
            head(m)
            head(r2)
            
            # Clean up r2 colnames
            r2$Return_X = r2$X
            r2$X = NULL
            r2$Return_Y = r2$Y
            r2$Y = NULL
            r2$Return_Date = r2$date
            r2$date = NULL
            
            head(r2)
            head(r1)
            
            
            # The length of relINFO should be more as it contains all the tagging events, r1 is only those events for which there were returns
            
            # Combine m and r2 
            c = merge(m, r2, by = "group")
            head(c)
            
            # Clean up the data
            c$type.x = NULL
            c$type.y = NULL
            c$div.x = NULL
            c$daysAtLarge.x = NULL
            
            c$release_month = c$month.x
            c$month.x = NULL  
            
            c$daysAtLarge = c$daysAtLarge.y
            c$daysAtLarge.y = NULL
            
            c$div = c$div.y
            c$div.y = NULL
            
            c$return_month = c$month.y
            c$month.y = NULL
            
            head(c)
            
            # Order by date
            c1 = c[order(c$div),]
            head(c1)
            
            
            
## Look at returnComplete
            
            head(returnComplete)
            
            
            