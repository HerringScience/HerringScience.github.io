
### Script to generate 'days at large' and identifier that is unique to the tagging events and all associated tag returns called 'no'. Further in the script it is used to generate distance

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf", "data.table", "geosphere", "pracma")

loadfunctions( "acousticHerring")


# Load data and create 'no' and 'daysAtLarge'

# Return Data
    load("returnData.RData")
          head(returnData)
                ids = returnData$TAG_NUMBER
                    # This allow us to differentiate between return and release info
                    

# Release Info
  rel = read.csv("TaggingEvents.csv")
    head(rel) 
      # Select those rows which have a tag number that matches to the returns
        events  = rel[which((rel$TAG_NUMBER %in% ids)), ] 
         

# Make sure both df's have the same dimensions
    dim(events)
      dim(returnData)
        head(returnData)
          head(events)    
            events$Tagger = NULL
        
            # Need to use GPS postions, as there can be multiple vessel/date tagging events
              events$id =paste(events$RELEASE_DATE, events$RELEASE_VESSEL, events$X, events$Y)
              dim(events)  
              
                l = unique(events$id)    
                    l = as.data.frame(l)
                        head(l)
                          # Creat the 'no'variable
                            colnames(l) = "id"
                              dim(l)
                                l$no = 1:90
                                  head(l)
                        
                                    eventss = merge(l, events)
                                      head(eventss)
                                                        
# df with both returns and release info
    all = merge(returnData, eventss, by = "TAG_NUMBER")
    str(all)
    all$ReturnDate = as.Date(all$ReturnDate, "%Y-%m-%d", origin = "1960-10-01")
    all$RELEASE_DATE = as.Date(all$RELEASE_DATE, "%Y-%m-%d", origin = "1960-10-01")

        # Add days at large
    
            all$daysAtLarge  = all$ReturnDate - all$RELEASE_DATE
                    all$id.x = NULL
                    all$id.y = NULL
                    all$returnNo = NULL
                
                          save(all, file="all.RData", compress=T)
                
                            # write.table(all, file= "all.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                

                
 # Calculate distance between tagged location and recapture
  # These tag numbers were determined by plotting all tag returns and manually recording which paths cross land and using 'no' and 'daysAtLarge' to identify                         
                
                ids = c(419770, 420714, 421170, 421619, 418251, 440150, 440261, 431604, 448029, 448030, 448505, 448906, 448912, 453163, 442788, 442864, 442927, 449347, 449786, 450446, 450689, 445793, 445880, 445969, 432950, 441773, 441801, 443959, 44358, 44374, 34171, 452321, 452653, 34493, 460183, 456401, 455887, 455889, 460131, 456509, 459313, 459364, 459394, 416466, 459473, 455068, 455333, 416636, 204314, 455602, 455750)
             
                # df that I need to manually calculate distance as there is land in the way
                  manual = subset(all, (all$TAG_NUMBER %in% ids))
                    head(manual)
                      dim(manual)
                        write.table(manual, file= "manual.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                      
                        loadfunctions( "acousticHerring")
                
                # this is the dataframe where I need to find a function to calculate the distances.
                  auto = anti_join(all, manual)
                    dim(auto)
                      str(auto)
                
                      # I can go through one by one..
                      x = 4
                      
                      m = auto[x,]
                      
                      n = distm(c(m$X, m$Y), c(m$ReturnX, m$ReturnY), fun = distHaversine)
                            n = n/1000
                              auto[x,18] = n     
                                  head(auto)                      
                
                                  
# For loop to generate distances 
                                  
                                  ids = auto$TAG_NUMBER
                                  r = data.frame( cbind( id=ids, distance=NA), stringsAsFactors = FALSE )
                                  
                                  for (i in 1:nrow(r)){
                                    test = which(auto$TAG_NUMBER==r[i,"id"])
                                        m = auto[test, ]
                                    
                                             n = distm(c(m$X, m$Y), c(m$ReturnX, m$ReturnY), fun = distHaversine)
                                                  n = n/1000
                                                        r$distance[i]=n
                                            
                                  }
                                  
                                  colnames(r) = c("TAG_NUMBER","Distance")
                                      head(r)                                  
# Now include distances back into all 
                                  
  head(auto)
  auto$Distance = NULL 
  
    autoDist  = merge(auto, r, by = "TAG_NUMBER")
        head(autoDist)                                  
          dim(autoDist)                            
                
                
