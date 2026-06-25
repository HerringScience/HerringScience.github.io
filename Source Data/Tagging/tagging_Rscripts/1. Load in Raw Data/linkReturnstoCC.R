
## Links Return Data to Commercial Catch to be able to determine return location and catch amount

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf", "data.table")

loadfunctions( "acousticHerring")


# Load commercial catch data:
# Load commercial data - this data frame is created from an output of the herring database
    catches = read.csv("commercialcatches.csv")
    passive = read.csv("weirShutCatches.csv")

# Load Grounds legend -  dataframe converts numbers to text so we can actually understand the ground
    grounds = read.csv("commercialcatchGrounds.csv")
    portCodes = read.csv("portCodes.csv")  


# Format commercial catch data

# catches
    catches$LAND_DATE = as.Date(catches$LAND_DATE, "%Y-%m-%d", origin = "1960-10-01")
    passive$DATE = as.Date(passive$DATE, "%Y-%m-%d", origin = "1960-10-01")


# Combine grounds with catches
    ccatches = merge(catches, grounds)
    passiveG = merge(passive, portCodes)


# Load Tag Return Data  
    rawReturn = read.csv("rawReturn.csv")
    dim(rawReturn)

head(rawReturn)
unique(rawReturn$catchAREA)

# there are 344 tag returns


# Remove those tag returns that are mid water trawl or weir_shutoff as these catches have not been identified. 

              x=rawReturn[which(rawReturn$GearType != "Mid Water Trawl"), ]
              y=x[which(x$GearType != "Weir_ShutOff"), ]
              
              dim(y)
              
              
# We have 329 tag returns that can be linked to a catch... I think.

# Need to categorize all tag returns according the locations listed in the commercial catch.
# match tag returns to catches using vessel and date


              
              
# Purse Seine First:              

# Only select purse seine catches to match to ccatches
      psReturns=rawReturn[which(rawReturn$GearType == "Purse Seine"), ]
      dim(psReturns)
      psReturns$id = paste(psReturns$DATE, psReturns$BOAT)
      
# Remove purse seine returns that don't have landed date
# tag numbers: 419912, 422173, 426370, 428739, 431855, 437873
              ids  = c(419912, 422173, 426370, 428739, 431855, 437873, 435157)

                      psReturns_ = subset(psReturns, !(psReturns$TAG_NUMBER %in% ids))
                      dim(psReturns_)         

# There are 303 tag returns that come from purse seine with a catch identified
              psReturns_$returnNo = 1:303

# Create the same id for the commercial catches
              ccatches$id = paste(ccatches$LAND_DATE, ccatches$Vessel.name)
                
                    length(unique(psReturns_$id))

# 187 unique events where tags were captured by purse seine, some events resulted in more than one return

                              listofIDS = unique(psReturns_$id)            
                              potentialCatchLocations = ccatches[which((ccatches$id %in% listofIDS)), ]
                              dim(potentialCatchLocations)
                              
                              
                              head(potentialCatchLocations)
                              check = potentialCatchLocations[!duplicated(potentialCatchLocations[1:2]),]    
                              dim(check)                      
                              dim(potentialCatchLocations)
                              # there are alot of duplicate id's.


                    head(psReturns_)                                
                    l = duplicated(psReturns_$TAG_NUMBER)
                    length(l)                                          

                      # This data frame gives us all the tag returns linked with commercial catch but there are tag number duplicates                                                    
                      catchesAroundReturnsPS = merge(psReturns_, potentialCatchLocations, by = "id")

# Mobile      
                  mobile = catchesAroundReturnsPS
                                              
                                              t <- duplicated(mobile$TAG_NUMBER)
                                              t = as.data.frame(t)
                                              l =     mobile$TAG_NUMBER
                                              
                                              f = data.frame(t,l)
                                              
                                              # select =TRUE
                                              fefe = f[which(f$t == "TRUE"), ]
                                              o = unique(fefe)
                                              head(o)
                                              dim(o)
                                              # 81 Tag Numbers are duplicated
                                              
                                              # Remove duplciated tag numbers, choose the one with the largest catch amount          
                                              
                                              mobile = as.data.table(mobile)
                                              mobileF = mobile[mobile[, .I[which.max(Catch.t)], by=TAG_NUMBER]$V1]
                                              dim(mobileF)
                                              
                                              # Check there are no duplicates
                                              
                                              dim(mobileF)
                                              duplicated(mobileF$TAG_NUMBER)
                                              
                                              
                                              head(mobileF)                            
                                              str(mobileF)
                                              
    # formatting
                                              mobileF$Lat = NULL
                                              mobileF$Lon = NULL
                                              
                                              mobileF$Vessel.name = NULL
                                              mobileF$CFV = NULL  
                                              mobileF$Type = NULL  
                                              mobileF$Year = NULL  
                                              mobileF$LAND_DATE = NULL  
                                              mobileF$Ground = NULL  
                                              mobileF$CATCH_T = mobileF$Catch.t
                                              mobileF$Catch.t = NULL
                                              head(mobileF)
                                              head(passiveReturns)
                                              
                                              
                                              # mobile is ready. PS Returns
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              


### Next gear type

# Do the same for weir catches and shutoff:

weirs=rawReturn[which(rawReturn$GearType == "Weir"), ]
dim(weirs)

# There are 14 weir tag returns
    duplicated(weirs$TAG_NUMBER)


weirs$DATE = as.Date(weirs$DATE, "%Y-%m-%d", origin = "1960-10-01")
head(weirs)

weirs$id = paste(weirs$DATE, weirs$BOAT)
dim(weirs)
weirs$returnNo = 1:14
m = unique(weirs$id)
duplicated(weirs$id)


            # Perhaps match using only date as there are not alot of weir landings in one day
            
            
            #                            weirs_9=weirs[which(weirs$returnNo == 9), ]


head(passive)
    passive$id = paste(passive$DATE, passive$Vessel)
        
        # there is 14 tags but only 13 events for weir returns
        # There were 2 tag returns from 2019-09-09 Senator Neil 
              listofIDS = unique(weirs$id)     
              length(listofIDS)
                # 13 IDS          
        
               
              
              
        passiveW=passive[which(passive$GearType == "W"), ]
              head(passiveW)
                dim(passiveW)  
                      passive$id        
                
                  weirCatch = passiveW[which((passiveW$id %in% listofIDS)), ]
                  duplicated(weirCatch$id)
                      # We have two ids where there are duplicated catches
            
                 
                  # This step removes duplicates and selects the larger catch. 
                  weirCatch = as.data.table(weirCatch)
                  weirCatch = weirCatch[weirCatch[, .I[which.max(CATCH_T)], by=id]$V1]
                  
                     head(weirCatch)
                  
                  dim(weirs)
                  dim(weirCatch)
                  
                  
                  weirY = merge(weirs, weirCatch, by = "id")
                  
                  dim(weirY)
                  head(weirY)
                  # weirY is ready
                  
                                                        # what is missing? one from weirs that's not in weirCatch
                                                        # a = unique(weirs$id)
                                                        # b = unique(weirCatch$id)
                                                          # setdiff(a,b)
                                                                # Capelco August 30, 2019 is not in the commercial catch DB.
                                                          
                                                          
                                                            # missing two tag returns from weirCatch (i.e. return data not matching up to the cc data)
                                                              #    m = weirCatch$id
                                                               #   listofIDS
                                                        
                                                                  # setdiff(listofIDS, m)
                           
                  
                  
                  

### Next gear type - ShutOff
          
          shut=rawReturn[which(rawReturn$GearType == "ShutOff"), ]
          dim(shut)
          shut$DATE = as.Date(shut$DATE, "%Y-%m-%d", origin = "1960-10-01")
          shut$id = paste(shut$DATE, shut$BOAT)
          
          dim(shut)
          shut$returnNo = 1:5
          
          # 5 tag returns from 4 events
            # Two returns from 2018-08-23 Michael Eilenn    
                
                  listofIDS = unique(shut$id)     
                  
                        passiveS=passive[which(passive$GearType == "S"), ]
                              head(passiveS)
                              
                                  shutCatch = passiveS[which((passiveS$id %in% listofIDS)), ]
                                      dim(shutCatch)
                                          head(shutCatch)
                              
                                          
                                          setdiff(listofIDS, shutCatch$id)
                                          
                                          
                              shutUp = merge(shut, shutCatch, by = "id")
                              
                              #shutUp is ready
                              
                              
                              
                              

                              
                              


# Can combine passive gears:                          
      passiveReturns = rbind(weirY, shutUp)
      head(passiveReturns)
      
          # prep formatting
                passiveReturns$DATE.x = NULL
                passiveReturns$portCode = NULL
                passiveReturns$Lat = NULL
                passiveReturns$Lon = NULL
                passiveReturns$WEIRNO = NULL
                passiveReturns$CATCH_KG = NULL
                passiveReturns$GearType.x = NULL
                passiveReturns$Vessel = NULL
                
                passiveReturns$DATE = passiveReturns$DATE.y
                passiveReturns$GearType = passiveReturns$GearType.y
                
                passiveReturns$DATE.y = NULL
                passiveReturns$GearType.y = NULL
                
                
                head(passiveReturns)
                
                passiveReturns$DATE = as.Date(passiveReturns$DATE, "%Y-%m-%d", origin = "1960-10-01")
                str(passiveReturns$DATE)
                
                head(mobileF)        
                str(mobileF)
                  
                mobileF$DATE = as.Date(mobileF$DATE, "%Y-%m-%d", origin = "1960-10-01")
                
                
                
                
                
                
# Now combine passive and mobile gears:                

      colnames(passiveReturns)          
      colnames(mobileF)          
                
      returnData = rbind(passiveReturns, mobileF, fill = TRUE)      
      
      dim(returnData)
      303+14+5

write.table(returnData, file= "returnData.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

# save this df 

returnData$catchAREA = as.character(returnData$catchAREA)
returnData$Name = as.character(returnData$Name)

head(returnData)

colnames(returnData) = c("id", "TAG_NUMBER", "plantReturnPlace", "ReturnY", "ReturnX", "ReturnVessel", "ReturnCompany", "returnNo", "OfficialReturnName","ReturnNAFO", "CATCH_T", "ReturnDate", "ReturnGearType")



file="returnData.RData"
save(returnData, file="returnData.RData", compress=T)


