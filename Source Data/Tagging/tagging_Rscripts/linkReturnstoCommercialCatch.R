

# Match returns to commercial catch for GPS location

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf")

loadfunctions( "acousticHerring")

# Load all data needed:
          # Load commercial data - this data frame is created from an output of the herring database
            catches = read.csv("commercialcatches.csv")
            
          # Load Grounds legend -  dataframe converts numbers to text so we can actually understand the ground
            grounds = read.csv("commercialcatchGrounds.csv")
            
          # Load Tag Return Data  
            data = read.csv("TagReturnsMaster.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
                  
              
                # there are 343 tag returns
    
  
# Format commercial catch data
      
    # catches
      catches$LAND_DATE = as.Date(catches$LAND_DATE, "%Y-%m-%d", origin = "1960-10-01")

    # Combine grounds with catches
        ccatches = merge(catches, grounds)
        
      # Need to categorize all tag returns according the locations listed in the commercial catch.
      # match tag returns to catches using vessel and date
        data$DATE = as.Date(data$DATE, "%Y-%m-%d", origin = "1960-10-01")
        data$BOAT = as.factor(data$BOAT)
      
          data$GearType = as.factor(data$GearType)
          data$catchAREA = as.factor(data$catchAREA)
          data$Company = as.factor(data$Company)
          
              # Only select purse seine catches to match
                  psReturns=data[which(data$GearType == "Purse Seine"), ]
                  psReturns$id = paste(psReturns$DATE, psReturns$BOAT)
                  head(psReturns)
                  dim(psReturns)
                    # 299 tag returns that come from purse seine
                  
                          psReturns$returnNo = 1:299
                              
                  
      # Create the same id for the commercial catches
        ccatches$id = paste(ccatches$LAND_DATE, ccatches$Vessel.name)
      
      # should include catches a day before and after with the same boat as the return has recorded

            psReturns$extraDate = psReturns$DATE + days(1)
            psReturns$minusDate = psReturns$DATE - days(1)
            
            head(psReturns)
            dim(psReturns)   
             length(unique(psReturns$id))
                  # 190 unique events where tags were captured by purse seine.
            
             # need to include the variable returnNo

            
            
            # Instead of adding another variable, I want to add the id's as rows 
            id1 = paste(psReturns$extraDate, psReturns$BOAT)
            id2 = paste(psReturns$minusDate, psReturns$BOAT)
            
             
            df1 <- data.frame(matrix(unlist(id1), nrow=length(id1), byrow=T))
              df1$returnNo = 1:299
                head(df1)
            
            df2 <- data.frame(matrix(unlist(id2), nrow=length(id2), byrow=T))
              df2$returnNo = 1:299
            
            df3 <- data.frame(matrix(unlist(psReturns$id), nrow=length(psReturns$id), byrow=T))
              df3$returnNo = 1:299
            
            colnames(df1) = c("id", "returnNo")
            colnames(df2) = c("id", "returnNo")
            colnames(df3) = c("id", "returnNo")
            
            
            df_ = rbind(df1, df2,df3)
              head(df_)
                dim(df_)
                  summary(df_)            
                
                  
                 
                  # use the day of, before and after:
                      listofIDS = df_$id
                        length(listofIDS)
      

# this step is removing those extra days...
                        dim(ccatches)
                        head(ccatches)                  
      
                        potentialCatchLocations = ccatches[which((ccatches$id %in% listofIDS)), ]
                              dim(potentialCatchLocations)
                                head(potentialCatchLocations)
                              
                                
                                # add the returnNo
                                head(df_)
                                write.table(df_, file= "df_.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

                                # df_ has all the alternate dates
                                total <- merge(potentialCatchLocations,df_,by="id")  
                                write.table(total, file= "total.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                                
                                # this df also contains all the alternate dates...
                                            dim(total)
                                      
                        #this is the step where things are messing up:
                                    # what do we need from psReturns?
                                            head(psReturns)
                                                # catchArea and returnNo
                                            
                                                    smaller = data.frame(psReturns$returnNo, psReturns$catchAREA, psReturns$DATE, psReturns$TAG_NUMBER)  
                                                    colnames(smaller) = c("returnNo", "catchAREA", "DATE", "TAG_number")
                                                    head(smaller)
           
                                                        catchesAroundReturns = merge(total, smaller, by = "returnNo")

                                            
                                              dim(catchesAroundReturns)  
                                              head(catchesAroundReturns)
      
      
      # remove the variables Lat1, Lon1 and Lon2
      catchesAroundReturns$Lat1=NULL
      catchesAroundReturns$Lon1=NULL
      catchesAroundReturns$Lon2=NULL
      
      str(catchesAroundReturns)
      catchesAroundReturns$TAG_number = as.numeric(catchesAroundReturns$TAG_number)
      
      catchesAroundReturns <-catchesAroundReturns[order(catchesAroundReturns$DATE, catchesAroundReturns$returnNo),]
      
      head(catchesAroundReturns)
      
    # this file does not have the extra dates...
      write.table(catchesAroundReturns, file= "catchesAroundReturns.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
          # this is good - what we want..continue
            
      
     
      
      
                
                
                
                
## This is now the QC where we check if Name and catchArea match up using a list of alternate names:  
                
                

# Need to load in the df with the fishing grounds and alternate names
# Here is where the alternate names for each fishing ground are listed:
                  
                  # Grand Manan
                  AcceptableNamesGM = c("Grand Manan", "Prong", "White Head", "White Head Grand Manan")
                  
                  #Long Island
                  AcceptableNamesLI = c("Long Island", "Sandy Cove NS", "North West Ledge", "McDormant Patch")

                  # Trinity
                  AcceptableNamesT = c("Trinity","McDormant Patch")
                
                  # Gannet Dry Ledge
                  AcceptableNamesGDL = c("Gannet Dry Ledge", "NW of German Bank", "German Bank", "Tear Drop")
                  
                  # Seal Island
                  AcceptableNamesSI = c("Seal Island", "German Bank")
                  
                  # German Bank
                  AcceptableNamesGB = c("German Bank", "Tear Drop", "SpawnTow", "Seal Island", "Gully 4Xq")
                  
                  # NB Coastal
                  AcceptableNamesNB = c("NB Coastal", "Wolfs Bank")
                  
                  # Offshore Banks
                  AcceptableNamesOB = c("Offshore Banks", "the Patch", "The Patch", "Patch")
                  
                  # Grand Manan Banks
                  AcceptableNamesGMB = c("Grand Manan Banks", "Prong", "Southwest Bank", "Northeast Bank")
                  
                  
                  head(catchesAroundReturns)
                  str(catchesAroundReturns)
                  catchesAroundReturns$catchAREA = as.character(catchesAroundReturns$catchAREA)
                  catchesAroundReturns$Name = as.character(catchesAroundReturns$Name)
                  
                  
# Subset the dataframe catchesAroundReturns for each unique fishing ground (DFO)
            
                  # NB Coastal
                  areaNB=catchesAroundReturns[which(catchesAroundReturns$Name == "NB Coastal"), ]
                    AreaNB = areaNB$catchAREA 
                  
                  # Grand Manan  
                  areaGM=catchesAroundReturns[which(catchesAroundReturns$Name == "Grand Manan"), ]
                    AreaGM = areaGM$catchAREA
                  
                  # Grand Manan Banks  
                  areaGMB=catchesAroundReturns[which(catchesAroundReturns$Name == "Grand Manan Banks"), ]
                    AreaGMB = areaGMB$catchAREA
                  
                  # Long Island  
                  areaLI=catchesAroundReturns[which(catchesAroundReturns$Name == "Long Island"), ]
                    AreaLI = areaLI$catchAREA
                  
                  # German Bank
                  areaGB=catchesAroundReturns[which(catchesAroundReturns$Name == "German Bank"), ]
                    AreaGB = areaGB$catchAREA
                  
                  # Seal Island
                  areaSI=catchesAroundReturns[which(catchesAroundReturns$Name == "Seal Island"), ]
                    AreaSI = areaSI$catchAREA
                  
                  # Scots Bay  
                  areaSB=catchesAroundReturns[which(catchesAroundReturns$Name == "Scots Bay"), ]
                    AreaSB = areaSB$catchAREA
                  
                  # Trinity  
                  areaT=catchesAroundReturns[which(catchesAroundReturns$Name == "Trinity"), ]
                    AreaT = areaT$catchAREA
                  
                  # Gannet Dry Ledge  
                  areaGDL=catchesAroundReturns[which(catchesAroundReturns$Name == "Gannet Dry Ledge"), ]
                    AreaGDL = areaGDL$catchAREA  
                    
                  # Areas not current included in results, may need to include in the future:
                        # areaBB=catchesAroundReturns[which(catchesAroundReturns$Name == "Browns Bank"), ]
                        # areaSWG=catchesAroundReturns[which(catchesAroundReturns$Name == "SW Grounds"), ]
                        # areaYB=catchesAroundReturns[which(catchesAroundReturns$Name == "Yankee Bank"), ]
                        # areaL=catchesAroundReturns[which(catchesAroundReturns$Name == "Lurcher"), ]
                  
                  names(AcceptableNamesNB) = AcceptableNamesNB 
                  names(AcceptableNamesGMB) = AcceptableNamesGMB 
                  names(AcceptableNamesGM) = AcceptableNamesGM 
                  names(AcceptableNamesGB) = AcceptableNamesGB 
                  names(AcceptableNamesT) = AcceptableNamesT 
                  names(AcceptableNamesSI) = AcceptableNamesSI 
                  names(AcceptableNamesGDL) = AcceptableNamesGDL 

                  AcceptableNamesNB[AreaNB]
                  AcceptableNamesGMB[AreaGMB]
                  AcceptableNamesGM[AreaGM]
                  AcceptableNamesGB[AreaGB]
                  AcceptableNamesT[AreaT]
                  AcceptableNamesSI[AreaSI]
                  AcceptableNamesGDL[AreaGDL]
                  

          # Unacceptable Names, first obtain all observations that contain an error, then combine into a df which can be exported and double checked:

                  

                  
                  
                  
                  
                  
# Continue here friday!!!                  
                  
                  
                  
                                    
                  AreaNB[which(is.na(AcceptableNamesNB[AreaNB]))]
                    # Good

                  AreaGMB[which(is.na(AcceptableNamesGMB[AreaGMB]))]
                    # one  "White Head Grand Manan"
                    # Now subset this observations from catchesAroundReturns:
                        areaGMB_error = areaGMB[which(areaGMB$catchAREA == "White Head Grand Manan"), ]

                  AreaGM[which(is.na(AcceptableNamesGM[AreaGM]))]
                    # Good
                  
                  AreaGB[which(is.na(AcceptableNamesGB[AreaGB]))]
                  list = AreaGB[which(is.na(AcceptableNamesGB[AreaGB]))]
                    me = unique(list)
                  # several: Undetermined, Scots Bay, Northeast Bank
                        areaGB_error = areaGB[which((areaGB$catchAREA %in% me)), ]
                
                  AreaT[which(is.na(AcceptableNamesT[AreaT]))]
                  # Sandy Cove NS
                    areaT_error = areaT[which(areaT$catchAREA == "Sandy Cove NS"), ]
                  
                  AreaSI[which(is.na(AcceptableNamesSI[AreaSI]))]
                    # Good
                  
                  AreaGDL[which(is.na(AcceptableNamesGDL[AreaGDL]))]
                    # Good                    
                  
                  # NB Coastal, Grand Manan, Seal Island and Gannet Dry Ledge have no mis-matches
                  # There is at least one mis-match with Grand Manan Banks, German Bank and Trinity 
                  
                  # Export the .csv - mismatchFishingGrounds
                    mismatch = rbind(areaGMB_error, areaGB_error, areaT_error)
                    
                    write.table(mismatch, file= "mismatchFishingGrounds.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    
                    
                  
                  
                  
                  
                  
                  
                  
# To make the QC easier - for instances where there has been only one catch for the date and vessel provided then this GPS coordinate can be automatically assigned
                  
                  head(catchesAroundReturns)
                  catchesAroundReturns$no  = 1:1
                  
                  # need to select for tag numbers where there is only one, i.e unique, or count
                  
                  summary(catchesAroundReturns$TAG_NUMBER)
                  
                  solo <-aggregate(no~TAG_NUMBER, catchesAroundReturns, FUN=sum)
                  oneChoice = solo[which(solo$no < 2), ]
                  tags = oneChoice$TAG_NUMBER
                  
                  head(catchesAroundReturns)
                  # Now use the list created (tags) to select those values in the catchesAroundReturns df
                  
                  jennaGPS = catchesAroundReturns[which((catchesAroundReturns$TAG_NUMBER %in% tags)), ]
                  
                  # Remove some of the unneeded columns now - this DF should be simple
                  head(jennaGPS)    
                  jennaGPS$extraDate = NULL
                  jennaGPS$minusDate = NULL
                  jennaGPS$id1 = NULL
                  jennaGPS$id2 = NULL
                  jennaGPS$no = NULL
                  jennaGPS$BOAT = NULL
                  jennaGPS$DATE = NULL
                  
                  # Now these GPS locations can be compared with those that were generated by DFO
                  dim(jennaGPS)
                  # currently I have 344 tag returns, and 193 GPS locations can be automatically generated.
                  sum(344-193)
                  
                  # This leaves 151 that have to be sorted through with more than one possible location possible.
                  # All I really care about however are those returns where there are more than one fishing area possible, for example if withint the three day window there was fishing in Scots Bay, then German Bank. In those cases I will have to do some investigation. What I can do next is look at all the tags which have different options for GPS location, i.e. >1, but they are all in the same fishing area. For these I can just choose one location as it doesn't really make a difference for analyses.
                  
                  
                  
                  
                  
                  
                  
                  area3$statement = AcceptableNamesGMB[area3$catchAREA]
                  head(area3$statement)
                  new_DF <- GMBanks[rowSums(is.na(GMBanks)) > 0,]
                  
                  area3[which(is.na(AcceptableNamesGMB[area3$catchAREA]))]
                  
                  ## Acceptable names
                  FishingGround[which(!is.na(AcceptableNames[FishingGround]))]
                  [1] "Lobster Bay" "Deep Cove"
                  
                  
                  t=boxes[which(boxes$Box == "GermanBank"), ]
                  