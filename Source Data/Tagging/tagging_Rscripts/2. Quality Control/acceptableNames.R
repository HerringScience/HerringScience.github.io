
## 

# Match returns to commercial catch for GPS location, and when there are multiple landings for the same day vessel, go with the landing of the largest catch weight (most likely)

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf", "data.table")

loadfunctions( "acousticHerring")
                    
# Need to load in the df with the fishing grounds and alternate names
# Here is where the alternate names for each fishing ground are listed:
                  
                 
      load("mobileF.RData")      
      unique(mobileF$catchAREA)
         x = mobileF
      
# Step1:
      
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
         AcceptableNamesGB = c("German Bank", "Tear Drop", "SpawnTow", "Seal Island", "Gully 4Xq", "Undetermined")
      # NB Coastal
         AcceptableNamesNB = c("NB Coastal", "Wolfs Bank")
      # Offshore Banks
         AcceptableNamesOB = c("Offshore Banks", "the Patch", "The Patch", "Patch")
      # Grand Manan Banks
         AcceptableNamesGMB = c("Grand Manan Banks", "Prong", "Southwest Bank", "Northeast Bank")
      
# Step 2:
      
      # NB Coastal
         areaNB=x[which(x$Name == "NB Coastal"), ]
         AreaNB = areaNB$catchAREA 
      # Grand Manan  
         areaGM=x[which(x$Name == "Grand Manan"), ]
         AreaGM = areaGM$catchAREA
      # Grand Manan Banks  
         areaGMB=x[which(x$Name == "Grand Manan Banks"), ]
         AreaGMB = areaGMB$catchAREA
      # Long Island  
         areaLI=x[which(x$Name == "Long Island"), ]
         AreaLI = areaLI$catchAREA
      # German Bank
         areaGB=x[which(x$Name == "German Bank"), ]
         AreaGB = areaGB$catchAREA
      # Seal Island
         areaSI=x[which(x$Name == "Seal Island"), ]
         AreaSI = areaSI$catchAREA
      # Scots Bay  
         areaSB=x[which(x$Name == "Scots Bay"), ]
         AreaSB = areaSB$catchAREA
      # Trinity  
         areaT=x[which(x$Name == "Trinity"), ]
         AreaT = areaT$catchAREA
      # Gannet Dry Ledge  
         areaGDL=x[which(x$Name == "Gannet Dry Ledge"), ]
         AreaGDL = areaGDL$catchAREA  
      # Browns Bank
         areaBB=x[which(x$Name == "Browns Bank"), ]
         AreaBB = areaBB$catchAREA  
      # SW Ground
         areaSW=x[which(x$Name == "SW Grounds"), ]
         AreaSW = areaSW$catchAREA  
      # Yankee Bank
         areaYB=x[which(x$Name == "Yankee Bank"), ]
         AreaYB = areaYB$catchAREA  
      # Lurcher
         areaL=x[which(x$Name == "Lurcher"), ]
         AreaL = areaL$catchAREA  
         
      
      
      # Step 3:
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
                  
# Subset the dataframe catchesAroundReturns for each unique fishing ground (DFO). If all is well this step should have the result cahracter (0)

                  AreaNB[which(is.na(AcceptableNamesNB[AreaNB]))]
                    # Good

                  AreaGMB[which(is.na(AcceptableNamesGMB[AreaGMB]))]
                     #list = AreaGMB[which(is.na(AcceptableNamesGMB[AreaGMB]))]
                      #me = unique(list)
                    # Now subset this observations from catchesAroundReturns:
                      #areaGMB_error = areaGMB[which((areaGMB$catchAREA %in% me)), ]
                      
                  AreaGM[which(is.na(AcceptableNamesGM[AreaGM]))]
                     #list = AreaGM[which(is.na(AcceptableNamesGM[AreaGM]))]
                       # me = unique(list)
                        #areaGM_error = areaGM[which((areaGM$catchAREA %in% me)), ]
                  
                  AreaGB[which(is.na(AcceptableNamesGB[AreaGB]))]
                    list = AreaGB[which(is.na(AcceptableNamesGB[AreaGB]))]
                        me = unique(list)
                          areaGB_error = areaGB[which((areaGB$catchAREA %in% me)), ]
                
                  AreaT[which(is.na(AcceptableNamesT[AreaT]))]
                    list = AreaT[which(is.na(AcceptableNamesT[AreaT]))]
                        me = unique(list)
                          areaT_error = areaT[which((areaT$catchAREA %in% me)), ]
                        
                  AreaSI[which(is.na(AcceptableNamesSI[AreaSI]))]
                    list = AreaSI[which(is.na(AcceptableNamesSI[AreaSI]))]
                      me = unique(list)
                        areaSI_error = areaSI[which((areaSI$catchAREA %in% me)), ]
                  
                  AreaGDL[which(is.na(AcceptableNamesGDL[AreaGDL]))]
                    list = AreaGDL[which(is.na(AcceptableNamesGDL[AreaGDL]))]
                      me = unique(list)
                        areaGDL_error = areaGDL[which((areaGDL$catchAREA %in% me)), ]
                    
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
# QC for when there are mismatches:
                        
                              # NB Coastal, Grand Manan, Seal Island and Gannet Dry Ledge have no mis-matches
                              # There is at least one mis-match with Grand Manan Banks, German Bank and Trinity 
                              
                              # Export the .csv - mismatchFishingGrounds
                               #mismatch = rbind(areaGMB_error, areaGM_error, areaGB_error, areaT_error, areaGDL_error, areaSI_error)
                              
                               
                               # this was the original without the extra dates:  
                                 # write.table(mismatch, file= "mismatchFishingGrounds.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                                
                                #write.table(mismatch, file= "mismatchedFishingGrounds.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    
                  
                  ### need to be able to export the mismatchFishingGrounds also to see which tag numbers need to be looked at. edit the scipt...
                  
                  
                  
                  
                  
# To make the QC easier - for instances where there has been only one catch for the date and vessel provided then this GPS coordinate can be automatically assigned
                  
                     catchesAroundReturns = x   
                        
                  head(catchesAroundReturns)
                  catchesAroundReturns$no  = 1:1
                  
                  # need to select for tag numbers where there is only one, i.e unique, or count
                  
                  solo <-aggregate(no~TAG_NUMBER, catchesAroundReturns, FUN=sum)
                  oneChoice = solo[which(solo$no < 2), ]
                  tags = oneChoice$TAG_NUMBER
                  
                  head(catchesAroundReturns)
                  
                  catchesAroundReturns = as.data.frame(catchesAroundReturns)
                  
                  
                  # Now use the list created (tags) to select those values in the catchesAroundReturns df
                  
                  jennaGPS = catchesAroundReturns[which((catchesAroundReturns$TAG_NUMBER %in% tags)), ]
                  head(jennaGPS)
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
                  
                  # currently I have 303 PS tag returns, and 193 GPS locations can be automatically generated.
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
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  ##### exrtra for having the extra dates
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