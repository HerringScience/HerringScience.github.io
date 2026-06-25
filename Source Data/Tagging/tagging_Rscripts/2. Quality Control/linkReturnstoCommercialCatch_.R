

# Match returns to commercial catch for GPS location

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf", "pracma")

loadfunctions( "acousticHerring")

# Load returnAndCCatch:

load("returnsAndCCatch.RData")

## This is now the QC where we check if Name and catchArea match up using a list of alternate names:  
                
              
# Need to load in the df with the fishing grounds and alternate names
# Here is where the alternate names for each fishing ground are listed:
                  
    AcceptableNamesGM = c("Grand Manan", "Prong", "White Head", "White Head Grand Manan")
    AcceptableNamesLI = c("Long Island", "Sandy Cove NS", "North West Ledge", "McDormant Patch")
    AcceptableNamesT = c("Trinity","McDormant Patch")
    AcceptableNamesGDL = c("Gannet Dry Ledge", "NW of German Bank", "German Bank", "Tear Drop")
    AcceptableNamesSI = c("Seal Island", "German Bank")
    AcceptableNamesGB = c("German Bank", "Tear Drop", "SpawnTow", "Seal Island", "Gully 4Xq")
    AcceptableNamesNB = c("NB Coastal", "Wolfs Bank")
    AcceptableNamesOB = c("Offshore Banks", "the Patch", "The Patch", "Patch")
    AcceptableNamesGMB = c("Grand Manan Banks", "Prong", "Southwest Bank", "Northeast Bank")
                    
                  
# Subset the dataframe returnsAndCCatch for each unique fishing ground (DFO)
            
            areaNB=returnsAndCCatch[which(returnsAndCCatch$Name == "NB Coastal"), ]
            AreaNB = areaNB$catchAREA      
            
            areaGM=returnsAndCCatch[which(returnsAndCCatch$Name == "Grand Manan"), ]
            AreaGM = areaGM$catchAREA
                  
            areaGMB=returnsAndCCatch[which(returnsAndCCatch$Name == "Grand Manan Banks"), ]
            AreaGMB = areaGMB$catchAREA
                  
            areaLI=returnsAndCCatch[which(returnsAndCCatch$Name == "Long Island"), ]
            AreaLI = areaLI$catchAREA
                  
            areaGB=returnsAndCCatch[which(returnsAndCCatch$Name == "German Bank"), ]
            AreaGB = areaGB$catchAREA
                  
            areaSI=returnsAndCCatch[which(returnsAndCCatch$Name == "Seal Island"), ]
            AreaSI = areaSI$catchAREA
                  
            areaSB=returnsAndCCatch[which(returnsAndCCatch$Name == "Scots Bay"), ]
            AreaSB = areaSB$catchAREA
                  
            areaT=returnsAndCCatch[which(returnsAndCCatch$Name == "Trinity"), ]
            AreaT = areaT$catchAREA
                  
            areaGDL=returnsAndCCatch[which(returnsAndCCatch$Name == "Gannet Dry Ledge"), ]
            AreaGDL = areaGDL$catchAREA  
                    
                  # Areas not current included in results, may need to include in the future:
                        # areaBB=returnsAndCCatch[which(returnsAndCCatch$Name == "Browns Bank"), ]
                        # areaSWG=returnsAndCCatch[which(returnsAndCCatch$Name == "SW Grounds"), ]
                        # areaYB=returnsAndCCatch[which(returnsAndCCatch$Name == "Yankee Bank"), ]
                        # areaL=returnsAndCCatch[which(returnsAndCCatch$Name == "Lurcher"), ]
                  
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

                  

           # if we are all good for QC - then we should see character (0) for each area  
                                    
                  AreaNB[which(is.na(AcceptableNamesNB[AreaNB]))]
                  
                  AreaGMB[which(is.na(AcceptableNamesGMB[AreaGMB]))]
                                  list = AreaGMB[which(is.na(AcceptableNamesGMB[AreaGMB]))]
                                    me = unique(list)
                                      areaGMB_error = areaGMB[which((areaGMB$catchAREA %in% me)), ]
                    
                  AreaGM[which(is.na(AcceptableNamesGM[AreaGM]))]
                                    list = AreaGM[which(is.na(AcceptableNamesGM[AreaGM]))]
                                        me = unique(list)
                                        areaGM_error = areaGM[which((areaGM$catchAREA %in% me)), ]
                                  
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
                                    
                  # If there ARE errors we can export the .csv - mismatchFishingGrounds and back check with teh commercial catches:
                   mismatch = rbind(areaGMB_error, areaGM_error, areaGB_error, areaT_error, areaGDL_error, areaSI_error)
                  
                   
                   # this was the original without the extra dates:  
                    write.table(mismatch, file= "mismatchFishingGrounds.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    
                 
                  

                    
