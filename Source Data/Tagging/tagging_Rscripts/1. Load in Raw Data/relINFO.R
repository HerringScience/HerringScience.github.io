
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata","PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "measurements", "sp", "ggrepel")


# we need to add NAFO subunit to this DF:

loadfunctions( "acousticHerring")


# Produce all tag return DF's

# First load the release data
  # Up to 2019
  rel = read.csv("TaggingEvents.csv")

  # Includes 2020
  rel = read.csv("TaggingEvents2020.csv")
  
  head(rel)

  
  
# Load ground polygons from Tim
  polysT = read.csv("timGrounds.csv")

  head(polysT)

  unique(polysT$Box)
  
    
# Load NAFO subunits
  polysNAFO = read.csv("NAFO_subunits.csv")
  
      head(polysNAFO)  
      unique(polysNAFO$Area)
# select 5Yb, 4Xr, 4Xq, 4Xs

# Just need a list of boxes with their associated subunit ID
  
  
  # Summarize into events, applys homemade function tagEve
    relINFO = tagEve(rel = rel )
    head(relINFO)
    str(relINFO)
  
    dim(rel)
    
  
    # QC
    
    # Check # of tags for 2020
  
  #  d2020 = relINFO[which(relINFO$Year == "2020"), ]
  #  write.table(d2020, file= "d2020.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
    
    
# Use point in polygon to automatically determine fishing ground
  # Load ground polygons from Tim

  ### Point in Polygon

  # Determine catchArea using GPS coordinates and the function point.in.polygon:
            area1=polysT[which(polysT$Box == "NB Coastal"), ]
            area2=polysT[which(polysT$Box == "Grand Manan"), ]
            area3=polysT[which(polysT$Box == "Grand Manan Banks"), ]
            area4=polysT[which(polysT$Box == "Long Island"), ]
            area5=polysT[which(polysT$Box == "German Bank"), ]
            area6=polysT[which(polysT$Box == "Seal Island"), ]
            area7=polysT[which(polysT$Box == "Scots Bay"), ]
            area8=polysT[which(polysT$Box == "Yankee Bank"), ]
            area9=polysT[which(polysT$Box == "Trinity"), ]
            area10=polysT[which(polysT$Box == "Browns Bank"), ]
            area11=polysT[which(polysT$Box == "SW Grounds"), ]
            area12=polysT[which(polysT$Box == "Gannet Dry Ledge"), ]
            area13=polysT[which(polysT$Box == "Lurcher"), ]
                  
            
            # NAFO Units
            area11=polysNAFO[which(polysNAFO$Area == "5Yb"), ]
            area22=polysNAFO[which(polysNAFO$Area == "4Xr"), ]
            area33=polysNAFO[which(polysNAFO$Area == "4Xq"), ]
            area44=polysNAFO[which(polysNAFO$Area == "4Xs"), ]
            
                  ids =(relINFO$set)
                  head(relINFO)
# Ground
            r = data.frame( set=ids, X = NA, Y = NA, NB_Coastal = NA, Grand_Manan = NA, Grand_Manan_Banks = NA,Long_Island = NA, German_Bank = NA, Seal_Island = NA, Scots_Bay = NA,Yankee_Bank = NA,Trinity = NA,Browns_Bank = NA,SW_Grounds = NA,Gannet_Dry_Ledge = NA,Lurcher = NA) 

          
            
# Ground
          for (i in 1:nrow(r)){
            test = which(relINFO$set==r[i, "set"])
            test1 = relINFO[test, ]
            r$Y[i] = test1$Y
            r$X[i] = test1$X
            r$NB_Coastal[i] = point.in.polygon(r$Y[i], r$X[i], area1$Y, area1$X, mode.checked=FALSE)
            r$Grand_Manan[i] = point.in.polygon(r$Y[i], r$X[i], area2$Y, area2$X, mode.checked=FALSE)
            r$Grand_Manan_Banks[i] = point.in.polygon(r$Y[i], r$X[i], area3$Y, area3$X, mode.checked=FALSE)
            r$Long_Island[i] = point.in.polygon(r$Y[i], r$X[i], area4$Y, area4$X, mode.checked=FALSE)
            r$German_Bank[i] = point.in.polygon(r$Y[i], r$X[i], area5$Y, area5$X, mode.checked=FALSE)
            r$Seal_Island[i] = point.in.polygon(r$Y[i], r$X[i], area6$Y, area6$X, mode.checked=FALSE)
            r$Scots_Bay[i] = point.in.polygon(r$Y[i], r$X[i], area7$Y, area7$X, mode.checked=FALSE)
            r$Yankee_Bank[i] = point.in.polygon(r$Y[i], r$X[i], area8$Y, area8$X, mode.checked=FALSE)
            r$Trinity[i] = point.in.polygon(r$Y[i], r$X[i], area9$Y, area9$X, mode.checked=FALSE)
            r$Browns_Bank[i] = point.in.polygon(r$Y[i], r$X[i], area10$Y, area10$X, mode.checked=FALSE)
            r$SW_Grounds[i] = point.in.polygon(r$Y[i], r$X[i], area11$Y, area11$X, mode.checked=FALSE)
            r$Gannet_Dry_Ledge[i] = point.in.polygon(r$Y[i], r$X[i], area12$Y, area12$X, mode.checked=FALSE)
            r$Lurcher[i] = point.in.polygon(r$Y[i], r$X[i], area13$Y, area13$X, mode.checked=FALSE)
            
          }
          

            head(r)
              
            write.table(r, file= "r.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
            
            
# NAFO Subunit
            e = data.frame( set=ids, X = NA, Y = NA, Subunit4Xs = NA, Subunit4Xr = NA, Subunit4Xq = NA, Subunit5Yb = NA)                         
            
            
# NAFO subunit
            # Ground
            for (i in 1:nrow(e)){
              test = which(relINFO$set==e[i, "set"])
              test1 = relINFO[test, ]
              e$Y[i] = test1$Y
              e$X[i] = test1$X
              e$Subunit5Yb[i] = point.in.polygon(e$Y[i], e$X[i], area11$Y, area11$X, mode.checked=FALSE)
              e$Subunit4Xr[i] = point.in.polygon(e$Y[i], e$X[i], area22$Y, area22$X, mode.checked=FALSE)
              e$Subunit4Xq[i] = point.in.polygon(e$Y[i], e$X[i], area33$Y, area33$X, mode.checked=FALSE)
              e$Subunit4Xs[i] = point.in.polygon(e$Y[i], e$X[i], area44$Y, area44$X, mode.checked=FALSE)
              
            }
            
            
            head(e)
           summary(e)
            
            
            
            
             head(r)
            
            re = merge(r,e)
            head(re)
            
            
            
            # Now QC - are there any events where the GPS coordinate matched two polygons? This is signified by a 2 under Max
              summary(re)
        
          # I have 2's for 
                # German Bank and Gannet Dry Ledge - so there is one boundary return
        
                  # How do we decide which box to include it in -  Take the more north
        
        
                        # Manually change to the area more north, the 2 becomes a 1 or 0  
                          why = re[which((re$Gannet_Dry_Ledge > 1)), ]
                          why
                        
                              # Fix the Gannet_Dry_Ledge
                              re[188,]
                              re[188,8] = 0
                              re[188,15] = 1        
                              
                    
                        
                        # Double Check:
                        summary(re)
                        head(re)
          
      # Next step is to go through each area, and for those that have id's > 0 they are assigned the fishing ground that has been identified
                        
# Ground                        
            re$FishingGround = NA        
            head(re)             
            
            
            
            
                  # NB Coastal
                  NB =re[which((re$NB_Coastal > 0)), ]
                  NB$FishingGround = "NB Coastal"       
                  
                  # Grand Manan
                  GM =re[which((re$Grand_Manan > 0)), ]
                  GM$FishingGround = "Grand Manan"       
                  
                  # Grand Manan Banks
                  GMB =re[which((re$Grand_Manan_Banks > 0)), ]
                  GMB$FishingGround = "Grand Manan Banks"       
                  
                  # Long Island
                  LI =re[which((re$Long_Island > 0)), ]
                  LI$FishingGround = "Long Island"       
                  
                  # German Bank
                  GB =re[which((re$German_Bank > 0)), ]
                  GB$FishingGround = "German Bank"       
                  
                  # Seal Island
                  SI =re[which((re$Seal_Island > 0)), ]
                  SI$FishingGround = "Seal Island"       
                  
                  # Scots Bay
                  SB =re[which((re$Scots_Bay > 0)), ]
                  SB$FishingGround = "Scots Bay"       
                  
                  # Yankee Bank
                  YB =re[which((re$Yankee_Bank > 0)), ]
                  YB$FishingGround = "Yankee Bank"       
                  
                  # Trinity
                  TR=re[which((re$Trinity > 0)), ]
                  TR$FishingGround = "Trinity"       
              
                    # Browns Bank: Up to this point we have not had any activity on Browns Bank
                    # BB=r[which((r$Browns_Bank > 0)), ]
                    # BB$FishingGround = "Browns Bank"       
                    
                  # SW Grounds
                  SW =re[which((re$SW_Grounds > 0)), ]
                  SW$FishingGround = "SW Grounds"       
                    
                  # Gannet Dry Ledge
                  GDL =re[which((re$Gannet_Dry_Ledge > 0)), ]
                  GDL$FishingGround = "Gannet Dry Ledge"       
                  
                  # Lurcher
                  L=re[which((re$Lurcher > 0)), ]
                  L$FishingGround = "Lurcher"       
                  
                      merged  = rbind(L, GDL, TR, YB, SB, SI, GB, LI, GMB, GM, SW, NB)
                        head(merged)
                          dim(merged)
                      
                            summary(merged)      
      
                    merged$FishingGround      
      
                    grounds  = data.frame(merged$set, merged$FishingGround)
                    head(grounds)
 
                    colnames(grounds) = c("set","FishingGround")
                    
                                       
# NAFO Subunit:
      re$NAFOSub = NA        
      head(re)             
      
      # 5Yb
      Yb =re[which((re$Subunit5Yb > 0)), ]
      Yb$NAFOSub = "5Yb"       
      
      # 4Xr
      Xr =re[which((re$Subunit4Xr > 0)), ]
      Xr$NAFOSub = "4Xr"       
      
      # 4Xq
      Xq = re[which((re$Subunit4Xq > 0)), ]
      Xq$NAFOSub = "4Xq"       
      
      # 4Xs
      Xs = re[which((re$Subunit4Xs > 0)), ]
      Xs$NAFOSub = "4Xs" 
      
      
      combi  = rbind(Yb, Xr, Xq, Xs)
      head(combi)
      
      combi$NAFOSub
      
      NAFOu = data.frame(combi$set, combi$NAFOSub)
      head(NAFOu)
      colnames(NAFOu) = c("set", "NAFOSub")
      
      
      
      diesel = merge(NAFOu, grounds)
      head(diesel)
      str(diesel)
      
      
      head(relINFO)
      
  unique(relINFO$set)
  unique(diesel$set)
  
  
  relINFO_ = merge(relINFO, diesel, by = "set")
  head(relINFO_)
  unique(relINFO_$NAFOSub)
  unique(relINFO_$FishingGround)
  
  relINFO = relINFO_
  
  head(relINFO)
  
  save(relINFO, file = "relINFO.RData")
  write.table(relINFO, file= "relINFO.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
  
  
  load("relINFO.RData")
