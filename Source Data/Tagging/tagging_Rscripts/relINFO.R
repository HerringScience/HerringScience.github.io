
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel")

loadfunctions( "acousticHerring")
# Produce all tag return DF's

# First load the release data

rel = read.csv("TaggingEvents_.csv")


# Summarize into events
  relINFO = tagEve(rel = rel )
  head(relINFO)
  
# Use point in polygon to automatically determine fishing ground
  # Load ground polygons from Tim
  polysT = read.csv("timGrounds.csv")
  
  ### Point in Polygon
  
  
  # sp function which tests for points in polygons
  head(relINFO)
  
  # Need to run through individual events and individual polygons:
  head(relINFO)
  head(polysT)
  
        # Create separate df's for each polygon - probably an easier way to do this..
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
        
  head(relINFO)
  summary(relINFO)
  
  
  ## Need to create a loop which goes through each event and classifies. Basically I want to have a data frame that contains all the unique ID's, along with each Box name, and either a 1 or 0 identifying which box is contained. 
  
  ids =unique(relINFO$set)
  unique(polysT$Box)
  
  head(relINFO)
  
  r = data.frame( set=ids,X = NA, Y = NA, NB_Coastal = NA, Grand_Manan = NA, Grand_Manan_Banks = NA,Long_Island = NA, German_Bank = NA, Seal_Island = NA, Scots_Bay = NA,Yankee_Bank = NA,Trinity = NA,Browns_Bank = NA,SW_Grounds = NA,Gannet_Dry_Ledge = NA,Lurcher = NA) 
  
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
  
  r
  head(r)
  
  # This is an error check - are any Max values for the fishing ground greater than 1? This means that the point is on a boundary between two polygons and a decision will have to be made with regards to what polygon it belongs.
    
    summary(r)
    
    # How do we decide which box to include it in? Take the more north?
  
  
          # Manually change to the area more north, the 2 becomes a 1 or 0  
          why = r[which((r$Gannet_Dry_Ledge > 1)), ]
          why
          r[9,]
          r[9,8] = 0
          r[9,15] = 1              
          
    # Make sure everything is dandy now...
          
          summary(r)
          
          
# Next step is to go through each area, and for those that have id's > 0 they are assigned the fishing ground that has been identified

      r$FishingGround = NA        
      head(r)             
      
      # NB Coastal
      NB =r[which((r$NB_Coastal > 0)), ]
      NB$FishingGround = "NB Coastal"       
      
      # Grand Manan
      GM =r[which((r$Grand_Manan > 0)), ]
      GM$FishingGround = "Grand Manan"       
      
      # Grand Manan Banks
      GMB =r[which((r$Grand_Manan_Banks > 0)), ]
      GMB$FishingGround = "Grand Manan Banks"       
      
      # Long Island
      LI =r[which((r$Long_Island > 0)), ]
      LI$FishingGround = "Long Island"       
      
      # German Bank
      GB =r[which((r$German_Bank > 0)), ]
      GB$FishingGround = "German Bank"       
      
      # Seal Island
      SI =r[which((r$Seal_Island > 0)), ]
      SI$FishingGround = "Seal Island"       
      
      # Scots Bay
      SB =r[which((r$Scots_Bay > 0)), ]
      SB$FishingGround = "Scots Bay"       
      
      # Yankee Bank
      YB =r[which((r$Yankee_Bank > 0)), ]
      YB$FishingGround = "Yankee Bank"       
      
      # Trinity
      T =r[which((r$Trinity > 0)), ]
      T$FishingGround = "Trinity"       
  
                    # Browns Bank: Up to this point we have not had any activity on Browns Bank
                    # BB=r[which((r$Browns_Bank > 0)), ]
                    # BB$FishingGround = "Browns Bank"       
                    
                    # SW Grounds: No activity
                    # SW =r[which((r$SW_Grounds > 0)), ]
                    # SW$FishingGround = "SW Grounds"       
                    
      # Gannet Dry Ledge
      GDL =r[which((r$Gannet_Dry_Ledge > 0)), ]
      GDL$FishingGround = "Gannet Dry Ledge"       
      
      # Lurcher
      L =r[which((r$Lurcher > 0)), ]
      L$FishingGround = "Lurcher"       
      
      merged  = rbind(L, GDL, T, YB, SB, SI, GB, LI, GMB, GM, NB)
      head(merged)
  
  # Remove all the unneeded columns
  
        merged$NB_Coastal = NULL
        merged$Grand_Manan = NULL
        merged$Grand_Manan_Banks = NULL
        merged$Long_Island = NULL
        merged$German_Bank = NULL
        merged$Seal_Island = NULL
        merged$Scots_Bay = NULL
        merged$Yankee_Bank = NULL
        merged$Trinity = NULL  
        merged$Browns_Bank = NULL  
        merged$SW_Grounds = NULL  
        merged$Gannet_Dry_Ledge = NULL  
        merged$Lurcher = NULL  
        
        merged$X = NULL  
        merged$Y = NULL  
        
        
        
  head(merged)  
  head(relINFO)
  dim(merged)  
  
  
  relINFO = merge(relINFO, merged, by = "set")

  save(relINFO, file = "relINFO.RData")

  
  load("relINFO.RData")
