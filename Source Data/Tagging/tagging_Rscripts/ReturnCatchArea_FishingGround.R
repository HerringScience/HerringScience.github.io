


RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf")

loadfunctions( "acousticHerring")
# Produce all tag return DF's


# Produce fishing grounds for all the return data from the returnGPSmaster.csv

# Bring in Return GPS data from DFO for returns 
returnGPS = read.csv("returnGPSMaster.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
head(returnGPS)

# Load ground polygons from Tim
polysT = read.csv("timGrounds.csv")

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

ids =(returnGPS$TAG_NUMBER)
unique(polysT$Box)

CA = (returnGPS$catchAREA)
GT = (returnGPS$RETURN_GEAR_TYPE)

head(returnGPS)

r = data.frame( TAG_NUMBER=ids, catch_area = CA,gear_type = GT, X = NA, Y = NA, NB_Coastal = NA, Grand_Manan = NA, Grand_Manan_Banks = NA,Long_Island = NA, German_Bank = NA, Seal_Island = NA, Scots_Bay = NA,Yankee_Bank = NA,Trinity = NA,Browns_Bank = NA,SW_Grounds = NA,Gannet_Dry_Ledge = NA,Lurcher = NA) 

for (i in 1:nrow(r)){
  test = which(returnGPS$TAG_NUMBER==r[i, "TAG_NUMBER"])
  test1 = returnGPS[test, ]
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

# QC
summary(r)
# Long_Island, German_Bank, Gannet_Dry_Ledge, Trinity


# How do we decide which box to include it in? Take the more north?


        # Manually change to the area more north, the 2 becomes a 1 or 0  
        why = r[which((r$Gannet_Dry_Ledge > 1)), ]
        why = r[which((r$Long_Island > 1)), ]

        why
        
        
        # Fix the Gannet_Dry_Ledge/German_Bank: Gannet Dry Ledge
            r[283,]
            r[283,10] = 0
            r[283,17] = 1        
        
            # Fix the Long Island/Trinity: Long Island
              r[309,]
              r[309,9] = 1
              r[309,14] = 0       
              
              # Double Check:
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
                            # YB =r[which((r$Yankee_Bank > 0)), ]
                            # YB$FishingGround = "Yankee Bank"       
                            
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
                        # L =r[which((r$Lurcher > 0)), ]
                        # L$FishingGround = "Lurcher"       
                        
                merged  = rbind(GDL, T, SB, SI, GB, LI, GMB, GM, NB)
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
                head(returnGPS)
                
                returnsWithFG = merge(returnGPS, merged, by = "TAG_NUMBER")
                head(returnsWithFG)
                          
                          returnsWithFG$gear_type = NULL              
                          returnsWithFG$catch_area = NULL              
                          
                          # This spreadsheet is a way to check if the fishing ground that is generated from the GPS coordinate provided makes sense with the Catch Area provided by the plant:
                          
                          
                          save(returnsWithFG, file="returnsWithFG.RData")
                          
                          write.table(returnsWithFG, file= "returnsWithFG.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                          