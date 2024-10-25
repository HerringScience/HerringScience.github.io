
rm(list = ls())

#### Set up ####

library(ggrepel)
library(sp)
library(measurements)
library(pander)
library(reshape2)
library(stringr)
library(maps)
library(ggthemes)
library(ggmap)
library(mapproj)
library(classInt)
library(RColorBrewer)
#library(maptools)
library(PBSmodelling)
library(mapdata)
library(plyr)
library(psych)
library(Hmisc)
library(pastecs)
library(reshape)
library(tidyverse)
library(cli)
library(lubridate)
library(reprex)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
#library(weathercan)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
#library(rgeos)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(ggplot2)

# we need to add NAFO subunit to this DF:
# First load the release data and ensure consistent column names with previous worksheet to match tagEve() function (TaggingEvents2020.csv)
# 'TaggingEvents' version from Sharepoint used below

# tagging events in Jenna's workspace updated November 2023

rel <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))
rel <- read.csv("TaggingEvents.csv") %>%
 rename("TAG_NUMBER"= 1,
       "RELEASE_DATE"= 2,
      "X" = 3,
     "Y" = 4,
    "RELEASE_VESSEL" = 5)


#### Load Relevant functions ####

tagEve = function(rel) {
  
  
  
  rel$no = (1:1)
  
  head(rel)
  
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
  
  summary
  
  # again check number of tags
  x = as.data.frame(unique(release[c("X", "Y", "set", "RELEASE_DATE", "RELEASE_VESSEL", "Tagger")]))
  
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
  
  
  
  # add julian day, week
  relINFO$Julian = format(relINFO$RELEASE_DATE, "%j")                   
  relINFO$Julian = as.factor(relINFO$Julian)
  relINFO$week = week(relINFO$RELEASE_DATE)
  
  head(relINFO)
  
  
  return (relINFO)
  
  
  
  
  
}


head(x)
RLibrary("dplyr")


#### Produce all tag return DF's ####



  # make sure there are no duplicate tag #s in rel
  n_distinct(rel$TAG_NUMBER) # no duplicates

#### Load shapefile info ####

  # Load ground polygons from Tim #
  polysT <- read.csv("timGrounds.csv")

  unique(polysT$Box) # Displays the names of the various polygons
  
    
# Load NAFO subunits
  polysNAFO <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/NAFO_subunits.csv"))
  
      unique(polysNAFO$Area) #Displays the names of the NAFO areas
# select 5Yb, 4Xr, 4Xq, 4Xs

# Just need a list of boxes with their associated subunit ID
  
 
#### Make relINFO ####    
         
  # Summarize into events, applys homemade function tagEve
    relINFO <- tagEve(rel = rel ) 
    
    head(relINFO)
    str(relINFO)
    dim(rel)
    
  
    # QC
    
    sum(relINFO$no_tags) # matches total observations

# Use point in polygon to automatically determine fishing ground

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
            
                  ids = (relINFO$set)
                  head(relINFO)
# Ground
            r = data.frame( set=ids, X = NA, Y = NA, NB_Coastal = NA, Grand_Manan = NA, Grand_Manan_Banks = NA,Long_Island = NA, German_Bank = NA, Seal_Island = NA, Scots_Bay = NA,Yankee_Bank = NA,Trinity = NA,Browns_Bank = NA,SW_Grounds = NA,Gannet_Dry_Ledge = NA,Lurcher = NA) 

          
            
# Assigning fishing grounds to sets
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
              
            #write.table(r, file= "r.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
            
            
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
                              re[253,]
                              re[253,8] = 0
                              re[253,15] = 1        
                              
                    
                        
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
  

  
  #write.csv(relINFO, 'relINFO.csv')
  #write.csv(rel, 'alltags.csv')
  
  relINFO <- read.csv('relINFO.csv')
