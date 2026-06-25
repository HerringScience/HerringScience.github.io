



# Match returns to commercial catch for GPS location

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf")

loadfunctions( "acousticHerring")

# Load all data needed:
# Load commercial data - this data frame is created from an output of the herring database
catches = read.csv("commercialcatches.csv")

# Load Grounds legend -  dataframe converts numbers to text so we can actually understand the ground
grounds = read.csv("commercialcatchGrounds.csv")

# Load Tag Return Data  
data = read.csv("rawReturn.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

summary(data)
head(data)
# there are 344 tag returns


# Format commercial catch data

# catches
catches$LAND_DATE = as.Date(catches$LAND_DATE, "%Y-%m-%d", origin = "1960-10-01")

# Combine grounds with catches
ccatches = merge(catches, grounds)
head(ccatches)


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



# Create the same id for the commercial catches
ccatches$id = paste(ccatches$LAND_DATE, ccatches$Vessel.name)

head(psReturns)
dim(psReturns)   
length(unique(psReturns$id))

# 190 unique events where tags were captured by purse seine.


# Once the data has already been QC'ed                        
listofIDS = psReturns$id
length(listofIDS)

# this step is removing those extra days...
dim(ccatches)
head(ccatches)                  

potentialCatchLocations = ccatches[which((ccatches$id %in% listofIDS)), ]
dim(potentialCatchLocations)
head(potentialCatchLocations)

length(unique(psReturns$id))


# There are 198 potential catches that can be linked to the 188 unique return events

returnsAndCCatch  = merge(potentialCatchLocations, psReturns, by = "id")                     
head(returnsAndCCatch)
str(returnsAndCCatch)

returnsAndCCatch$TAG_NUMBER = as.numeric(returnsAndCCatch$TAG_NUMBER)
returnsAndCCatch <-returnsAndCCatch[order(returnsAndCCatch$DATE, returnsAndCCatch$TAG_NUMBER),]

returnsAndCCatch$catchAREA = as.character(returnsAndCCatch$catchAREA)
returnsAndCCatch$Name = as.character(returnsAndCCatch$Name)


# Add GPS coordiantes from DFO for a future QC step:
      head(returnsAndCCatch)
      dim(returnsAndCCatch)
      
      head(returnGPS)
      dim(returnGPS)

# Only need TAG_NUMBER, X and Y:
      returnGPS$id = NULL
      returnGPS$RETURN_GEAR_TYPE = NULL
      returnGPS$RETURN_FISH_PLANT = NULL
      returnGPS$catchAREA = NULL

head(returnGPS)

x = merge(returnsAndCCatch, returnGPS, by = "TAG_NUMBER")
head(x)

returnsAndCCatch = x
head(returnsAndCCatch)

save(returnsAndCCatch, file="returnsAndCCatch.RData", compress=T)

