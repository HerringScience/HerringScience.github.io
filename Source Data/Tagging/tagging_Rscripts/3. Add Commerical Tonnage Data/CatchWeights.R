

# The purpose of this script is to create a dataframe which has each tagging event that had tag returns, including tonnage as well as the number of tag returns obtained.


RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf")

loadfunctions( "acousticHerring")

load("returnsAndCCatch.RData")

load("rawReturn.RData")
dim(rawReturn)

write.table(rawReturn, file= "rawReturn.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

write.table(returnsAndCCatch, file= "returnsAndCCatch.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

dim(returnsAndCCatch)

head(returnsAndCCatch)
returnsAndCCatch$no = 1:1



returnsAndCCatch$Lat = NULL
returnsAndCCatch$Lon = NULL
returnsAndCCatch$Vessel.name = NULL
returnsAndCCatch$LAND_DATE = NULL
returnsAndCCatch$CFV = NULL
returnsAndCCatch$Type = NULL


# Determine number of tags obtained from each catch
catchesWReturns<-aggregate(no~id, returnsAndCCatch, FUN=sum)
colnames(catchesWReturns) = c("id", "NoReturnsPerCatch")
head(catchesWReturns)         

y = merge(returnsAndCCatch, catchesWReturns, by = "id")
head(y)

returnDF = y
head(returnDF)

returnDF$month = month(returnDF$DATE)
returnDF$Year = year(returnDF$DATE)

dim(returnDF)

file="returnDF.RData"
save(returnDF, file="returnDF.RData", compress=T)


write.table(returnDF, file= "returnDF.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
