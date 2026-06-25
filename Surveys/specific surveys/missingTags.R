

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

taglist = read.csv("2021_returns.csv")
head(taglist)

missing = setdiff(taglist$DFO, taglist$HSC)
missing = as.data.frame(missing)

# Tiffany has 8 tag numbers I am missing

write.table(missing, file= "missingTagNumbers.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
