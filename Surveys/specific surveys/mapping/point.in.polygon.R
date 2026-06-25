

## Polygons and NAFO divisiosn from Tim Barrett



RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx")

loadfunctions( "acousticHerring")

# Mapping

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
CP <- as(extent(-67.6, -61.8, 43, 45.8), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

boxes = read.csv("catchBoxes.csv")

ggplot(relINFO, aes(x=X, y=Y)) + geom_line(data =NAFO, aes(x =X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), colour = "grey77") + geom_polygon(data=out,aes(x=long, y=lat, group=group), fill = "grey76")  + geom_point(pch=21, size = 2, fill = "White") + ggtitle("Tag Releases") + labs(x=NULL, y=NULL) + coord_map()


NAFO <- read.csv("NAFOareas.csv", header = TRUE, sep = ",")    #NAFO boundaries 

Qarea <- read.csv("QuotaArea.csv", header = TRUE, sep = ",")     #Polygon for quota area - by 
grounds <- read.csv("grounds_.csv", header = TRUE, sep = ",")     #Polygon for quota area - by 

head(relINFO)

if(point.in.polygon((relINFO$X[i],relINFO$Y[i],grounds$X[i],grounds$Y[i])>0)



colnames(DF)[1] <- "GROUND"
ground_list <- unique(DF$GROUND)[!is.na(unique(DF$GROUND))]
M$GROUND <- NA
for(i in 1:nrow(M))
{
  for(j in 1:length(ground_list)) #this loop assigns a fishing ground by checking to see if coordinates are inside the polygons for the fishing grounds
  {
    DF_sub <- DF[DF[,1]==ground_list[j],]
    DF_sub <- DF_sub[!is.na(DF_sub[,4]),]
    if(point.in.polygon(M$LONGITUDE[i],M$LATITUDE[i],DF_sub$X2,DF_sub$Y2)>0)
    {
      M$GROUND[i] <- as.character(ground_list[j])
    }
  }
}