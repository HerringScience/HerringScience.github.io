
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "ggsn")

loadfunctions( "acousticHerring")

can<-getData('GADM', country="CAN", level=1) # provinces
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
unique(can1$NAME_1)

NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine", "New Hampshire", "Vermont","Massachusetts"),]

# Proper coordinates for Tagging 
CP <- as(extent(-72, -60, 42, 46), "SpatialPolygons")
        
# Just Canada
CP <- as(extent(-69, -63, 42, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Grounds
polysT = read.csv("timGrounds.csv")
# NAFO Subunits
polysNAFO = read.csv("NAFO_subunits.csv")

ids = c("5Yb", "4Xr", "4Xs", "4Xq")
polysNAFO = polysNAFO[which((polysNAFO$Area %in% ids)), ]

# Group by area category & year and sum the tag releases 

# Format data
load("relINFO.RData")

head(relINFO)


# Summary of number of tags applied per ground
            events = relINFO
            summary(relINFO)
            
            sum(events$no_tags)
            
            head(events)
            area = with(events, tapply(no_tags, list(FishingGround), sum))
            
            area = as.data.frame(area)
            x = rownames(area)
            area$RELEASE_LOCATION  =  x
            unique(area$RELEASE_LOCATION)
            head(area)
            area$area = as.numeric(area$area)
            # Total number of tags applied in the program:                                    
                                    sum(area$area)
                        

 # Summary of number of tags applied per NAFO subunit
    nafo = with(events, tapply(no_tags, list(NAFOSub), sum))
        nafo = as.data.frame(nafo)
        x = rownames(nafo)
        nafo$RELEASE_LOCATION  =  x
        head(nafo)
        nafo$nafo = as.numeric(nafo$nafo)
                                    # Total number of tags applied in the program:
                                    sum(nafo$nafo)
                                    
                  

# Map
                                    
# Grounds
polysT = read.csv("timGrounds.csv")                 
head(polysT)

unique(polysT$Box)
ids = c("SW Grounds","Browns Bank")

polysTee= subset(polysT, !(polysT$Box %in% ids))
unique(polysTee$Box)

# remove Browns Bank and SW grounds for mapping

# look at tagging events by grounds
        ggplot(relINFO, aes(x=X, y=Y)) + geom_polygon(data=polysTee,aes(x=X, y=Y, group=Box, fill = Box), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(colour = "white") + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68")) + ggtitle("Tagging Events by Ground") + scaleBar(lon = -64, lat = 43.5, distanceLon = 25, distanceLat = 5, distanceLegend = -4, dist.unit = "km")

# By NAFO subunit

        ggplot(relINFO, aes(x=X, y=Y)) + geom_polygon(data=polysNAFO,aes(x=X, y=Y, group=Area, fill = Area), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(colour = "white") + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68")) + ggtitle("Tagging Events by NAFO Subunit") + scaleBar(lon = -64, lat = 43.5, distanceLon = 25, distanceLat = 5, distanceLegend = -4, dist.unit = "km")


# Look at annual differences in the number of tags applied
        year = with(events, tapply(no_tags, list(Year), sum))
        year = as.data.frame(year)
        x = rownames(year)
        year$Year  =  x
        
write.table(year, file= "year.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        

# Look at temporal differences in the number of tags applied by month

        month = with(events, tapply(no_tags, list(month), sum))
        month = as.data.frame(month)
        x = rownames(month)
        month$Month  =  x



write.table(month, file= "month_2019.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 



  