
# Tagging Data

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

loadfunctions( "acousticHerring")

# Raw Data - Tag releases
rel = read.csv("2016tagReleases.csv")
rel$no = (1:1)
tail(rel)


# Unique locations
x = unique(rel[c("DecLongRelease", "DecLatRelease")])
dim(x)
x$set = 1:39

release = merge(rel, x, by = "DecLatRelease")
head(release)
release$DecLongRelease.y = NULL
release$X = release$DecLongRelease.x 
release$Y = release$DecLatRelease 

summary = with(release, tapply(no, list(set), sum))
summary = as.data.frame(summary)
summary$set = 1:39

x = as.data.frame(unique(release[c("X", "Y", "set", "RELEASE_DATE", "RELEASE_LOCATION", "RELEASE_VESSEL")]))
dim(x)

relINFO = merge(summary, x, by = "set", )
relINFO$no_tags = relINFO$summary

# Data set that contains the summary release information
relINFO
write.table(relINFO, file= "relINFO.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


# Determine limits for mapping
min(relINFO$X)
max(relINFO$X)

min(relINFO$Y)
max(relINFO$Y)

# Mapping

# Load German Bank Box Data
boxes = read.csv("surveyBoxes.csv")
    # German
    germanBox=boxes[which(boxes$Box == "GermanBank"), ]
    scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
    northeast = boxes[which(boxes$Box == "NortheastBank"), ]

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Limits
CP <- as(extent(-68, -64, 43, 46), "SpatialPolygons")
# German Bank
CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


loadfunctions( "acousticHerring")

# Produce figure of the unique tagging locations of 2016
ggplot(relINFO, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') +  geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black')+  geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_point(aes(size = no_tags), pch = 21) + ggtitle("HSC Tagging Locations 2016")+ labs(x=NULL, y=NULL)+ scaleBar(lon = -64.6, lat = 43.3, distanceLon = 15, distanceLat = 10, distanceLegend = -6, dist.unit = "km") + coord_map() 

colnames(relINFO)

# Summary for each vessel
ggplot(relINFO, aes(x=RELEASE_VESSEL)) + geom_bar()+ labs(x=NULL, y=NULL) + ggtitle("Number of Tagging Events per Vessel 2016") + scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14), limits = c(0,14)) 

# Summary for dates
colnames(relINFO)
relINFO$RELEASE_DATE = as.Date(relINFO$RELEASE_DATE, "%Y-%m-%d")
head(relINFO)
relINFO$month = month(relINFO$RELEASE_DATE)

ggplot(relINFO, aes(x=month)) + geom_bar()+ labs(x=NULL, y=NULL) + ggtitle("Number of Tagging Events per Month 2016") 

# Summary for locations
unique(relINFO$RELEASE_LOCATION)

ggplot(relINFO, aes(x=RELEASE_LOCATION)) + geom_bar()+ labs(x=NULL, y=NULL) + ggtitle("Number of Tagging Events per Location 2016") + scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), limits = c(0,30)) 




# Load tag return data
return = read.csv("tagRelDat2016.csv")
paths = read.csv("tagPaths.csv")

# formatting
return$RETURN_DATE = as.Date(return$RETURN_DATE, "%Y-%m-%d")
return$Release.Date = as.Date(return$Release.Date, "%Y-%m-%d")

paths$group = as.factor(paths$group)
head(paths)

CP <- as(extent(-68, -64, 43, 46), "SpatialPolygons")
# German Bank
CP <- as(extent(-68, -65.5, 43, 44.7), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# divide into group for easier visualization

ids = c(1:40, 42:52, 54,56:61, 63:130, 132:142, 144:148)
ids2 = c(41,53,55,62,131,143)
p1 = paths[which((paths$group %in% ids)), ]
p2 = paths[which((paths$group %in% ids2)), ]
# plot

boxes = read.csv("surveyBoxes.csv")
# German
germanBox=boxes[which(boxes$Box == "GermanBank"), ]
scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
northeast = boxes[which(boxes$Box == "NortheastBank"), ]
feeding = boxes[which(boxes$Box == "FeedingGround"), ]


ggplot(p1, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') +geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='mintcream',col='black') + geom_polygon(data=northeast,aes(x=X, y=Y, group=Box),fill='mintcream',col='black')+ geom_polygon(data=feeding,aes(x=X, y=Y, group=Box),fill='mintcream',col='black')+ geom_path(aes(col = group, group=group), lwd = 2, show.legend = FALSE) +geom_point(pch=19, alpha =0.5, size = 2) + ggtitle("SW Nova Scotia Tag Returns 2016") + labs(x=NULL, y=NULL) + coord_map() + annotate("text", x = -66.17, y = 43.28, label = "German Bank") + annotate("text", x = -67.05, y = 43.5, label = "Feeding Area?") + annotate("text", x = -67.05, y = 44.39, label = "Northeast Bank")

gglocator()

ggplot(p2, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') +geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='mintcream',col='black') + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='mintcream',col='black') + geom_path(aes(col = group, group=group), lwd = 2, show.legend = TRUE) +geom_point(pch=19, alpha =0.5, size = 2) + ggtitle("Outer to Inner BOF Migrations 2016") + labs(x=NULL, y=NULL) + coord_map()

p1=paths[which(paths$group == ids), ]
p2=paths[which(paths$div == 2), ]
p3=paths[which(paths$div == 3), ]
p4=paths[which(paths$div == 4), ]
p5=paths[which(paths$div == 5), ]
p6=paths[which(paths$div == 6), ]
p7=paths[which(paths$div == 7), ]
p8=paths[which(paths$div == 8), ]



 
# Determine limits for mapping
colnames(return)
min(return$X_return)
max(return$X_return)

min(return$Y_return)
max(return$Y_return)

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Limits
CP <- as(extent(-67.5, -66, 43, 44.5), "SpatialPolygons")
CP <- as(extent(-67.5, -64, 43, 45.5), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# German/Scots migrations
gerScot = c(41, 53, 55, 62, 131 ,143)
gerScots = paths[which((paths$group %in% gerScot)), ]

write.table(gerScots, file= "germanScots.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

# Other
othe = c(121,122,123,124,125,134)

(5,44,115,116,117,118,119,120)

other = paths[which((paths$group %in% othe)), ]

write.table(gerScots, file= "germanScots.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

p_ = paths[which(paths$group == 148), ]
p_

# Produce figure of the tag return locations on the map
ggplot(other, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') +geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_path(aes(col = group, group=group), lty = 1, lwd = 1) +geom_point(pch=21) + ggtitle("Caught on German Bank, found in Scots Bay - 2016") + labs(x=NULL, y=NULL) + coord_map() 

# Produce figure of the paths of a portion of the tag returns
ggplot(paths, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') +geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_path(aes(col = group, group=group), lty = 1, lwd = 1) +geom_point(pch=21) + ggtitle("Caught on German Bank, found in Scots Bay - 2016") + labs(x=NULL, y=NULL) + coord_map()

# tag releases 2016
ggplot(return, aes(x=X_release, y=Y_release)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') +geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_point(col = "orange", size = 2) + ggtitle("Tag Releases 2016")+ labs(x=NULL, y=NULL) + coord_map() 

# tag returns 2016
ggplot(return, aes(x=X_return, y=Y_return)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') +geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_point(col = "orange", size = 2) + ggtitle("Tag Returns 2016")+ labs(x=NULL, y=NULL) + coord_map() 





geom_path(aes(col = group, group=group), lty = 1, lwd = 1) +geom_point(pch=21) + ggtitle("Caught on German Bank, found in Scots Bay - 2016") + labs(x=NULL, y=NULL) + coord_map() 



min(p1$X)
min(p1$Y)
max(p1$X)
max(p1$Y)


head(paths)
colnames(return)
head(return)

p + geom_path(data=d, aes(x=lon, y=lat, group=group), color="black", size=1)
+ geom_line()

+ geom_point(pch = 21, col = "red") + geom_point(aes(x = X_release, y = Y_release))

+  geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_point(aes(size = no_tags), pch = 21) + ggtitle("HSC Tagging Locations 2016")+ labs(x=NULL, y=NULL)+ scaleBar(lon = -64.6, lat = 43.3, distanceLon = 15, distanceLat = 10, distanceLegend = -6, dist.unit = "km") + coord_map() 


