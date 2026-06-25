
# Way to use a common border to clip polygons

require(adehabitat)
require(rgeos)
require(maptools)
require(PBSmapping)
require(plyr)
require(ggplot2) #for fortify
require(raster)

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Load stratum polygons
SUA = read.csv("new_lines.csv")
polys = as.PolySet(SUA, projection="LL")

# Load data
herring = read.csv("test.1.csv")

ggplot(polys, aes(x=X, y=Y)) + geom_point(aes(size = Sv_mean, colour = Boat))  +ylab("")+xlab("") + ggtitle(label = "Sv mean using 1nm divisions") 

+ geom_point(data = herring, aes(x=X, y=Y, size = Sv_mean)) + annotate("text", x = -65.75, y = 43.85, label = "Nova Scotia") + ggtitle(label = "German Bank Acoustic Survey #1, Aug 21 2016") 


head(herring)

# To plot this:

GBpoly = read.csv("mapping3.csv")
GBpoly = as.PolySet(GBpoly, projection="LL")

surveyLines = read.csv("test.1.csv")


ggplot(polySept1, aes(x=X, y=Y))+ geom_polygon(aes(group=PID))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') + geom_segment(data=surveyLines, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size=1)

+ ggtitle(label = "German Bank Survey #2")  + coord_map() + guides(fill=FALSE)  +ylab("") + xlab("") + scaleBar(lon = -66, lat = 43.25, distanceLon = 4, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") 

+ geom_point(data = herring, aes(x=X, y=Y, col=Vessel)) + annotate("text", x = -65.75, y = 43.85, label = "Nova Scotia") + ggtitle(label = "German Bank Acoustic Survey #1, Aug 21 2016") 

calcArea(polys) 
calcArea(GBpoly) 


ggplot(subset(GBpoly), aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))



