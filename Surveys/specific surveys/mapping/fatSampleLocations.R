

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

loadfunctions( "polygons")
loadfunctions( "acousticHerring")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for SW Nova
CP <- as(extent(-66.5, -64, 35, 45), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Load Catch Locatiosn 
locations = read.csv("samples.csv")

# Location of VHSV confirmed case
x = -66.2009
y = 43.2695

vhs = data.frame(x,y)

# Produce figure of the survey plan
ggplot(vhs, aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') + geom_point(aes(x = x, y = y),colour = "red", size = 3)+ labs(x=NULL, y=NULL) +  geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black')

+ annotate("text", x = -65.75, y = 43.85, label = "Nova Scotia")   
