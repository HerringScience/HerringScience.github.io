
# Plotting the St Mary's Bay Dec 29th 2016 Data - Fish Kill Investigation

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")


# Integration by 250m (only transects)
mapTrack = read.table("maps_jan11.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
mapTrack1 =  mapDat(x = mapTrack)

# To get the total track (entire data set)
track = read.table("track.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
track1 = mapDat(x = track)

# To get density for nonstandard track
dtrack = read.table("densityCalcs.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
dtrack1 = mapDat(x = dtrack)

# Load base data 
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Specify map limits
# Good for plots where fish are found
CP <- as(extent(-66.5, -66, 44.38, 44.5), "SpatialPolygons")
# Whole survey area
CP <- as(extent(-66.5, -65.8, 44.3, 44.8), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Remove zeros, replace with NA
is.na(dtrack1$Density) <- !dtrack1$Density

# Change Density to be called Density (kg/m2)
head(dtrack1)
dtrack1 = rename(dtrack1, c("Density_kg/m2"="Density_kg_m2"))


# Survey Track
ggplot(track1, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1, linetype = 2, colour = "red") + labs(x=NULL, y=NULL) + ggtitle("Morning Star Survey Tracks Dec 29, 2016") + annotate("text", x = -66.19, y = 44.32, label = "St. Mary's Bay",  colour = "blue", size = 4) + annotate("text", x = -65.99, y = 44.57, label = "Digby Neck", colour = "blue", size = 4) + annotate("text", x = -65.9, y = 44.41, label = "Nova Scotia", colour = "blue", size = 5)

# Look at partial track, but only data that is being used for analysis (4 transects)
ggplot(dtrack1, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1, linetype = 2, colour = "red") + labs(x=NULL, y=NULL) + ggtitle("Morning Star Survey Tracks Dec 29, 2016") 


# Total Survey Track with Density
ggplot(dtrack1, aes(x=X, y=Y)) + geom_segment(data = track1, aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1, linetype = 2, colour = "dark grey") + labs(x=NULL, y=NULL) + geom_point(aes(size =Density_kg_m2), shape = 21, fill = "red", colour = "navy") + theme_bw()

# Just to get the transects that will be used for the biomass calculations
transects=dtrack1[which(mapTrack1$Region_ID != 180), ]
nonstandard =dtrack1[which(mapTrack1$Region_ID == 180), ]

write.table(transects, file= "transectsDec29.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

# Partial Survey Track with PRC_ABC (only regions with fish)
ggplot(transects, aes(x=X, y=Y)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1, linetype = 2, colour = "dark grey") + labs(x=NULL, y=NULL)  + geom_point(aes(size =Density_kg_m2), shape = 21, fill = "red", colour = "navy") + theme_bw() + ggtitle("Survey Data from Dec 29, 2016 Collection")

ggplot(dtrack1, aes(x=X, y=Y)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1, linetype = 2, colour = "dark grey") + labs(x=NULL, y=NULL)  + geom_point(aes(size =Density), shape = 21, fill = "red", colour = "navy") + theme_bw() + ggtitle("Nonstandard Data from Dec 29, 2016 Collection")

# Transects with a 250m grid, PRC_ABC, coloured by transect
ggplot(mapTrack1, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Region_name, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map() + theme(legend.position = "none")+ ggtitle("Morning Star Survey - ABC_PRC") 
                                                                                                
# Create a polygon for the three transects
gglocator()

SUA = read.csv("polygon_stmarysbay.csv")
polySB = as.PolySet(SUA, projection="LL")

ggplot(transects, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')+geom_polygon(data=polySB,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour=Region_name), size = 1)

calcArea(polySB)

