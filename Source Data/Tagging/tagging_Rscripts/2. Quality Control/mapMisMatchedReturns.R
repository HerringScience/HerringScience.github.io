

# Check coordinates from the commercial catches from those given by DFO after selecting for those tag returns where the catchAREA did not match up with the area fished in the commercial catch with the date and vessel.

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel", "gtools", "dplyr", "rgdal","sf")

loadfunctions( "acousticHerring")

can<-getData('GADM', country="CAN", level=1) # provinces
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
unique(can1$NAME_1)

NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine", "New Hampshire", "Vermont","Massachusetts"),]

# Proper coordinates for Tagging 
CP <- as(extent(-72, -60, 42, 46), "SpatialPolygons")


# just Can

CP <- as(extent(-69, -63, 42, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

polysT = read.csv("timGrounds.csv")
polysNAFO = read.csv("NAFO_subunits.csv")
unique(polysNAFO$Area)


ids = c("5Yb", "4Xr", "4Xq", "4Xs")

polys_NAFO = polysNAFO[which((polysNAFO$Area %in% ids)), ] 
unique(polys_NAFO$Area)


# NAFO
ggplot(coordins,aes(x = X, y = Y)) + geom_polygon(data=polys_NAFO,aes(x=X, y=Y, group=Area), fill = "white", colour = "black")  + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_point(aes(shape = Coord_Type), colour = "blue", size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()




  coordins  = read.csv("checkCoordinates.csv")
head(coordins)
str(coordins)
coordins$TAG_number = as.factor(coordins$TAG_number)
coordins$Coord_Type = as.factor(coordins$Coord_Type)

ggplot(coordins,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_point(aes(shape  = Coord_Type, colour = TAG_number)) + coord_map()

# grounds
  ggplot(coordins,aes(x = X, y = Y)) + geom_polygon(data=polysT,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_point(aes(shape = Coord_Type), colour = "blue", size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()

# NAFO
  head(polysNAFO)
  ggplot(coordins,aes(x = X, y = Y)) + geom_polygon(data=polys_NAFO,aes(x=X, y=Y, group=Area), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_point(aes(shape = Coord_Type), colour = "blue", size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()
  
  
  
## Export one by one:

# where do I want the maps saved?
results = "C:/Users/herri/OneDrive/Documents/Jenna/workspace/"

# create list of counties in data to loop over 
county_list <- unique(coordins$TAG_number)
head(coordins)



# create for loop to produce ggplot2 graphs 
for (i in seq_along(county_list)) { 
  
  # create plot for each county in df 
  plot <- 
    ggplot(subset(coordins, coordins$TAG_number==county_list[i]),
           aes(X, Y)) + 
    geom_polygon(data=polysT,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_point(aes(shape = Coord_Type), colour = "blue", size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map() +
    
    ggtitle(paste(county_list[i]))
  
  # save plots as .png
  ggsave(plot, file=paste(results, county_list[i], ".png", sep="_"), scale=2)
  
  
  # print plots to screen
  print(plot)
}




# look at GPS coordinates given by DFO:
	

# Proper coordinates for Tagging 
CP <- as(extent(-67.5, -64, 44, 46), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Ship Head Cove '91
X = -66.89
Y = 44.9163

# given for Ship Cove
Y1 = 44.91616667	
X1 = -66.88983333


# Money Cove
X = -66.82
Y = 44.777

# Bradford's Cove
X1 = -66.82
Y1 = 44.62983

# The Mumps (Given by DFO aka Duck is Ledge)
  Y1 = 44.67266667	
  X1 = -66.686

          # Duck is Ledge (eastern Grand Manan)
          X = -66.69
          Y = 44.67266


# Pipe Dream (Grand Manan, almost the same as Flagg Cove)
X=-66.75
Y = 44.74283333	

        # Flagg Cove (Grand Manan)
        X1 =-66.75
        Y1= 44.75


# Coastal NB
    # Seeley's Basin (Coastal NB)
    X = -66.6788
    Y = 45.071

    # Seeley's Cove (Coastal NB)
    X1 = -66.6503
    Y1 = 45.0863   
    
    # Mill Cove (Coastal NB)
    X = -66.86
    Y = 45.0395	
      
    # Spruce Island (Coastal NB)
    X = -66.86
    Y = 45.02566667	
      
    # Spectacle (Coastal NB)
    X = -66.92 
    Y = 44.99033333
    
    # Flower Pot (Coastal NB)
    X1 = -66.78 
    Y1 = 45.04533333   
    
    # Winner (North Grand Manan)
    X = -66.76
    Y = 44.77433333	
    
# Eagle Island (Coastal NB)
  
    X = -66.87 
    Y = 45.0255 
    
    #DFO Given
    Y= 44.258
    X = -66.99833333
    
# Database	
    	
    
    X=-66.36666667
    Y= 44.31666667 

test = data.frame(X, Y)

ggplot(test, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_point(pch=21, size = 2, fill = "White")+ labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68")) 


test1 = data.frame(X1, Y1)

+ geom_point(data = test1,aes(x=X1, y=Y1), colour = "red")



 ggplot(test, aes(x=X, y=Y)) + geom_polygon(data=polysT,aes(x=X, y=Y, group=Box),fill = "grey88",  colour = "white") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_point(pch=21, size = 2, fill = "White")+ labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68")) + geom_point(data = test1,aes(x=X1, y=Y1), pch = 21, size = 2, fill = "red")

