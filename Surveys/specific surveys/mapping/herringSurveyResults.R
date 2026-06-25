

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

loadfunctions( "polygons")
loadfunctions( "acousticTuna")

# weird -  you have to have data points (i.e. surveylines) to be able to run the scale bar
  
  # Load land data
  can<-getData('GADM', country="CAN", level=1) # provinces
  NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

  
    # Proper coordinates for Both
    CP <- as(extent(-67, -63, 43, 46), "SpatialPolygons")
  
    # Proper coordinates for Scots Bay
    CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")

    # Proper coordinates for German Bank 
    CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
    
        # German Bank Tow Box
        CP <- as(extent(-66.5, -66, 43, 43.6), "SpatialPolygons")

        proj4string(CP) <- CRS(proj4string(NBNS))
        out <- gIntersection(NBNS, CP, byid=TRUE)

-----------------------------------------------------------------------------------

# Load combined Maps data for ACTUAL transects edited
  surveyLines_major = read.csv("MAP_aug27.csv")
  surveyLines_major = read.csv("MAP_aug21.csv")
  surveyLines_major = read.csv("MAP_aug13.csv")
  surveyLines_major = read.csv("MAP_jul30.csv")
  surveyLines_major = read.csv("MAP_oct7.csv")
  surveyLines_major = read.csv("MAP_sept1_.csv")
  surveyLines_major = read.csv("MAP_sept12.csv")
  surveyLines_major = read.csv("MAPS_sept19.csv")
  surveyLines_major = read.csv("MAPS_sept19CO.csv")
  surveyLines_major = read.csv("Map_sept6.csv")
  surveyLines_major = read.csv("Map_21.csv")
  
  towLines = read.csv("MAP_sept1tow.csv")
  towLines = read.csv("MAP_sept12Tow.csv")
  towLines = read.csv("tow_sept19.csv")
  towLines = read.csv("Map_sept19Tow.csv")
  
# Make sure the data loaded properly
  ggplot(surveyLines_major, aes(x=X, y=Y))+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) 
  # Tow Area
  ggplot(towLines, aes(x=X, y=Y))+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL)
  
  # To compare to the survey plan
  + geom_segment(data=sevenboatsGB, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1, linetype = 2)
  
  -----------------------------------------------------------------------------------  
  
# Load Polygon Data  
  # German Bank
    SUA = read.csv("polygon_GB.csv")
    polyGB = as.PolySet(SUA, projection="LL")
    calcArea(polyGB)
    
    # Bit wider
    SUA = read.csv("polygon_GB2.csv")
    polyGB = as.PolySet(SUA, projection="LL")
    calcArea(polyGB)
    
    # Bit wider
    SUA = read.csv("polygon_GB3.csv")
    polyGB = as.PolySet(SUA, projection="LL")
    calcArea(polyGB)
    
    # More narrow
    SUA = read.csv("polygon_GB4.csv")
    polyGB = as.PolySet(SUA, projection="LL")
    calcArea(polyGB)
    
        # German Bank Tow Box
          SUA = read.csv("new_lines.csv")
          polyTow = as.PolySet(SUA, projection="LL")
    
          # Bit wider
          SUA = read.csv("new_lines2.csv")
          polyTow = as.PolySet(SUA, projection="LL")
          calcArea(polyTow)
          
          # In between
          SUA = read.csv("new_lines3.csv")
          polyTow = as.PolySet(SUA, projection="LL")
          calcArea(polyTow)
          
          # taller (sept 19)
          SUA = read.csv("new_lines4.csv")
          polyTow = as.PolySet(SUA, projection="LL")
          calcArea(polyTow)
          
              # Sept 6 Poly
                SUA = read.csv("poly_sept6.csv")
                poly6 = as.PolySet(SUA, projection="LL")
          
  # Scots Bay
    SUA = read.csv("polygon_SB.csv")
    polySB_main = as.PolySet(SUA, projection="LL")
    
    SUA = read.csv("polygon_SBEastern.csv")
    polySB_eastern = as.PolySet(SUA, projection="LL")
    
    SUA = read.csv("polygon_SBNorthern.csv")
    polySB_northern = as.PolySet(SUA, projection="LL")
  S
# Take a look at tracks over polygon
    # German Bank
    ggplot(surveyLines_major, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') + geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + annotate("text", x = -65.75, y = 43.85, label = "Nova Scotia")    
   
     # German Bank Tow
    ggplot(towLines, aes(x=X, y=Y))  + geom_polygon(data=polyTow,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map()
          # Add if you want land in the figure
            geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')
    
    
    # German Bank Main Box and Tow Box tracks 
    ggplot(surveyLines_major, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')+geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID)) + geom_segment(data = surveyLines_major, aes(x = X, y = Y, xend = Xend, yend = Yend, colour=Vessel), size = 1)+ geom_polygon(data=polyTow,aes(x=X, y=Y, group=PID), fill = "white") + geom_segment(data = towLines, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + labs(x=NULL, y=NULL)+ annotate("text", x = -65.75, y = 43.85, label = "Nova Scotia") 
    
    # Scot's Bay
    ggplot(map_data,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))+ scaleBar(lon = -65.5, lat = 45.1, distanceLon = 4, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID)) + geom_polygon(data=polySB_eastern,aes(x=X, y=Y, group=PID))+ geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID))+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel, size = ), size = 1) + labs(x=NULL, y=NULL)  + scaleBar(lon = -65.5, lat = 45.1, distanceLon = 4, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + labs(x=NULL, y=NULL)+ coord_map()   
   
    # Look at Both boxes
    ggplot(surveyLines_major, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=polySB_main,aes(x=X, y=Y, group = PID), fill = "blue") + geom_polygon(data=polySB_eastern,aes(x=X, y=Y, group=PID), fill = "blue")+ geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID), fill = "blue") + geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID), fill = "purple") + coord_map() + labs(x=NULL, y=NULL) + annotate("text", x = -66.45, y = 43.7, label = "German Bank", colour = "purple")+ annotate("text", x = -65.6, y = 45.15, label = "Scots Bay", colour = "blue") + annotate("text", x = -65, y = 44.5, label = "Nova Scotia", colour = "white")  + annotate("text", x = -66.2, y = 45.5, label = "New Brunswick", colour = "white") + scaleBar(lon = -63.8, lat = 43.1, distanceLon = 30, distanceLat = 10, distanceLegend = -6, dist.unit = "km")   
    
    # distanceLon and distanceLat change the size of the white and black box
    # distanceLegend changes the distance between the bottom numbers and the rectangle
    
    loadfunctions( "acousticTuna")
    
------------------------------------------------------------------------------------
      
# Run map to show changes in backscatter between 1nm grids PRC_ABC  
      ggplot(surveyLines_major, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    

    ggplot(towLines, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    
    
calcArea(polyTow)
