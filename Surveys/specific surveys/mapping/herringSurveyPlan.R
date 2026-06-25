

RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

  loadfunctions( "polygons")
  loadfunctions( "acousticHerring")
  
# Load land data
  can<-getData('GADM', country="CAN", level=1) # provinces
  NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
  
  # Proper coordinates for Scots Bay
    CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
  
  # Proper coordinates for German Bank 
    CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
                          # German Bank Tow Box
                            CP <- as(extent(-66.5, -66, 43, 43.6), "SpatialPolygons")
  
  proj4string(CP) <- CRS(proj4string(NBNS))
  out <- gIntersection(NBNS, CP, byid=TRUE)

  boxes = read.csv("surveyBoxes.csv")
  #june21
  june21 = read.csv("june21.csv")
  
  # German
  germanBox=boxes[which(boxes$Box == "GermanBank"), ]
  # Scot's
  scotsBox=boxes[which(boxes$Box == "ScotsBay"), ]
  # CTD
  ctd = boxes[which(boxes$Box == "SBocean"), ]
  
  polys = read.csv("scotsPolys.csv")

# Survey Plan -  German Bank
          
  # Load Survey Plans Lines 
    fiveboatsGB = read.csv("fiveboatsGB.csv")
    sixboatsGB = read.csv("sixboatsGB.csv")
    sevenboatsGB = read.csv("sevenboatsGB.csv")
    eightboatsGB = read.csv("eightboatsGB.csv")
    nineboatsGB = read.csv("nineboatsGB.csv")
    tenboatsGB = read.csv("tenboatsGB.csv")
    elevenboatsGB = read.csv("elevenboatsGB.csv")
    twelveboatsGB = read.csv("twelveboatsGB.csv")
    
    eightboatsGB$id = as.factor(eightboatsGB$id)
    eightboatsGB$Vessel = c("SH", "TM", "MS", "LB","SH", "TM", "MS", "LB", "LM", "SL", "LJ", "DV", "LM", "SL", "LJ", "DV")
    
      twelveboatsGB$Vessel = twelveboatsGB$id
    
              # Load Survey Plan Lines for German Bank, Tow Region
    
                  tow5 = read.csv("fiveboatsTow.csv")
                  tow6 = read.csv("sixboatsTow.csv")
                  tow7 = read.csv("sevenboatsTow.csv")
                  tow8 = read.csv("eightboatsTow.csv")
                  tow9 = read.csv("nineboatsTow.csv")
                  
                  tow9$id = as.factor(tow9$id)
                  
    # Produce figure of the survey plan
    ggplot(eightboatsGB, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel, linetype = Vessel), size = 1)+ labs(x=NULL, y=NULL) + scaleBar(lon = -65.8, lat = 43.25, distanceLon = 8, distanceLat = 3, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -65.75, y = 43.85, label = "Nova Scotia") + ggtitle("German Bank Survey #2 - September 8, 2017")  
    
    #+  geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black')
  
# Survey Plan -  Scots Bay
    
    # Load Survey Plans Lines 
    fiveboatsSB = read.csv("fiveboatsSB.csv")
    sixboatsSB = read.csv("sixboatsSB.csv")
    sevenboatsSB = read.csv("sevenboatsSB.csv")
      
    eightboatsSB = read.csv("eightboatsSB.csv")
    nineboatsSB = read.csv("nineboatsSB.csv")
    easternLinesSB = read.csv("easternLinesSB.csv")
    northernLinesSB = read.csv("northernLinesSB.csv")
    
    northernLinesSB$Vessel = "C1"
    ids = (1:6)
    threeboats = sixboatsSB[which((sixboatsSB$id %in% ids)), ]
    threeboats$Vessel = as.factor(threeboats$Vessel) 
    threeboats$Vessel = c("LB", "MS", "DV", "LB", "MS", "DV")
    easternLinesSB$Vessel = "MS"
    
    ids = (1:6)
    sept8 = sixboatsSB[which((sixboatsSB$id %in% ids)), ]
    
    
    jul29 = rbind(sixboatsSB, northernLinesSB)
    
    eightboatsSB$Vessel = c("Fundy Monarch", "Lady Melissa", "Canada 100", "Brunswick Provider", "Lady Janice", "Sealife II", "Morning Star", "Leroy and Barry", "Fundy Monarch", "Lady Melissa", "Canada 100", "Brunswick Provider", "Lady Janice", "Sealife II", "Morning Star", "Leroy and Barry")
    
  
    
    eightboatsSB$id = as.factor(eightboatsSB$id)
    eightboatsSB$Vessel = as.factor(eightboatsSB$Vessel)
    
    
    
    # Produce figure of the survey plan
    ggplot(eightboatsSB, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + geom_polygon(data=ctd,aes(x=X, y=Y)) + labs(x=NULL, y=NULL)+ scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) 
    
    
  
    northernLinesSB$id = as.factor(northernLinesSB$id)
    northernLinesSB$Vessel = as.factor(northernLinesSB$Vessel)
    
    easternLinesSB$id = as.factor(easternLinesSB$id)
    easternLinesSB$Vessel = as.factor(easternLinesSB$Vessel)
    
          aug12 = rbind(sixboatsSB, northernLinesSB, easternLinesSB)
    
    sevenboatsSB$id = as.factor(sevenboatsSB$id)
    sevenboatsSB$Vessel = as.factor(sevenboatsSB$Vessel)
    
    june21$id = as.factor(june21$id)
    june21$vessel = as.factor(june21$vessel)
    june21$id2 = as.factor(june21$id2)
    
    
    # Produce figure of the survey plan
    ggplot(eightboatsSB, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = id), size = 1)+ geom_segment(data = easternLinesSB,aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1) + geom_segment(data = northernLinesSB,aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1) + labs(x=NULL, y=NULL) + scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) 
    
    
    
 ##### Specific plan for Aug 18, 2019 in Scots Bay   
    
    plan = read.csv("plan_Aug18_2019.csv")
    # add CTD box
    
    
    
  # for aug 18, 2019
    ggplot(plan, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')+ geom_polygon(data=ctd,aes(x=X, y=Y),fill='grey',col='black')  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + labs(x=NULL, y=NULL) + scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) 
 
    
    head(sixboatsSB)
    
    
    sixboatsSB$Vessel_name = c("MS", "LM", "C1", "LB", "BP", "LJ")
    northernLinesSB$Vessel_name = c("FM")
    easternLinesSB$Vessel_name = c("SL")
    
    # without survey polygons
    ggplot(sixboatsSB, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour  = Vessel_name), size = 1)+ geom_segment(data = easternLinesSB,aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel_name), size = 1) + geom_segment(data = northernLinesSB,aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel_name), size = 1) + labs(x=NULL, y=NULL) + scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) + ggtitle("Scots Bay Survey #5 Sunday August 4, 2019")
       
    # lines not boxes
    ggplot(sixboatsSB, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1)+ geom_segment(data = easternLinesSB,aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1) + geom_segment(data = northernLinesSB,aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1) + labs(x=NULL, y=NULL) + scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) + ggtitle("Plan A") 
  
    
    
    # When Only the main survey box will be completed  
    
    sept8$Vessel = c("BP", "C1", "ME", "BP", "C1", "ME")
    
    ggplot(threeboats, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black')  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel, linetype = Vessel), size = 1) + labs(x=NULL, y=NULL) + scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) + ggtitle("Scots Bay Survey #8 - September 22, 2017") 
    
    # When Only the main survey box will be completed  
    
    ggplot(sixboatsSB, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey')  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + labs(x=NULL, y=NULL) + scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) + ggtitle("Scots Bay Survey #1 - June 21, 2017") + geom_segment(data = sixboatsSB, aes(x=X, y=Y, xend = Xend, yend = Yend))
    
    ggplot(june21, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey')  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 1, colour = "red") + labs(x=NULL, y=NULL) + scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) + ggtitle("Scots Bay Survey #1 - June 21, 2017") + geom_segment(data = sixboatsSB, aes(x=X, y=Y, xend = Xend, yend = Yend))
    
    
    june21
    
    
    # + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='white',col='black') + geom_polygon(data=polys,aes(x=X, y=Y, group=lines),fill='white',col='black')
    
    
    
    # Only the main box and northern box
    ggplot(both, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey',col='black') + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = id, linetype = id), size = 1)  + labs(x=NULL, y=NULL) + scaleBar(lon = -65.5, lat = 45.01, distanceLon = 6, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + coord_map() + annotate("text", x = -64.73, y = 45.07, label = "Nova Scotia", size = 5) + ggtitle("Scots Bay Survey #1 - June 17, 2017")  
    
    #  + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='white',col='black') + geom_polygon(data=polys,aes(x=X, y=Y, group=lines),fill='white',col='black')
  
    # 
    northernLinesSB$Vessel = 7:7
    northernLinesSB$id = 7:7
    both = rbind(northernLinesSB, sixboatsSB)
    
    write.table(both, file= "scots1.2017.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
    
    #Plankton tow
    ggplot(out,aes(x=long, y=lat, group=group)) + geom_polygon(fill='grey',col='black') + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box),fill='slateblue1',col='black') + geom_segment(data = plankton, aes(x = x, y = y, xend = xend, yend = yend, group = id), size =2) + geom_point(data = cast1, aes(x,y, group = 1)) + geom_point(data = cast2, aes(x,y, group = 1), col = "red")  + coord_map() + labs(x=NULL, y=NULL) + ggtitle("Plankton Tow Location and CTD Casts")    

polys    
scots=polys[which(polys$year == 2013), ]
        
    
x = c(-65.132)    
y = c(45.0885)
xend =c(-65.2168)
yend =c(45.1419)
plankton= data.frame(x,y,xend,yend) 
plankton$id = 1

 
y = 45.115
x = -65.2400
cast1 = data.frame(x,y)
cast1$group = 1

str(cast1)
y = 45.1409
x = -65.2193
cast2 = data.frame(x,y)
cast2$group = 1

    