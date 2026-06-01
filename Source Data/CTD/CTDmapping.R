


# MAPPING with CTD data
# Creates a map with all the tagging locations as well as a closer look at Scots Bay and German Bank
# Map to take a look at temporal patterns of sampling over the years to see if casts were taken from the same place and time between years

load("castAway.RData")
load("CTDdata.RData")
load("events.RData")

load("germanBox.RData")
load("scotsBox.RData")
load("planktonBox.RData")
load("sealBox.RData")
load("GBocean.RData")
load("SBocean.RData")

GBplank = read.csv("GBplanktonBox.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)







sealBox$X = c(-66.229, -66.229, -66.02, -66.02)
sealBox$Y = c(43.502, 43.2, 43.2, 43.502)


can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Tagging 
CP <- as(extent(-69, -64, 42, 46), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# First plot all CTD locations 
# Plot boxes from the tagging data

boxes = read.csv("catchBoxes_.csv")


# All events in all areas
ggplot(events,aes(x=Lon, y=Lat)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", linetype = 3)  + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_point(aes(colour = ground),pch = 21, size = 2)+ coord_map()





# Look at the two main spawning grounds individually:


# Scots Bay
         scots=events[which(events$ground == "Scots Bay"), ]
         dim(scots)
         scots$class = 1:29
         
         # Proper coordinates for Scots Bay 
              CP <- as(extent(-65.7, -64.6, 44.9, 45.4), "SpatialPolygons")
              proj4string(CP) <- CRS(proj4string(NBNS))
              out <- gIntersection(NBNS, CP, byid=TRUE)
              
              
              
              
              

              # Capture year and depth category
              # colour, shape
              ggplot(scots,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", linetype = 2)+ coord_map() + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + geom_polygon(data=planktonBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_point(aes(colour = Year), size = 2, stroke = 1)+ geom_polygon(data=SBocean,aes(x=X, y=Y, group=Box), fill = NA, colour = "black", lwd = 1) 
        
              
              
              
              
              
                    
              # Look at the cast and whether the location is within the plankton box
              ggplot(scots,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", linetype = 2)+ coord_map() + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + geom_polygon(data=planktonBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)  + geom_text_repel(aes(x = Lon, y = Lat, label = class))
                

                # casts that aren't inside the plankton box
                ids_out = c(22,4,10,11,14,17,2,1,3,6,7,18,8,9,12,5)
                r = length(ids_out)
                # casts inside
                ids_in =c(27,25,26,29,24,19,28,20,16,15,23,21,13)
                t = length(ids_in)
                r+t
                              
                # Scots Out
                scotsout = scots[which((scots$class %in% ids_out)), ]
                scotsout$plankB = 0
                scotsin = scots[which((scots$class %in% ids_in)), ]
                scotsin$plankB = 1
                
                # Contains all scots casts and inclues whether the location is in or out of the plankton box
                 
                scotsplankB = rbind(scotsin, scotsout)
                scotsplankB$class = NULL
                save(scotsplankB, file="scotsplankB.RData", compress=T)
                
                
                
                
                

              # Look at temporal trends on the same spawning grounds..investigate if there was sampling on the same week throughout the years
              
              ggplot(scots,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=scotsBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(aes(colour = week, shape = Year), size=3, stroke  = 1.5)+ coord_map()+ scale_shape(solid=FALSE)
              
              
              
              
               
              # Zoom in on just the plankton box
              
              # Proper coordinates for Scots Bay 
              CP <- as(extent(-65.25, -65.1, 45, 45.2), "SpatialPolygons")
              proj4string(CP) <- CRS(proj4string(NBNS))
              out <- gIntersection(NBNS, CP, byid=TRUE)
              
              head(scots)
              x=scots[which(scots$Lon < -65.1), ]
              y=x[which(x$Lat < 45.1), ]
              z=y[which(y$Lon > -65.27), ]
              
              
              # Total number of casts within the plankton box in Scots Bay:
              
              ggplot(z,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + coord_map() + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + geom_polygon(data=planktonBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_polygon(data=SBocean,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", lwd = 1) + geom_point(aes(shape = DepthCat), alpha = 0.5)
              
              
              write.table(z, file= "scotsbay.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
              # list of unique ID's that are within the plankton box
              ids = unique(z$id)
              scotsComp = stratified[which((stratified$id %in% ids)), ]

              write.table(scotsComp, file= "scotsComp.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
              
              
              
              
              
              
              
              
              
              
              
              
# German Bank
              german=events[which(events$ground == "German Bank"), ]
              seal=events[which(events$ground == "Seal Island"), ]
              
              dim(german)
              german$class = 1:17
              
              
              # Proper coordinates for German Bank 
              CP <- as(extent(-67, -66, 43, 43.5), "SpatialPolygons")
              proj4string(CP) <- CRS(proj4string(NBNS))
              out <- gIntersection(NBNS, CP, byid=TRUE)
              
              ggplot(german,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", linetype = 2) +  geom_polygon(data=sealBox,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", linetype = 2) + coord_map()  +  geom_point(data = seal,aes (x = Lon, y = Lat, colour = Year), size = 1.7, stroke = 0.9)+ theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ geom_polygon(data=GBocean,aes(x=X, y=Y, group=PID), colour = "black", fill="white",lwd = 1)+ geom_point(aes(colour = Year), size = 1.7, stroke = 0.9)
              

              
              # Look at the cast and whether the location is within the plankton box
              ggplot(german,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", linetype = 2)+ coord_map() + theme(panel.background = element_rect(fill = "white", colour = "grey50"))  + geom_polygon(data=GBplank,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_text_repel(aes(x = Lon, y = Lat, label = class))
              
              
              # casts that aren't inside the plankton box
              ids_out = c(1,2,3,4,5,6)
              r = length(ids_out)
              # casts inside
              ids_in =c(7,8,9,10,11,12,13,14,15,16,17)
              t = length(ids_in)
              r+t
              
              # German Out
              germanout = german[which((german$class %in% ids_out)), ]
              germanout$plankB = 0
              germanin = german[which((german$class %in% ids_in)), ]
              germanin$plankB = 2
              
              # Contains all scots casts and inclues whether the location is in or out of the plankton box
              
              germanplankB = rbind(germanin, germanout)
              germanplankB$class = NULL
              save(germanplankB, file="germanplankB.RData", compress=T)
              
              
              
              
# Look at temporal trends on the same spawning grounds..investigate if there was sampling on the same week throughout the years
   
              
          # German Bank        
          ggplot(german,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_polygon(data=germanBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_polygon(data=sealBox,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(aes(colour = week, shape = Year), size=3, stroke  = 1.5)+ coord_map()+ scale_shape(solid=FALSE)+  geom_point(data = seal,aes (x = Lon, y = Lat, colour = week, shape = Year),size = 3, stroke = 1.5)

          
          
          
          # Look at only those casts that were in the same area
            # create a plankton box for German Bank
          
          
          
          # Proper coordinates for German Bank 
          CP <- as(extent(-67, -66, 43, 43.5), "SpatialPolygons")
          proj4string(CP) <- CRS(proj4string(NBNS))
          out <- gIntersection(NBNS, CP, byid=TRUE)

          
          x=german[which(german$Lon < -66.3), ]
          y=x[which(x$Lat > 43.5), ]
      
          
          x = events[which(events$Lon < -66.3), ]
          y=x[which(x$Lat > 43.5), ]
          d = y[which(y$Lat < 43.8), ]
          
          dim(y)
          write.table(y, file= "y.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
          
          
      
          ggplot(d,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", linetype = 2) + coord_map() + theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ geom_polygon(data=GBocean,aes(x=X, y=Y, group=PID), colour = "black", fill="white",lwd = 1) +  geom_polygon(data=GBplank,aes(x=X, y=Y, group=PID), colour = "black", fill="white",lwd = 1) + geom_point(size = 1.7, stroke = 0.9) 
          
          
          
          ggplot(german,aes(x=Lon, y=Lat)) + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_polygon(data=germanBox,aes(x=X, y=Y, group=Box), fill = "white", colour = "black", linetype = 2) + coord_map() + theme(panel.background = element_rect(fill = "white", colour = "grey50"))+ geom_polygon(data=GBocean,aes(x=X, y=Y, group=PID), colour = "black", fill="white",lwd = 1) +  geom_polygon(data=GBplank,aes(x=X, y=Y, group=PID), colour = "black", fill="white",lwd = 1) + geom_point(size = 1.7, stroke = 0.9) 
          
          
          calcArea(GBplank)          
          calcArea(planktonBox)      
          
          events
          