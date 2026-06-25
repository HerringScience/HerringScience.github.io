
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("grounds_.csv")

can<-getData('GADM', country="CAN", level=1) # provinces
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
unique(can1$NAME_1)
NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia", "Maine"),]


# Proper coordinates for Grand Manan/SWNS
CP <- as(extent(-68, -66.4, 44.5, 45.1), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)






# Survey analysis

# Jan 11, 2021 
regions1 = read.table("C1_Jan11_2021Region.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


mapping1 = read.table("C1_Jan11_2021Map.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


# Jan 13, 2021
regions2 = read.table("region_Jan13_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping2 = read.table("map_Jan13_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Jan 14, 2021

regions3 = read.table("Jan14_2021Region.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping3 = read.table("Jan14_2021Map.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


# Combine ALL

regions = rbind(regions1, regions2, regions3)

mapping = rbind(mapping1, mapping2, mapping3)

map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -33.1178, TS50 = -33.2251)



# Add variable according to letter
trans$area = substring(as.character(trans$Transect_No), 1,1)
count(unique(trans$area))      
unique(trans$area)
map$area = substring(as.character(map$Transect_No), 1,1)


          # Grounds
          ggplot(trans, aes(x=X, y=Y)) +  geom_polygon(data=boxes,aes(x=X, y=Y, group=GROUND, fill = GROUND))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() + annotate("text", x =-66.73, y = 45.02, label = "Black's Harbour", size  = 3, colour = "black") + geom_segment(aes(x = -66.74, y = 45.025, xend = -66.79, yend = 45.06, colour = "segment"), lwd=1)+ annotate("text", x =-66.8, y = 44.7, label = "Grand Manan", size  = 3, colour = "white") + ggtitle("Jan 11-12, 2021")


head(boxes)
boxes$group = boxes$GROUND
  
# Jan 13  
ggplot(trans, aes(x=X, y=Y))  +  geom_polygon(data=boxes,aes(x=X, y=Y, group=GROUND, fill = GROUND))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() + annotate("text", x =-66.58, y = 44.7, label = "Grand Manan", size  = 3, colour = "black") + ggtitle("Jan 13, 2021")


# Proper coordinates for the Wolves
CP <- as(extent(-67, -66.5, 44.8, 45.25), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


head(trans)


# Jan 14  
ggplot(trans, aes(x=X, y=Y))  + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() + ggtitle("Jan 14, 2021")



CP <- as(extent(-67.3, -66.4, 44.1, 45), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# All areas

head(trans)

ggplot(trans, aes(x=X, y=Y))  + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Survey_date), size = 1.5)  + labs(x=NULL, y=NULL) + coord_map() 


ggplot(map, aes(x=Xend, y=Yend)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black')  + geom_point(aes(colour = Vessel, size = PRC_ABC)) 


# Break up data by ground

head(trans)
# NB Coastal
ids = c("U","Y", "F", "X", "Y","G", "O", "B")



# Grand Manan
ids = c("J", "A", "M", "K", "R")


transNBCoastal = trans[which((trans$area %in% ids)), ]

transGM = trans[which((trans$area %in% ids)), ]

D=trans[which(trans$area == "D"), ]
jan14= D[which(D$Survey_date == "Jan14_2021"), ]
jan11_D= D[which(D$Survey_date == "Jan11_2021"), ]

L=trans[which(trans$area == "L"), ]
jan13= L[which(L$Survey_date == "Jan13_2021"), ]
jan11= L[which(L$Survey_date == "Jan11_2021"), ]




gm = rbind(jan11, transGM, jan11_D)

transNBCoastal = trans[which((trans$area %in% ids)), ]

# Grand Manan Banks

ids = c("Q", "P", "E")
transGMB = trans[which((trans$area %in% ids)), ]
gmb = rbind(transGMB, jan13)


unique(transNBCoastal$Survey_date)

# 1. NB Coastal
# Jan 11 - U Y F X Y G O B D


CP <- as(extent(-66.8, -66.5, 44.9, 45), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)



ggplot(NBCoastal_trans, aes(x=X, y=Y))  + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = area), size = 1.5)  + labs(x=NULL, y=NULL) + coord_map() 



# 2. Grand Manan

CP <- as(extent(-66.95, -66.5, 44.55, 44.85), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


ggplot(gm, aes(x=X, y=Y))  + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = area), size = 1.5)  + labs(x=NULL, y=NULL) + coord_map() 

ggplot(gm, aes(x=X, y=Y))  + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Survey_date), size = 1.5)  + labs(x=NULL, y=NULL) + coord_map()


#3. GMB

CP <- as(extent(-68, -66.5, 44, 44.7), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


ggplot(gmb, aes(x=X, y=Y))  + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = area), size = 1.5)  + labs(x=NULL, y=NULL) + coord_map() 

ggplot(gmb, aes(x=X, y=Y))  + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Survey_date), size = 1.5)  + labs(x=NULL, y=NULL) + coord_map() 

















# Jan 14: D, B and O

#D  
      D = trans[which((trans$area == "D")), ]


        Y = D

# Load land data

      CP <- as(extent(-67, -66.8, 44, 44.5), "SpatialPolygons")
      proj4string(CP) <- CRS(proj4string(NBNS))
      out <- gIntersection(NBNS, CP, byid=TRUE)




          SUA = read.csv("polygon_D2.csv")
          polyD = as.PolySet(SUA, projection="LL")

              ggplot(D, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ggtitle("Survey D2") + coord_map() + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1))+ geom_polygon(data=polyD,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) 
              

calcArea(polyD) 
# 0.5 km         


# QC
    unique(Y$Transect_No)
    unique(Y$area)

        resultsa = biomassCalc(x = Y, areaKm = 0.5)
        a = unique(resultsa$total_biomass)

# Run results
            tableA = resultTableA(x = Y)
            tableB = resultTableB(x = Y)
            tableC = resultTableC(x = resultsa)
            
            tableC$Layer = "Survey D2"
            
# Export Analysis Tables
      write.table(tableA, file= "D2tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
      write.table(tableB, file= "D2tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      write.table(tableC, file= "D2tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)


      
#B  
      B = trans[which((trans$area == "B")), ]
      
      
      Y = B
      
      # Load land data
      
      CP <- as(extent(-67, -66.8, 44, 44.5), "SpatialPolygons")
      proj4string(CP) <- CRS(proj4string(NBNS))
      out <- gIntersection(NBNS, CP, byid=TRUE)
      
      
      
      
      SUA = read.csv("polygon_B.csv")
      polyB = as.PolySet(SUA, projection="LL")
      
      ggplot(B, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ggtitle("Survey B") + coord_map() + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1))+ geom_polygon(data=polyB,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) 
      
      
      calcArea(polyB) 
      # 0.07 km         
      
      
      # QC
      unique(Y$Transect_No)
      unique(Y$area)
      
      resultsa = biomassCalc(x = Y, areaKm = 0.07)
      a = unique(resultsa$total_biomass)
      
      # Run results
      tableA = resultTableA(x = Y)
      tableB = resultTableB(x = Y)
      tableC = resultTableC(x = resultsa)
      
      tableC$Layer = "Survey B"
      
      # Export Analysis Tables
      write.table(tableA, file= "BtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
      write.table(tableB, file= "BtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      write.table(tableC, file= "BtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
#O  
      O = trans[which((trans$area == "O")), ]
      
      
      Y = O
      
      # Load land data
      
      CP <- as(extent(-67, -66.8, 44, 44.5), "SpatialPolygons")
      proj4string(CP) <- CRS(proj4string(NBNS))
      out <- gIntersection(NBNS, CP, byid=TRUE)
      
      
      
      
      SUA = read.csv("polygon_O.csv")
      polyO = as.PolySet(SUA, projection="LL")
      
      ggplot(O, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ggtitle("Survey O") + coord_map() + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1))+ geom_polygon(data=polyO,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) 
      
      
      calcArea(polyO) 
      # 0.06 km         
      
      
      # QC
      unique(Y$Transect_No)
      unique(Y$area)
      
      resultsa = biomassCalc(x = Y, areaKm = 0.06)
      a = unique(resultsa$total_biomass)
      
      # Run results
      tableA = resultTableA(x = Y)
      tableB = resultTableB(x = Y)
      tableC = resultTableC(x = resultsa)
      
      tableC$Layer = "Survey O"
      
      # Export Analysis Tables
      write.table(tableA, file= "OtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
      write.table(tableB, file= "OtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      write.table(tableC, file= "OtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
                    
