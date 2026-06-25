
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")



# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Grand Manan/SWNS
CP <- as(extent(-68, -66.4, 44.5, 45.1), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)






# Survey analysis

# Jan 11, 2021 
regions = read.table("C1_Jan11_2021Region.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

regions = read.table("region_Jan13_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)



mapping = read.table("C1_Jan11_2021Map.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


mapping = read.table("map_Jan13_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)



map = mapDat(x = mapping)

trans = transects(x= regions, TS38 = -33.1178, TS50 = -33.2251)

ggplot(trans, aes(x=X, y=Y)) +  geom_polygon(data=boxes,aes(x=X, y=Y, group=GROUND, fill = GROUND))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() + annotate("text", x =-66.73, y = 45.02, label = "Black's Harbour", size  = 3, colour = "black") + geom_segment(aes(x = -66.74, y = 45.025, xend = -66.79, yend = 45.06, colour = "segment"), lwd=1)+ annotate("text", x =-66.8, y = 44.7, label = "Grand Manan", size  = 3, colour = "white") + ggtitle("Jan 11-12, 2021")


boxes = read.csv("grounds_.csv")
head(boxes)
boxes$group = boxes$GROUND
  
# Jan 13  
ggplot(trans, aes(x=X, y=Y))  +  geom_polygon(data=boxes,aes(x=X, y=Y, group=GROUND, fill = GROUND))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() + annotate("text", x =-66.58, y = 44.7, label = "Grand Manan", size  = 3, colour = "black") + ggtitle("Jan 13, 2021")




# Add variable according to letter
        trans$area = substring(as.character(trans$Transect_No), 1,1)
        count(unique(trans$area))      
        unique(trans$area)
        map$area = substring(as.character(map$Transect_No), 1,1)
        

        #14 sub areas
        

#A                
        A = trans[which((trans$area == "A")), ]
        
        CP <- as(extent(-66.9, -66.8, 44.6, 44.65), "SpatialPolygons")
        proj4string(CP) <- CRS(proj4string(NBNS))
        out <- gIntersection(NBNS, CP, byid=TRUE)
        
        
        SUA = read.csv("polygon_A.csv")
        polyA = as.PolySet(SUA, projection="LL")
        
        ggplot(A, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_polygon(data=polyA,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_polygon(data=polyM,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey A") 
        
        calcArea(polyA) 
                # 1.126        
        
        
        # QC
        unique(A$Transect_No)
        unique(A$area)
        
        resultsa = biomassCalc(x = A, areaKm = 1.126)
        a = unique(resultsa$total_biomass)
        
        # Run results
        tableA = resultTableA(x = A)
        tableB = resultTableB(x = A)
        tableC = resultTableC(x = resultsa)
        
        tableC$Layer = "Survey A"
        
        # Scots
        write.table(tableA, file= "AtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(tableB, file= "AtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(tableC, file= "AtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      
        
        
#M                
        M = trans[which((trans$area == "M")), ]
        
        CP <- as(extent(-67, -66.8, 44.57, 44.65), "SpatialPolygons")
        proj4string(CP) <- CRS(proj4string(NBNS))
        out <- gIntersection(NBNS, CP, byid=TRUE)
        
        
        SUA = read.csv("polygon_M.csv")
        polyM = as.PolySet(SUA, projection="LL")
        
        ggplot(M, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + geom_polygon(data=polyM,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + geom_polygon(data=polyA,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey M") 
        
        calcArea(polyM) 
        # 0.362        
        
        
        # QC
        unique(M$Transect_No)
        unique(M$area)
        
        resultsa = biomassCalc(x = M, areaKm = 0.362)
        a = unique(resultsa$total_biomass)
        
        # Run results
        tableA = resultTableA(x = M)
        tableB = resultTableB(x = M)
        tableC = resultTableC(x = resultsa)
        
        tableC$Layer = "Survey M"
        
                  # Scots
                  write.table(tableA, file= "MtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                  write.table(tableB, file= "MtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                  write.table(tableC, file= "MtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
# D
        
        D = trans[which((trans$area == "D")), ]
        Dmap = map[which((map$area == "D")), ]
        
        CP <- as(extent(-66.8, -66.3, 44.7, 44.8), "SpatialPolygons")
        proj4string(CP) <- CRS(proj4string(NBNS))
        out <- gIntersection(NBNS, CP, byid=TRUE)
        
        
        SUA = read.csv("polygon_D.csv")
        polyD = as.PolySet(SUA, projection="LL")
        
        ggplot(D, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2) + geom_polygon(data=polyD,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey D") 
        
        
        ggplot(Dmap, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)+coord_map()
        
        calcArea(polyD) 
        # 16.9         
        
        
        # QC
        unique(D$Transect_No)
        unique(D$area)
        
        resultsa = biomassCalc(x = D, areaKm = 16.9 )
        a = unique(resultsa$total_biomass)
        
        # Run results
        tableA = resultTableA(x = D)
        tableB = resultTableB(x = D)
        tableC = resultTableC(x = resultsa)
        
        tableC$Layer = "Survey D"
                    
                    # Scots
                    write.table(tableA, file= "DtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "DtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "DtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
                    
# K
                    
                    K = trans[which((trans$area == "K")), ]
                    
                    CP <- as(extent(-66.8, -66.3, 44.75, 44.8), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                      SUA = read.csv("polygon_K.csv")
                      polyK = as.PolySet(SUA, projection="LL")
                      
                      ggplot(K, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyK,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey K") 
                      
                    
                    calcArea(polyK) 
                    # 2 km         
                    
                    
                    # QC
                    unique(K$Transect_No)
                    unique(K$area)
                    
                    resultsa = biomassCalc(x = K, areaKm = 2 )
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = K)
                    tableB = resultTableB(x = K)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey K"
                    
                    # Scots
                    write.table(tableA, file= "KtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "KtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "KtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
# J        
        
                    J = trans[which((trans$area == "J")), ]
                    
                    CP <- as(extent(-66.8, -66.3, 44.7, 44.8), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                    SUA = read.csv("polygon_J.csv")
                    polyJ = as.PolySet(SUA, projection="LL")
                    
                    ggplot(J, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyJ,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey J") 
                    
                    
                    calcArea(polyJ) 
                    # 2.2 km         
                    
                    
                    # QC
                    unique(J$Transect_No)
                    unique(J$area)
                    
                    resultsa = biomassCalc(x = J, areaKm = 2.2 )
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = J)
                    tableB = resultTableB(x = J)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey J"
                    
                    # Scots
                    write.table(tableA, file= "JtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "JtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "JtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
# R        
        
                    R = trans[which((trans$area == "R")), ]
                    
                    CP <- as(extent(-66.8, -66.3, 44.75, 44.9), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                    SUA = read.csv("polygon_R.csv")
                    polyR = as.PolySet(SUA, projection="LL")
                    
                    ggplot(R, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyR,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey R") 
                    
                    
                    calcArea(polyR) 
                    # 2 km         
                    
                    
                    # QC
                    unique(R$Transect_No)
                    unique(R$area)
                    
                    resultsa = biomassCalc(x = R, areaKm = 2 )
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = R)
                    tableB = resultTableB(x = R)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey R"
                    
                    # Scots
                    write.table(tableA, file= "RtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "RtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "RtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
#Y
            
                    Y = trans[which((trans$area == "Y")), ]
                    
                    CP <- as(extent(-66.8, -66.6, 44.9, 45), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                    SUA = read.csv("polygon_Y.csv")
                    polyY = as.PolySet(SUA, projection="LL")
                    
                    ggplot(Y, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyY,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey Y") 
                    
                    
                    calcArea(polyY) 
                    # 0.95 km         
                    
                    
                    # QC
                    unique(Y$Transect_No)
                    unique(Y$area)
                    
                    resultsa = biomassCalc(x = Y, areaKm = 0.95 )
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = Y)
                    tableB = resultTableB(x = Y)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey Y"
                    
                    # Scots
                    write.table(tableA, file= "YtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "YtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "YtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    
        
#U        
        
                    
                    U = trans[which((trans$area == "U")), ]
                    
                    
                     Y = U
                    
                    CP <- as(extent(-66.8, -66.6, 44.9, 45), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                    SUA = read.csv("polygon_U.csv")
                    polyU = as.PolySet(SUA, projection="LL")
                    
                    ggplot(Y, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyU,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey U") 
                    
                    
                    calcArea(polyU) 
                    # 0.3 km         
                    
                    
                    # QC
                    unique(Y$Transect_No)
                    unique(Y$area)
                    
                    resultsa = biomassCalc(x = Y, areaKm = 0.3 )
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = Y)
                    tableB = resultTableB(x = Y)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey U"
                    
                    # Scots
                    write.table(tableA, file= "UtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "UtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "UtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    
                    
#F        
        
                    F = trans[which((trans$area == "F")), ]
                    
                    
                    Y = F
                    
                    CP <- as(extent(-66.8, -66.6, 44.9, 45), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                    SUA = read.csv("polygon_F.csv")
                    polyF = as.PolySet(SUA, projection="LL")
                    
                    ggplot(Y, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyF,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey F") 
                    
                    
                    calcArea(polyF) 
                    # 0.83 km         
                    
                    
                    # QC
                    unique(Y$Transect_No)
                    unique(Y$area)
                    
                    resultsa = biomassCalc(x = Y, areaKm = 0.83 )
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = Y)
                    tableB = resultTableB(x = Y)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey F"
                    
                    # Scots
                    write.table(tableA, file= "FtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "FtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "FtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    
                    
                    
# G
                    
                    G = trans[which((trans$area == "G")), ]
                    
                    
                    Y = G
                    
                    CP <- as(extent(-66.8, -66.6, 44.9, 45), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                    SUA = read.csv("polygon_G.csv")
                    polyG = as.PolySet(SUA, projection="LL")
                    
                    ggplot(Y, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyG,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey G") 
                    
                    
                    calcArea(polyG) 
                    # 0.55 km         
                    
                    
                    # QC
                    unique(Y$Transect_No)
                    unique(Y$area)
                    
                    resultsa = biomassCalc(x = Y, areaKm = 0.55 )
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = Y)
                    tableB = resultTableB(x = Y)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey G"
                    
                    # Scots
                    write.table(tableA, file= "GtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "GtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "GtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    

                    
# X                                        
                    
                    X = trans[which((trans$area == "X")), ]
                    
                    
                    Y = X
                    
                    CP <- as(extent(-66.8, -66.6, 44.9, 45), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                    SUA = read.csv("polygon_X.csv")
                    polyX = as.PolySet(SUA, projection="LL")
                    
                    ggplot(X, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyX,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey X") 
                    
                    
                    calcArea(polyX) 
                    # 0.07 km         
                    
                    
                    # QC
                    unique(Y$Transect_No)
                    unique(Y$area)
                    
                    resultsa = biomassCalc(x = Y, areaKm = 0.07 )
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = Y)
                    tableB = resultTableB(x = Y)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey X"
                    
                    # Scots
                    write.table(tableA, file= "XtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "XtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "XtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    
                    
# Jan 13, 2021
                    
                    
# E                    
                    
                    
                    E = trans[which((trans$area == "E")), ]
                    
                    
                    Y = E
                    
                    # Load land data
                    
                    
                    can<-getData('GADM', country="CAN", level=1) # provinces
                    us = getData('GADM', country = "USA", level = 1)
                    can1 = rbind(can,us)
                    unique(can1$NAME_1)
                    NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia", "Maine"),]
                    CP <- as(extent(-67.5, -66.5, 44, 44.5), "SpatialPolygons")
                    proj4string(CP) <- CRS(proj4string(NBNS))
                    out <- gIntersection(NBNS, CP, byid=TRUE)
                    
                    
                    
                    
                    SUA = read.csv("polygon_E.csv")
                    polyE = as.PolySet(SUA, projection="LL")
                    
                    ggplot(E, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ geom_polygon(data=polyE,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey E") 
                    
                    
                    calcArea(polyE) 
                    # 1.4 km         
                    
                    
                    # QC
                    unique(Y$Transect_No)
                    unique(Y$area)
                    
                    resultsa = biomassCalc(x = Y, areaKm = 1.4)
                    a = unique(resultsa$total_biomass)
                    
                    # Run results
                    tableA = resultTableA(x = Y)
                    tableB = resultTableB(x = Y)
                    tableC = resultTableC(x = resultsa)
                    
                    tableC$Layer = "Survey E"
                    
                    # Scots
                    write.table(tableA, file= "EtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    write.table(tableB, file= "EtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    write.table(tableC, file= "EtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                    
            