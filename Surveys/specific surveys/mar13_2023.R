
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

boxes = read.csv("grounds_.csv")

can<-getData('GADM', country="CAN", level=1) # provinces
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)


#NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia", "Maine"),]

NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia"),]

# Survey analysis

# March 13, 2022 
regions = read.table("Region_Mar13_2023.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Mar13_2023.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)
trans = transects(x= regions, TS38 = -33.1178, TS50 = -33.2251)

# Add variable according to letter
trans$area = substring(as.character(trans$Transect_No), 1,1)
count(unique(trans$area))      
unique(trans$area)
map$area = substring(as.character(map$Transect_No), 1,1)



# Grounds
# Includes NB Coastal, Grand Manan and Grand Manan Banks

head(boxes)
unique(boxes$GROUND)

#ids = c("Grand Manan", "Grand Manan Banks", "NB Coastal")
ids = c("Grand Manan", "NB Coastal")

juvMap = boxes[which((boxes$GROUND %in% ids)), ] 

GM = boxes[which((boxes$GROUND == "Grand Manan")), ] 
GMB = boxes[which((boxes$GROUND == "Grand Manan Banks")), ] 
NBC = boxes[which((boxes$GROUND == "NB Coastal")), ] 


# Remove Wolves - improper transects.
head(trans)

trans=trans[which(trans$area != "D"), ]
map=map[which(map$area != "D"), ]


# Proper coordinates for Coastal NB/Grand Manan
CP <- as(extent(-68, -66, 44.5, 45.25), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


# Plot showing the survey
ggplot(trans, aes(x=X, y=Y)) +  geom_polygon(data=juvMap,aes(x=X, y=Y, group=GROUND, fill = GROUND), colour = "black")+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2, colour = "white")  + labs(x=NULL, y=NULL) + coord_map() + ggtitle ("Juvenile Acoustic Survey March 13, 2023")




CP <- as(extent(-67, -66.5, 44.8, 45), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

box = read.csv("polygon_20230313.csv")
box1 = as.PolySet(box, projection="LL")


# Plot showing the survey
ggplot(trans, aes(x=X, y=Y)) +geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') +  geom_polygon(data=box,aes(x=X, y=Y,colour = "black"))+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() + ggtitle ("Juvenile Acoustic Survey March 13, 2023")

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)



          calcArea(box1) 
                #54.85         
        
          resultsa = biomassCalc(x = trans, areaKm = 54.85)
          unique(resultsa$total_biomass)
          
                # Run results
                tableA = resultTableA(x = trans)
                tableB = resultTableB(x = trans)
                tableC = resultTableC(x = resultsa)
                
                
                        # Export Analysis Tables
                        write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                        write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                        write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                        
          
                        
                        
                        
          
                        
# Surveys A and B Off Grand Manan (eastern)
                        
                        X = trans[which(trans$Yend > 44.4), ]
                        X1 = X [which(X$Yend < 44.7), ]
                        
                        tB = X1 [which(X1$area == "B"), ]
                        tA = X1 [which(X1$area == "A"), ]
                        
                        tBA = rbind(tB,tA)
                        
                        mB = map [which(map$area == "B"), ]
                        mA = map [which(map$area == "A"), ]
                        
                        mBA = rbind(mA, mB)
                    
                          # Proper coordinates for Grand Manan/SWNS
                          CP <- as(extent(-66.8, -66.5, 44.5, 44.65), "SpatialPolygons")
                          proj4string(CP) <- CRS(proj4string(NBNS))
                          out <- gIntersection(NBNS, CP, byid=TRUE)
                          
# Continue here Friday - working on the A and B polygons
                        
                          SUA = read.csv("polygon_AB22.csv")
                          polyAB = as.PolySet(SUA, projection="LL")
                          
                          #polyAB$PID  = as.factor(polyAB$PID)
                          
                          ggplot(tBA, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black', lwd=1)+  geom_polygon(data=polyAB,aes(x=X, y=Y, fill = PID),colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2, colour = "black")  + labs(x=NULL, y=NULL) + coord_map() + ggtitle ("Juvenile Acoustic Surveys - A & B ")
                          
                    ggplot(mBA, aes(x=Xend, y=Yend))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black', lwd=1) + geom_point(aes(colour = Vessel, size = PRC_ABC))+ ggtitle ("Juvenile Acoustic Survey - A & B") + coord_map()
                        

            polyA = polyAB[which(polyAB$PID == "2" ), ]
            polyB = polyAB[which(polyAB$PID == "1" ), ]
                        
                        calcArea(polyA) 
                        # 1.9
                        calcArea(polyB)
                        #1.8
                        
                        # Results for Survey A
                                resultsa = biomassCalc(x = tA, areaKm = 1.9)
                                unique(resultsa$total_biomass)
                            
                                      # Run results
                                      tableA = resultTableA(x = tA)
                                      tableB = resultTableB(x = tA)
                                      tableC = resultTableC(x = resultsa)
                                          tableC$Layer = "Survey A"
                            
                                          # Export Analysis Tables
                                          write.table(tableA, file= "SurveyA_tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                                          write.table(tableB, file= "SurveyA_tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                                          write.table(tableC, file= "SurveyA_tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                                          
                                          
                                          
                        
                        # Results for Survey B                        
                              resultsb = biomassCalc(x = tB, areaKm = 1.8)
                              unique(resultsb$total_biomass)
                            
                                    # Run results
                                    tableA = resultTableA(x = tB)
                                    tableB = resultTableB(x = tB)
                                    tableC = resultTableC(x = resultsb)
                                        tableC$Layer = "Survey B"
                        
                        # Export Analysis Tables
                        write.table(tableA, file= "SurveyB_tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                        write.table(tableB, file= "SurveyB_tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                        write.table(tableC, file= "SurveyB_tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                        
          
                        
                        
                        
                        
                        
              
# Surveys G and F
              
                        X = trans[which(trans$Yend > 44.2), ]
                        X1 = X [which(X$Yend < 44.4), ]
                        
                        tF = X1 [which(X1$area == "F"), ]
                        tG = X1 [which(X1$area == "G"), ]
                        
                        tFG = rbind(tF,tG)
                        
                        mF = map [which(map$area == "F"), ]
                        mG = map [which(map$area == "G"), ]
                        
                        mFG = rbind(mF, mG)
                        
                        # Proper coordinates for Grand Manan/SWNS
                        CP <- as(extent(-68.5, -66.6, 44, 44.5), "SpatialPolygons")
                        proj4string(CP) <- CRS(proj4string(NBNS))
                        out <- gIntersection(NBNS, CP, byid=TRUE)
                        
                        # Continue here Friday - working on the A and B polygons
                        
                        SUA = read.csv("polygon_FG22.csv")
                        polyFG = as.PolySet(SUA, projection="LL")
                        
                        
                        polyF = polyFG[which(polyFG$PID == "1" ), ]
                        polyG = polyFG[which(polyFG$PID == "2" ), ]
                        
                        
                        polyFG$PID  = as.factor(polyFG$PID)
                        
                        ggplot(tFG, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black', lwd=1)+  geom_polygon(data=polyFG,aes(x=X, y=Y, fill = PID),colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2, colour = "black")  + labs(x=NULL, y=NULL) + coord_map() + ggtitle ("Juvenile Acoustic Surveys - F & G ")
                        
                        ggplot(tF, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black', lwd=1)+  geom_polygon(data=polyF,aes(x=X, y=Y, fill = PID),colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2, colour = "black")  + labs(x=NULL, y=NULL) 
                        
                        ggplot(tG, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black', lwd=1)+  geom_polygon(data=polyG,aes(x=X, y=Y, fill = PID),colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2, colour = "black")  + labs(x=NULL, y=NULL) 
                        
                        
                        ggplot(mFG, aes(x=Xend, y=Yend))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black', lwd=1) + geom_point(aes(colour = Vessel, size = PRC_ABC))+ ggtitle ("Juvenile Acoustic Survey - F & G") + coord_map()
                        
  
                        
                        # Need to separate the polygons to determine area.                      
                        calcArea(polyF)
                        #1.8
                        
                        calcArea(polyG)
                        #6.5
                        
            
                        
                        
                        # Survey analysis for F
                          resultsa = biomassCalc(x = tF, areaKm = 1.8)
                          unique(resultsa$total_biomass)
                          
                              # Run results
                              tableA = resultTableA(x = tF)
                              tableB = resultTableB(x = tF)
                              tableC = resultTableC(x = resultsa)
                              
                                  tableC$Layer = "Survey F"
                        
                                        # Export Analysis Tables
                                        write.table(tableA, file= "SurveyF_tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                                        write.table(tableB, file= "SurveyF_tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                                        write.table(tableC, file= "SurveyF_tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                                        
          
                                        
                        # Survey analysis for G
                                        
                                        resultsG = biomassCalc(x = tG, areaKm = 6.5)
                                        unique(resultsG$total_biomass)
                                        
                                        # Run results
                                        tableA = resultTableA(x = tG)
                                        tableB = resultTableB(x = tG)
                                        tableC = resultTableC(x = resultsG)
                                        
                                        tableC$Layer = "Survey G"
                                        
                                        # Export Analysis Tables
                                        write.table(tableA, file= "SurveyG_tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                                        write.table(tableB, file= "SurveyG_tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                                        write.table(tableC, file= "SurveyG_tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                                        
          
          
          
          
          
          
          
          
# Survey E
                                        
                                        
        X = trans[which(trans$Yend < 44.2), ]
                                        
          mE = map [which(map$area == "E"), ]
                                        
             # Proper coordinates for Grand Manan/SWNS
                  CP <- as(extent(-68.5, -66.6, 44, 44.5), "SpatialPolygons")
                      proj4string(CP) <- CRS(proj4string(NBNS))
                          out <- gIntersection(NBNS, CP, byid=TRUE)
                                        
                                        
                                        SUA = read.csv("polygon_E22.csv")
                                        polyE = as.PolySet(SUA, projection="LL")
                                        
                                        polyE$PID  = as.factor(polyE$PID)
                                        
                                        ggplot(X, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black', lwd=1)+  geom_polygon(data=polyE,aes(x=X, y=Y, fill = PID),colour = "black")+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2, colour = "black")  + labs(x=NULL, y=NULL)   + coord_map() + ggtitle ("Juvenile Acoustic Survey - E ")
                                        
                                        ggplot(mE, aes(x=Xend, y=Yend))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black', lwd=1) + geom_point(aes(colour = Vessel, size = PRC_ABC))+ ggtitle ("Juvenile Acoustic Survey - E") + coord_map()
                                        
                                        
                                        
                                        # Need to separate the polygons to determine area.                      
                                        calcArea(polyE)
                                        #17
                                        
                                        # Survey analysis for E
                                        resultsa = biomassCalc(x = X, areaKm = 17)
                                        unique(resultsa$total_biomass)
                                        
                                        # Run results
                                        tableA = resultTableA(x = X)
                                        tableB = resultTableB(x = X)
                                        tableC = resultTableC(x = resultsa)
                                        
                                        tableC$Layer = "Survey E"
                                        
                                        # Export Analysis Tables
                                        write.table(tableA, file= "SurveyE_tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                                        write.table(tableB, file= "SurveyE_tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                                        write.table(tableC, file= "SurveyE_tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
                                        
                                        
                                        
                                        