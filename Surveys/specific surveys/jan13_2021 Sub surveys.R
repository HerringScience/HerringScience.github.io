

# Jan 13, 2021


# E                    


E = trans[which((trans$area == "E")), ]


Y = E

# Load land data


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




# I
I = trans[which((trans$area == "I")), ]


Y = I

# Load land data


CP <- as(extent(-67, -66, 44.4, 44.8), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)




SUA = read.csv("polygon_E.csv")
polyE = as.PolySet(SUA, projection="LL")

ggplot(I, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ggtitle("Survey I: No Fish Observed") 



# P
P = trans[which((trans$area == "P")), ]


Y = P

# Load land data


CP <- as(extent(-67, -66.5, 43.5, 44.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)




SUA = read.csv("polygon_P.csv")
polyP = as.PolySet(SUA, projection="LL")

ggplot(P, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ggtitle("Survey P") + geom_polygon(data=polyP,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) + coord_map() +ggtitle("Survey P") 


calcArea(polyP) 
# 11.6 km         


# QC
unique(Y$Transect_No)
unique(Y$area)

resultsa = biomassCalc(x = Y, areaKm = 11.6)
a = unique(resultsa$total_biomass)

# Run results
tableA = resultTableA(x = Y)
tableB = resultTableB(x = Y)
tableC = resultTableC(x = resultsa)

tableC$Layer = "Survey P"

# Scots
write.table(tableA, file= "PtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "PtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "PtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)


# L
L = trans[which((trans$area == "L")), ]


Y = L

# Load land data


CP <- as(extent(-67, -66.8, 44, 44.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)




SUA = read.csv("polygon_L.csv")
polyL = as.PolySet(SUA, projection="LL")

ggplot(L, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ggtitle("Survey L") + coord_map() + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1))  + geom_polygon(data=polyL,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) 


calcArea(polyL) 
# 1.0 km         


# QC
unique(Y$Transect_No)
unique(Y$area)

resultsa = biomassCalc(x = Y, areaKm = 1.0)
a = unique(resultsa$total_biomass)

# Run results
tableA = resultTableA(x = Y)
tableB = resultTableB(x = Y)
tableC = resultTableC(x = resultsa)

tableC$Layer = "Survey L"

# Scots
write.table(tableA, file= "LtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "LtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "LtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)


# Q
Q = trans[which((trans$area == "Q")), ]


Y = Q

# Load land data


CP <- as(extent(-67, -66.8, 44, 44.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)




SUA = read.csv("polygon_Q.csv")
polyQ = as.PolySet(SUA, projection="LL")

ggplot(Q, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ggtitle("Survey Q") + coord_map() + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1))  + geom_polygon(data=polyQ,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) 


calcArea(polyQ) 
# 0.6 km         


# QC
unique(Y$Transect_No)
unique(Y$area)

resultsa = biomassCalc(x = Y, areaKm = 0.6)
a = unique(resultsa$total_biomass)

# Run results
tableA = resultTableA(x = Y)
tableB = resultTableB(x = Y)
tableC = resultTableC(x = resultsa)

tableC$Layer = "Survey Q"

# Scots
write.table(tableA, file= "QtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "QtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "QtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)


# V
V = trans[which((trans$area == "V")), ]


Y = V

# Load land data


CP <- as(extent(-66.85, -66.5, 44.9, 45), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)




SUA = read.csv("polygon_Q.csv")
polyQ = as.PolySet(SUA, projection="LL")

ggplot(V, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='black',col='black') + coord_map()+ geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)+ggtitle("Survey V") + coord_map() + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1))  


+ geom_polygon(data=polyQ,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend =Xend, yend = Yend, colour = Vessel), size = 1.2)  + labs(x=NULL, y=NULL) 


calcArea(polyQ) 
# 0.6 km         


# QC
unique(Y$Transect_No)
unique(Y$area)

resultsa = biomassCalc(x = Y, areaKm = 0.6)
a = unique(resultsa$total_biomass)

# Run results
tableA = resultTableA(x = Y)
tableB = resultTableB(x = Y)
tableC = resultTableC(x = resultsa)

tableC$Layer = "Survey Q"

# Scots
write.table(tableA, file= "QtableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "QtableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "QtableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)                                        