
# August 18, 2021
# Ad hoc survey for juvenile fish; LB and MS Long Island Shore
# 5 transects by the MS, GPS wasn't working so log used
# 12 transects by the LB
# Areas overlap so the larger biomass to be selected
# TS calculated using the fat data averge length and weight from Long Island Shore

# Spoke with Gary and have to distance weight the LB trasnects 3-12. Add up the distance of each transect, then add together. Turn the backscatter for each transect into empirical, than multiple by the distance factor (Transect distance/total distance). Then convert the empircal SA into ABS again and proceed with normal calculation.



# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Coordinates
CP <- as(extent(-66.75, -66.25, 44, 44.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


# August 18, 2021 Long Island shore
regions = read.table("Region_Aug18_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Only contains LB data
mapping = read.table("Map_Aug18_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

                map = mapDat(x = mapping)
                
                x = regions

# Need to calculate target strength from the data that Allan D sent.
# No longer need 50  - TM has EK80

                
                # use the TS from the samples from multiple sources
                # Same as August 19
                trans = transects(x= regions, TS38 = -33.453 , TS50 = NA )
                
                
#trans = transects(x= regions, TS38 = -34.76 , TS50 = NA )


# Load all the custom polygons: 

#Polygon for T1-3
SUA = read.csv("polygon_MS.csv")
polySB_MS1 = as.PolySet(SUA, projection="LL")
        #Polygon for T4/5
        SUA = read.csv("polySB_MS2.csv")
        polySB_MS2 = as.PolySet(SUA, projection="LL")
                 # Polygon for T1/T2
                SUA = read.csv("polygon_LB1.csv")
                polySB_LB1 = as.PolySet(SUA, projection="LL")
                        # This is the polygon for T3-12 LB
                        SUA = read.csv("polygon_Aug18LB.csv")
                        polySB_LB3 = as.PolySet(SUA, projection="LL")
                        

# Plot transects and polygons
ggplot(trans, aes(x=X, y=Y))+ geom_polygon(data = polySB_MS2, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = polySB_LB1, aes(x=X, y=Y), fill = "lavenderblush2", colour = "black")+ geom_polygon(data = polySB_LB3, aes(x=X, y=Y), fill = "lavenderblush2", colour = "black") + geom_polygon(data = polySB_MS1, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77") + annotate("text", x = -66.295, y = 44.3, label = "Long Island, NS", size = 6, colour = "navyblue")


# PRC ABC for LB
ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug18_2021"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)

# Separate the data by how they will be analyzed in terms of polygons.

# LB (Both polys)
trans_surveyLB=trans_survey[which(trans_survey$Vessel != "MS"), ]

# LB DataSets
ids = c("T01", "T02")

trans_surveyLB1=trans_surveyLB[which(trans_surveyLB$Transect_No == ids), ]

# This df needs to be distance weighted:
trans_surveyLB2=trans_surveyLB[which(trans_surveyLB$Transect_No != ids), ]

trans_surveyLB2_W = distanceWeighting(x = trans_surveyLB2)

write.table(trans_surveyLB2_W, file= "distanceWeighted.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)


# MS (Both polys)
trans_surveyMS=trans_survey[which(trans_survey$Vessel == "MS"), ]

ids = c("T01", "T02", "T03")

trans_surveyMS1=trans_surveyMS[which(trans_surveyMS$Transect_No == ids), ]
trans_surveyMS2=trans_surveyMS[which(trans_surveyMS$Transect_No != ids), ]
# errors but still works?




calcArea(polySB_MS1)
#7.5
calcArea(polySB_MS2)
#24
calcArea(polySB_LB1)
#2.2

# This survey needs to be distance weighted
calcArea(polySB_LB3)
#32.4


# Results
resultsa = weightedBiomassCalc(x = trans_surveyMS1, areaKm = 7.5)
unique(resultsa$total_biomass)

# 11,057.4

resultsb = weightedBiomassCalc(x = trans_surveyMS2, areaKm = 24)
unique(resultsb$total_biomass)

# 8,456.17

resultsc = weightedBiomassCalc(x = trans_surveyLB1, areaKm = 2.2)
unique(resultsc$total_biomass)

#427 t

resultsd = weightedBiomassCalc(x = trans_surveyLB2, areaKm = 32.4)
unique(resultsd$total_biomass)

#29,348

# Run results
tableA = resultTableA(x = trans_surveyMS1)
tableB = resultTableB(x = trans_surveyMS1)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = trans_surveyMS2)
tableE = resultTableB(x = trans_surveyMS2)
tableF = resultTableC(x = resultsb)

tableG = resultTableA(x = trans_surveyLB1)
tableH = resultTableB(x = trans_surveyLB1)
tableI = resultTableC(x = resultsc)

tableJ = resultTableA(x = trans_surveyLB2)
tableK = resultTableB(x = trans_surveyLB2)
tableL = resultTableC(x = resultsd)



tableC$Layer = "MSBox1"
tableF$Layer = "MSBox2"
tableI$Layer = "LBBox1"
tableL$Layer = "LBBox2"

A = rbind(tableA, tableD, tableG, tableJ)
B = rbind(tableB, tableE, tableH, tableK)
C = rbind(tableC, tableF, tableI, tableL)

write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)





tableG = resultTableA(x = eastern)
tableH = resultTableB(x = eastern)
tableI = resultTableC(x = resultsc)

        tableC$Layer = "Main Box"
        tableI$Layer = "Eastern Box"
        
        A = rbind(tableA,tableG)
        B = rbind(tableB,tableH)
        C = rbind(tableC,tableI)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
S        
        


# Development ----
# Visulization of MS transects - 
ggplot(trans_surveyMS, aes(x=X, y=Y))+ geom_polygon(data = polySB_MS1, aes(x=X, y=Y), fill = "white")+ geom_polygon(data = polySB_MS2, aes(x=X, y=Y), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77") + geom_label(label = trans_surveyMS$RegionName)

ggplot(trans_surveyMS1, aes(x=X, y=Y))+ geom_polygon(data = polySB_MS1, aes(x=X, y=Y), fill = "white") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77") + geom_label(label = trans_surveyMS1$RegionName)

# Visualization of LB Transects
ggplot(trans_surveyLB, aes(x=X, y=Y))+ geom_polygon(data = polySB_LB1, aes(x=X, y=Y), fill = "white") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77") 

ggplot(trans_surveyLB, aes(x=X, y=Y))+ geom_polygon(data = polySB_LB1, aes(x=X, y=Y), fill = "white")+ geom_polygon(data = polySB_LB3, aes(x=X, y=Y), fill = "white") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77") 

# Trying to figure out what line is which? The first method didn't work well.
ggplot(trans_surveyLB2, aes(x=X, y=Y)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() +geom_text(label = trans_surveyLB2$Y, check_overlap = TRUE, size = 3)+geom_text(label = trans_surveyLB2$Yend, check_overlap = TRUE, size = 3)

# This was a better method. Plot each point indivudally then identify using id
Points = read.csv("pointsAug18.csv")

ggplot(Points, aes(x=X, y=Y)) + geom_point() +geom_text(label = Points$id, size = 6, colour = "Red")


        