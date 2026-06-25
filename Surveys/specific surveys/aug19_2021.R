
# August 19, 2021

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Coordinates
CP <- as(extent(-67.5, -66.25, 44, 45), "SpatialPolygons")

# northern lines
CP <- as(extent(-66.8, -66.7, 44.75, 44.9), "SpatialPolygons")

# only BP adhoc
CP <- as(extent(-66.8, -66.7, 44.7, 44.8), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


# August 19, 2021 NB
regions = read.table("Region_Aug19_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# Only contains LB data
mapping = read.table("Map_Aug19_2021.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

                map = mapDat(x = mapping)
                
                x = regions

# Need to calculate target strength from the data that Allan D sent.
# No longer need 50  - TM has EK80
                
# based on samples from different sources in the .csv samplesMixedsource        
                
                trans = transects(x= regions, TS38 = -33.453 , TS50 = NA )


# based on 222mm and 0.073
#trans = transects(x= regions, TS38 = -33.604 , TS50 = NA )


#trans = transects(x= regions, TS38 = -35.144 , TS50 = NA )





# Load all the custom polygons: 

#Polygon for T1-3
        SUA = read.csv("juvenileBoxes_results.csv")
        boxes = as.PolySet(SUA, projection="LL")
        theBank=boxes[which(boxes$Box == "TheBank"), ]
        #whiteHead=boxes[which(boxes$Box == "WhiteHead"), ]
        prongBank=boxes[which(boxes$Box == "ProngBank"), ]
        prong=boxes[which(boxes$Box == "Prong"), ]
        off=boxes[which(boxes$Box == "offWH"), ]
        wolves=boxes[which(boxes$Box == "wolves"), ]
        fish = boxes[which(boxes$Box == "fishCove"), ]
        swall = boxes[which(boxes$Box == "swallow"), ]
        North = boxes[which(boxes$Box == "north"), ]
        
        add = rbind(wolves, fish, swall, North)
        
        
        # focus on the BP ashoc area
        ggplot(adHoc, aes(x=X, y=Y))+ geom_polygon(data = add, aes(x=X, y=Y, group = PID, fill = Box), colour = "black") + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), colour = "white", size = 0.8)

        
        #+ geom_text(label = trans_surveyBP2$Transect_No, check_overlap = TRUE, size = 3)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug19_2021"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)
unique(trans_survey$Transect_No)




# Separate the data by how they will be analyzed in terms of polygons.

# The Bank (south)
trans_surveyTM=trans_survey[which(trans_survey$Vessel == "TM"), ]
trans_surveyLM=trans_survey[which(trans_survey$Vessel == "LM"), ]

trans_surveyTheBank = rbind(trans_surveyLM, trans_surveyTM)


# The Prong
trans_surveySL=trans_survey[which(trans_survey$Vessel == "SL"), ]
trans_surveyLJ=trans_survey[which(trans_survey$Vessel == "LJ"), ]

trans_surveyTheProng = rbind(trans_surveySL, trans_surveyLJ)

# ProngBank
trans_surveyLB=trans_survey[which(trans_survey$Vessel == "LB"), ]
trans_surveyMS=trans_survey[which(trans_survey$Vessel == "MS"), ]

trans_surveyProngBank = rbind(trans_surveyLB, trans_surveyMS)

# offWhiteHead
trans_surveyBP=trans_survey[which(trans_survey$Vessel == "BP"), ]
ids = c("T01", "T02", "T03", "T04", "T05", "T06", "T07", "T08")
trans_surveyBP1=trans_surveyBP[which(trans_surveyBP$Transect_No == ids), ]

trans_surveyBP2 = trans_surveyBP[which(trans_surveyBP$Transect_No != ids), ]


trans_surveyFM=trans_survey[which(trans_survey$Vessel == "FM"), ]
trans_surveyFM1=trans_surveyFM[which(trans_surveyFM$Y < 44.75), ]

trans_surveyFM2=trans_surveyFM[which(trans_surveyFM$Y > 44.75), ]


trans_surveyoffWH = rbind(trans_surveyBP1, trans_surveyFM1)
adHoc = rbind(trans_surveyBP2, trans_surveyFM2)


# Need to separate BP2 into the three boxes:
trans_surveyBP2$Transect_No

ids = c("T09", "T10", "T11")
trans_surveyBPfishCove = trans_surveyBP2[which(trans_surveyBP2$Transect_No == ids), ]

trans_surveyBPswallow = trans_surveyBP2[which(trans_surveyBP2$Dist_T > 2), ]


a =trans_surveyBP2[6, ]
b =trans_surveyBP2[7, ]
c =trans_surveyBP2[8, ]

trans_surveyBPnorthHead = rbind(a,b,c)



# Plot transects and polygons ADHOC
ggplot(adHoc, aes(x=X, y=Y))+ geom_polygon(data = wolves, aes(x=X, y=Y), fill = "azure2", colour = "black")+ labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) 


# Plot transects and polygons
ggplot(trans, aes(x=X, y=Y))+ geom_polygon(data = theBank, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = off, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = prong, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = prongBank, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = wolves, aes(x=X, y=Y), fill = "azure2", colour = "black") + labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) 



ggplot(trans, aes(x=X, y=Y))+ geom_polygon(data = boxes, aes(x=X, y=Y, group = PID, fill = Box), colour = "black")+ labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend), size = 0.5) 


+ geom_polygon(data = theBank, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = off, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = prong, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = prongBank, aes(x=X, y=Y), fill = "azure2", colour = "black") 


# Plot transects and polygons
ggplot(trans, aes(x=X, y=Y))+ geom_polygon(data = theBank, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = off, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = prong, aes(x=X, y=Y), fill = "azure2", colour = "black")+ geom_polygon(data = prongBank, aes(x=X, y=Y), fill = "azure2", colour = "black") + labs(x=NULL, y=NULL) + coord_map() + theme_dark() + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) 


+ annotate("text", x = -66.295, y = 44.3, label = "Long Island, NS", size = 6, colour = "navyblue")
        
        
        
        + geom_polygon(data = polySB_LB1, aes(x=X, y=Y), fill = "lavenderblush2", colour = "black")+ geom_polygon(data = polySB_LB3, aes(x=X, y=Y), fill = "lavenderblush2", colour = "black") + geom_polygon(data = polySB_MS1, aes(x=X, y=Y), fill = "azure2", colour = "black")


# PRC ABC for LB
ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()



calcArea(off)
#141.5
calcArea(theBank)
#217
calcArea(prong)
#117
calcArea(prongBank)
#118.6
calcArea(wolves)
#5.05



# Results

resultsa = weightedBiomassCalc(x = trans_surveyoffWH, areaKm = 141.5)
unique(resultsa$total_biomass)
# 51,026.32

resultsb = weightedBiomassCalc(x = trans_surveyTheBank, areaKm = 217)
unique(resultsb$total_biomass)
# 6,169.66

resultsc = weightedBiomassCalc(x = trans_surveyTheProng, areaKm = 117)
unique(resultsc$total_biomass)
# 18,462.93

resultsd = weightedBiomassCalc(x = trans_surveyProngBank, areaKm = 118.6)
unique(resultsd$total_biomass)
# 3,418.82

resultse = weightedBiomassCalc(x = trans_surveyFM2, areaKm = 5)
unique(resultse$total_biomass)
# 2,982.8

calcArea(fish)
#2.1
calcArea(swall)
#0.6
calcArea(North)
#1.6

#fishCove
resultsf = weightedBiomassCalc(x = trans_surveyBPfishCove, areaKm = 2.1)
unique(resultsf$total_biomass)

#Swallow
resultsg = weightedBiomassCalc(x = trans_surveyBPswallow, areaKm = 0.6)
unique(resultsg$total_biomass)

# North Head
resultsh = weightedBiomassCalc(x = trans_surveyBPnorthHead, areaKm = 1.6)
unique(resultsh$total_biomass)

# Run results
tableA = resultTableA(x = trans_surveyoffWH)
tableB = resultTableB(x = trans_surveyoffWH)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = trans_surveyTheBank)
tableE = resultTableB(x = trans_surveyTheBank)
tableF = resultTableC(x = resultsb)

tableG = resultTableA(x = trans_surveyTheProng)
tableH = resultTableB(x = trans_surveyTheProng)
tableI = resultTableC(x = resultsc)

tableJ = resultTableA(x = trans_surveyProngBank)
tableK = resultTableB(x = trans_surveyProngBank)
tableL = resultTableC(x = resultsd)

tableM = resultTableA(x = trans_surveyFM2)
tableN = resultTableB(x = trans_surveyFM2)
tableO = resultTableC(x = resultse)

tableP = resultTableA(x = trans_surveyBPfishCove)
tableQ = resultTableB(x = trans_surveyBPfishCove)
tableR = resultTableC(x = resultsf)

tableS = resultTableA(x = trans_surveyBPswallow)
tableT = resultTableB(x = trans_surveyBPswallow)
tableU = resultTableC(x = resultsg)

tableV = resultTableA(x = trans_surveyBPnorthHead)
tableW = resultTableB(x = trans_surveyBPnorthHead)
tableX = resultTableC(x = resultsh)


tableC$Layer = "Off White Head"
tableF$Layer = "The Bank"
tableI$Layer = "The Prong"
tableL$Layer = "Prong Bank"
tableO$Layer = "Wolves"
tableR$Layer = "Fish Cove"
tableU$Layer = "Swallow"
tableX$Layer = "North Head"


A = rbind(tableA, tableD, tableG, tableJ, tableM, tableP, tableS, tableV)
B = rbind(tableB, tableE, tableH, tableK, tableN, tableQ, tableT, tableW)
C = rbind(tableC, tableF, tableI, tableL, tableO, tableR, tableU, tableX)

write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)





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


        