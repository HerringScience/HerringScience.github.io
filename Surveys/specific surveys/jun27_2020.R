
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")



# Load land data
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


SUA = read.csv("polygon_SB5.csv")
polySB_main = as.PolySet(SUA, projection="LL")
ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))


SUA = read.csv("polygon_SBNorthern27.csv")
polySB_northern = as.PolySet(SUA, projection="LL")
##polygon_SBNorthern2

ggplot(trans, aes(x=X, y=Y)) + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)+ labs(x=NULL, y=NULL) + coord_map() + theme_dark() 

ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))





# Survey analysis

# June 27, 2020 Scots Bay
regions = read.table("RegionJun27_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapJun27_2020.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

x = regions
head(x)



trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )
trans = transects(x= regions, TS38 =-35.5 , TS50 = -35.607 )


x = surveyTrack(x=trans, polyNameA  = polySB_main, polyNameB  = polySB_northern, polyNameC = polySB_eastern, title = name )


ggplot(trans, aes(x=X, y=Y))+ geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID), fill = "white", colour = "black") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1)+ labs(x=NULL, y=NULL) + coord_map() + theme_dark() 



ids = c("BP")
mastersub = trans[which((trans$Vessel %in% ids)), ]


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jun27_2020"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# remove FM
trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]

# Northern
northern=trans_survey[which(trans_survey$Vessel == "BP"), ]


area = calcArea(polySB_main) 
area - 1.61

calcArea(polySB_northern)

# Results
resultsa = biomassCalc(x = trans_survey1, areaKm = 640.7993)
a = unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 83.27)
b = unique(resultsb$total_biomass)


a+b

# Run results
tableA = resultTableA(x = trans_survey1)
tableB = resultTableB(x = trans_survey1)
tableC = resultTableC(x = resultsa)

tableD = resultTableA(x = northern)
tableE = resultTableB(x = northern)
tableF = resultTableC(x = resultsb)


        tableC$Layer = "Main Box"
        tableF$Layer = "Northern Box"
        
        A = rbind(tableA,tableD)
        B = rbind(tableB,tableE)
        C = rbind(tableC,tableF)
        
        # Scots
        write.table(A, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
        write.table(B, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        write.table(C, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
        
        
        