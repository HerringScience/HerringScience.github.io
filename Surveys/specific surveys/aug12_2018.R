

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

# Aug 12, 2018 German Bank
regions = read.table("Region_aug12_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
  # test 
    regions = read.table("Region_test.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)


    
    
mapping = read.table("Map_aug12_2018.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

# test
  mapping = read.table("Map_test.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

map = mapDat(x = mapping)

SUA = read.csv("towArea.csv")
tow = as.PolySet(SUA, projection="LL")

ggplot(map, aes(x=Xend, y=Yend)) +geom_polygon(data=tow,aes(x=X, y=Y, group=PID), fill = "grey")+ geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()

trans = transects(x= regions, TS38 = -35.5 , TS50 = -35.609 )




# German Bank
SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")


SUA = read.csv("CHP_polygon.csv")
CHP = as.PolySet(SUA, projection="LL")


x = surveyTrack(x=trans, polyNameA  = polyGB, polyNameB = tow, polyNameC = NULL)

# CHP polygon



ggplot(CHP, aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='tan4',col='black') + geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID), fill = "white", colour = "black")  + geom_polygon(fill = 'blue')  + labs(x=NULL, y=NULL) + coord_map() + theme_dark() 

ggplot(data=CHP, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

ggplot(data=polyGB, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=id))

calcArea(polyGB) 

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Aug12_2018"), ]

trans_survey = trans

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


calcArea(polyGB) 
resultsa = biomassCalc(x = trans_survey, areaKm =866.46)
unique(resultsa$total_biomass)

map = mapDat(x = mapping)


#892.91


# Main

# Determine the ordering of polygon points
ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))





# plankton tow

# CTD/plankton position
x2 = c(-66.35, -66.35965, -66.3618)
y2 = c(43.55, 43.53333333, 43.5491)
type = c(1,1,2)
type = as.factor(type)
plank1 = data.frame(x2,y2, type)

ggplot(plank1,aes(x=x2, y=y2)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3) + geom_point(aes(colour = type), size = 3) + coord_map() + labs(x=NULL, y=NULL) + geom_line(aes(group = type), size = 1)



ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL)


# Run results
tableA = resultTableA(x = trans_survey)
tableB = resultTableB(x = trans_survey)
tableC = resultTableC(x = resultsa)

# Scots
write.table(tableA, file= "tableA.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(tableB, file= "tableB.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(tableC, file= "tableC.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)






