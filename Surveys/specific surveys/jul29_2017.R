
# Scots Bay

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]

# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
# Northern box 
CP <- as(extent(-65.25, -64.75, 45.25, 45.4), "SpatialPolygons")
# Look closer at main box
CP <- as(extent(-65.3, -64.55, 45, 45.3), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)


# july 29, 2017 Scots Bay
regions = read.table("RegionJul29.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("MapJul29.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -35.051, TS50 = -35.158)

#Target Strength (38kHz) =	-35.051, Target Strength (50kHz) =	-35.158




# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jul29_2017"), ]
# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

map = mapDat(x = mapping)



# Remove BP  and for Jul 29, 2017
trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]
northern=trans_survey[which(trans_survey$Vessel == "BP"), ]

resultsa = biomassCalc(x = trans_survey1, areaKm = 668.44)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 69.86)
unique(resultsb$total_biomass)

# Load polygons

# Main
SUA = read.csv("polygon_SB_jul15.csv")
polySB_main = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthernJul29.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

# Determine the ordering of polygon points
ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))


# Scot's Bay
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))+ geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + coord_map() + labs(x=NULL, y=NULL) 

# plankton tow
ggplot(plank,aes(x=x1, y=y1)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), colour = "black", fill="white",linetype = 3)+ geom_point(color = "red", size = 3) + geom_point(data=CTD1,aes(x=x2, y=y2), color = "blue", size = 3, pch = 3) + geom_point(data=CTD2,aes(x=x3, y=y3), color = "blue", size = 3, pch = 3) + ggtitle("Plankton Tow and CTD Locations") + coord_map() + labs(x=NULL, y=NULL)



x1 = -64.9216
y1 = 45.1943
plank = as.data.frame(c(x1,y1))

x2 = -65.2798
y2 = 45.08343333
CTD1 = as.data.frame(c(x2,y2))

x3 = -64.9216
y3 = 45.1943
CTD2 = as.data.frame(c(x3,y3))

##############

# just northern box

# Northern    
ggplot(BP,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) 

# Determine the ordering of polygon points
ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

BP=map[which(map$Vessel == "BP"), ]

ggplot(BP, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL)

# just main
main=map[which(map$Vessel != "BP"), ]

ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))


SUA = read.csv("polygon_SB_jul29.csv")
polySB_main = as.PolySet(SUA, projection="LL")

ggplot(main,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + coord_map() + labs(x=NULL, y=NULL) 

calcArea(polySB_northern) 
calcArea(polySB_main) 

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC, line.type = Vessel)) + labs(x=NULL, y=NULL)






# Figure for report
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))+  geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -64.82, y = 45.1, label = "Main box = 673.24 km2" )+ annotate("text", x = -65.15, y = 45.32, label = "Northern box = 71.83 km2" ) + coord_map() + labs(x=NULL, y=NULL) 


calcArea(polySB_main)
# - 1.61km2 (Ile Haute)"


#   
gglocator()
()

calcArea(polySB_main) 
calcArea(GBpoly) 

ggplot(test, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC), pch = 5) + labs(x=NULL, y=NULL) + coord_map()    

head(map)
ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

