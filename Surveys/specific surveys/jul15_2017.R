
# Scots Bay


# july 15, 2017 Scots Bay
regions = read.table("RegionJul15.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_jul15.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -35.5, TS50 = -35.609)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jul15_2017"), ]
# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)


# For regions
trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]
northern=trans_survey[which(trans_survey$Vessel == "C1"), ]
eastern=trans_survey[which(trans_survey$Vessel == "BP"), ]
main=trans_survey1[which(trans_survey1$Vessel != "C1"), ]

resultsa = biomassCalc(x = main, areaKm = 646.61)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 80.31)
unique(resultsb$total_biomass)

resultsc = biomassCalc(x = eastern, areaKm = 131.12)
unique(resultsc$total_biomass)


# Load polygons

# Main
SUA = read.csv("polygon_SB_jul15.csv")
polySB_main = as.PolySet(SUA, projection="LL")
# Eastern
SUA = read.csv("polygon_SBEastern.csv")
polySB_eastern = as.PolySet(SUA, projection="LL")
# Northern    
SUA = read.csv("polygon_SBNorthernJul15.csv")
polySB_northern = as.PolySet(SUA, projection="LL")


# Remove BP  and for Jul 29, 2017
trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]
trans_survey2=trans_survey1[which(trans_survey1$Vessel != "C1"), ]

northern=trans_survey[which(trans_survey$Vessel == "C1"), ]
s=trans_survey[which(trans_survey$Vessel == "BP"), ]

# Determine the ordering of polygon points
ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

# Main
SUA = read.csv("polygon_SB_jul15.csv")
polySB_main = as.PolySet(SUA, projection="LL")

# Scot's Bay
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))+ geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID)) + geom_polygon(data=polySB_eastern,aes(x=X, y=Y, group=PID))  + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + coord_map() + labs(x=NULL, y=NULL) 

# plankton tow
ggplot(data,aes(x=x, y=y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +   geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID), fill = "blue")+ geom_point(color = "red") + ggtitle("Plankton Tow Location") + coord_map() + labs(x=NULL, y=NULL)



x = -65.2667
y = 45.0945833
data = as.data.frame(c(x,y))


##############



# Northern Box work
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]


# Proper coordinates for just northern box
CP <- as(extent(-65.25, -64.75, 45.2, 45.4), "SpatialPolygons")

proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

SUA = read.csv("polygon_SBNorthernJul012017.csv")
polySB_northern = as.PolySet(SUA, projection="LL")



# Determine the ordering of polygon points
ggplot(polySB_northern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

SUA = read.csv("polygon_SBNorthernJul012017.csv")
polySB_northern = as.PolySet(SUA, projection="LL")

# just northern box
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) 

calcArea(polySB_northern) 
calcArea(polySB_main) 
calcArea(polySB_eastern) 


ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL)






# Figure for report
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))+  geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -64.82, y = 45.1, label = "Main box = 673.24 km2" )+ annotate("text", x = -65.15, y = 45.32, label = "Northern box = 71.83 km2" ) + coord_map() + labs(x=NULL, y=NULL) 


calcArea(polySB_main)
# - 1.61km2 (Ile Haute)"


#   
gglocator()
()

calcArea(polySB_main) 
calcArea(GBpoly) 

ggplot(map, aes(x=Xend, y=Yend)) + geom_point(aes(colour = Vessel, size = PRC_ABC)) + labs(x=NULL, y=NULL) + coord_map()    

head(map)
ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

head(map_data)

+ scaleBar(lon = -65.5, lat = 45.1, distanceLon = 4, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km")  + labs(x=NULL, y=NULL)  + scaleBar(lon = -65.5, lat = 45.1, distanceLon = 4, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + labs(x=NULL, y=NULL)+ coord_map()
                                                                                                  
                                                                                                  d = -1.5, dist.unit = "km") + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID)) + geom_polygon(data=polySB_eastern,aes(x=X, y=Y, group=PID))+ geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID))
