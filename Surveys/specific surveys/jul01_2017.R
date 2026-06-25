
# Main Box
# main box polygon is polygon_SB_jul01_2017.csv in workspace
# july 01, 2017 Scots Bay
regions = read.table("RegionJul01.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("Map_Jul01_2017.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 = -35.5, TS50 = -35.609)

# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jul01_2017"), ]
# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

# Remove BP and for Jul 01, 2017
trans_survey1=trans_survey[which(trans_survey$Vessel != "BP"), ]
northern=trans_survey[which(trans_survey$Vessel == "BP"), ]

resultsa = biomassCalc(x = trans_survey1, areaKm = 664.42)
unique(resultsa$total_biomass)

resultsb = biomassCalc(x = northern, areaKm = 79.75)
unique(resultsb$total_biomass)

# Determine the ordering of polygon points
ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

SUA = read.csv("polygon_SB_jul01_2017.csv")
polySB_main = as.PolySet(SUA, projection="LL")

# Scot's Bay
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID))+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -65.22, y = 45.3, label = "Polygon Area = 677.38 km2 - 1.61km2 (Ile Haute)") + coord_map() + labs(x=NULL, y=NULL) 



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