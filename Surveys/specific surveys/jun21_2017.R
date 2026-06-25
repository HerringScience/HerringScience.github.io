

# june 21, 2017 Scots Bay
regions = read.table("regionJune21_2017.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

mapping = read.table("mappingJune21_2017.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

trans = transects(x= regions, TS38 =   -34.822, TS50 = -34.929)


# Specify the survey you are doing an analysis for  
trans_survey= trans[which(trans$Survey_date == "Jun21_2017"), ]

# QC
unique(trans_survey$Survey_date)
unique(trans_survey$Vessel)

resultsa = biomassCalc(x = trans_survey, areaKm = 626.84)
unique(resultsa$total_biomass)

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

# Scot's Bay
ggplot(map,aes(x=X, y=Y)) + geom_polygon(data=out,aes(x=long, y=lat, group=group)) +  geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID)) + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + ggtitle("Survey Polygon") + annotate("text", x = -65.22, y = 45.3, label = "Polygon Area = 655.19 km2 - 1.61km2 (Ile Haute)") + coord_map() + labs(x=NULL, y=NULL) 

()

calcArea(polySB_main) 
calcArea(GBpoly) 


head(map)
ggplot(polySB_main, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))

head(map_data)

+ scaleBar(lon = -65.5, lat = 45.1, distanceLon = 4, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km")  + labs(x=NULL, y=NULL)  + scaleBar(lon = -65.5, lat = 45.1, distanceLon = 4, distanceLat = 2, distanceLegend = -1.5, dist.unit = "km") + labs(x=NULL, y=NULL)+ coord_map()
                                                                                                  
                                                                                                  d = -1.5, dist.unit = "km") + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID)) + geom_polygon(data=polySB_eastern,aes(x=X, y=Y, group=PID))+ geom_polygon(data=polySB_northern,aes(x=X, y=Y, group=PID))