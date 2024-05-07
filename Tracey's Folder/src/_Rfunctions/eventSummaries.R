

eventSummaries = function(x) {
  
  
  can<-getData('GADM', country="CAN", level=1) # provinces
  NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
  
          # don't include eastern boxes or trinity/coastal NB - better for tagging events
          CP <- as(extent(-67.6, -64.5, 43, 45.8), "SpatialPolygons")
          proj4string(CP) <- CRS(proj4string(NBNS))
          out <- gIntersection(NBNS, CP, byid=TRUE)
          
          # don't include eastern boxes (shelburne and the patch) trinity
          boxes = read.csv("catchBoxes_.csv")
          
          years<-aggregate(no_tags~Year, x, FUN=sum)
          grounds<-aggregate(no_tags~RELEASE_LOCATION, x, FUN=sum)
          months<-aggregate(no_tags~month, x, FUN=sum)
          vessels<-aggregate(no_tags~RELEASE_VESSEL, x, FUN=sum)
          
          
          write.table(years, file= "years.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
          write.table(grounds, file= "grounds.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
          write.table(months, file= "months.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
          write.table(vessels, file= "vessels.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
          
          
          # Figure 2. Tagging application sites categorized with specific tagging locations 
          
          alltaggingEvents = ggplot(x, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box, fill = Box), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_point(pch=21, size = 2, fill = "White") + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey82"))
          
          ggsave("alltaggingEvents.png")
          
          
          
          
          # Figure 3. Tagging application categorized sites and distribution of tags 
          area = with(x, tapply(no_tags, list(RELEASE_LOCATION), sum))
          area = as.data.frame(area)
          m = rownames(area)
          area$RELEASE_LOCATION  =  m
          
          areaGPS = read.csv("circlePlotGPS.csv")
          circle = merge(area, areaGPS, by = "RELEASE_LOCATION")
          
          
          circlePlot = ggplot(circle, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box, fill = Box), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_point(aes(size = area), colour = "white") + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey82"))
          
          ggsave("circlePlot.png")
          
          
          
          
          # Figure 4. Temporal coverage of tagging application between years
          # Julian day versus year
          temporalCoverage = ggplot(x, aes(x=Julian, y=Year)) + geom_point(size = 4,pch = 22) + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))  
          
                                                                           
          ggsave("temporalCoverage.png")
          
          
          
          
          # Figure 5. Tagging application sites by month    
          
          monthMap =  ggplot(x, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_point(aes(colour = month)) + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey82"))
          
          ggsave("monthMap.png")
          
          
          
          
          
          # Figure 6. Tagging application sites by year     
          
          yearMap = ggplot(x, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group)) + geom_point(aes(colour = Year)) + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey82"))
          
          ggsave("yearMap.png")
          
          
          
}
