rm(list=ls())


# Run first few lines of taggingMaster first to load in relINFO

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/"))

relINFO <- read.csv("relINFO.csv")

head(relINFO)
# Years

two16=relINFO[which(relINFO$Year == "2016"), ]
two17=relINFO[which(relINFO$Year == "2017"), ]
two18=relINFO[which(relINFO$Year == "2018"), ]
two19=relINFO[which(relINFO$Year == "2019"), ]
two20=relINFO[which(relINFO$Year == "2020"), ]
two21=relINFO[which(relINFO$Year == "2021"), ]
# two22=relINFO[which(relINFO$Year == "2022"), ]
# two23=relINFO[which(relINFO$Year == "2023"), ]

# Tenporal range of tagging season

unique(two16$Julian)
unique(two17$Julian)
unique(two18$Julian)
unique(two19$Julian)
unique(two20$Julian)
unique(two21$Julian)
# unique(two22$Julian)
# unique(two23$Julian)


#2021

head(two21)
sum(two21$no_tags)

sum(two21$no_tags)


unique(two21$RELEASE_VESSEL)



# 2019
    two19
    head(two19)
    sum(two19$no_tags)
    unique(two19$RELEASE_VESSEL)

# Nicole Seamone
          nicole=two20[which(two20$RELEASE_VESSEL == "Canada 100"), ]
          sum(nicole$no_tags)
          
          head(nicole)
          
          dim(nicole)


# Lisa Houston
          lisaA=two20[which(two20$RELEASE_VESSEL == "Morning Star"), ]
          lisaB=two19[which(two19$RELEASE_VESSEL == "Leroy and Barry"), ]

          lisa = rbind(lisaA, lisaB)
          sum(lisa$no_tags)
          
          dim(lisa)

          lisa=two20[which(two20$RELEASE_VESSEL == "Sealife II"), ]
          sum(lisa$no_tags)
          
          
          # 2021
          lisa=two21[which(two21$RELEASE_VESSEL == "Morning Star"), ]
          sum(lisa$no_tags)
          
          
# Manon Holmes          
          
          manonA=two19[which(two19$RELEASE_VESSEL == "Lady Melissa"), ]
            manonB=two19[which(two19$RELEASE_VESSEL == "Fundy Monarch"), ]
            
            manon = rbind(manonA, manonB)
            sum(manon$no_tags)
            dim(manon)
# Emilie/Jenna
            
            ej=two20[which(two20$RELEASE_VESSEL == "Lady Melissa"), ]
            sum(ej$no_tags)
            
            
            # per month
            two19$month = month(two19$RELEASE_DATE)
            two19$month =as.factor(two19$month)
            head(two19)
            
            # figure
              x<-aggregate(no_tags~month, two21, FUN=sum)
              y<-aggregate(no_tags~month, two20, FUN=sum)
              
              
              head(x)
ggplot(y, aes(month, no_tags)) + geom_point(size = 5, colour = "red") + theme(panel.background = element_rect(fill = "white", colour = "grey50"), , text = element_text(size=20))
              
            # Number of tags per person and event
              ggplot(relINFO, aes(month, no_tags)) + geom_point(aes(size = 5, colour = "red")) + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
              
            # Taggers
            x<-aggregate(no_tags~Tagger, two21, FUN=sum)
            head(x)
            ggplot(x, aes(Tagger, no_tags)) + geom_point(size = 5, colour = "red") + theme(panel.background = element_rect(fill = "white", colour = "grey50"), text = element_text(size=20))
            
            
            ggplot(x, aes(x=Tagger, y=no_tags)) + 
              geom_bar(stat = "identity", fill = "blue", width = 0.4) + ggtitle("Tags Applied per Tagger in 2021")
            
            
            
            
            #tags per year
            x<-aggregate(no_tags~Year, relINFO, FUN=sum)
            head(x)
            ggplot(x, aes(x=Year, y=no_tags)) + 
              geom_bar(stat = "identity", fill = "blue", width = 0.5) + ggtitle("Tags Applied per Year by the HSC")
            
            
            (size = 5, colour = "red") + theme(panel.background = element_rect(fill = "white", colour = "grey50"), text = element_text(size=20))
            
            
            
            
            # Number of tags per event
            ggplot(two21, aes(RELEASE_DATE, no_tags)) + geom_point(aes(colour = Tagger),size = 2) + theme(panel.background = element_rect(fill = "white", colour = "grey50"), text = element_text(size=12)) + ggtitle("2021")
            
            ggplot(two18, aes(RELEASE_DATE, no_tags)) + geom_point(aes(colour = Tagger),size = 2) + theme(panel.background = element_rect(fill = "white", colour = "grey50"), text = element_text(size=12)) + ggtitle("2018") 
            
                                                                
                                                                
  # 2018
            two18
            head(two18)
            sum(two18$no_tags)
            unique(two18$RELEASE_VESSEL)
            
            # Nicole Seamone
                  nicole=two18[which(two18$RELEASE_VESSEL == "Canada 100"), ]
                  sum(nicole$no_tags)
                  
                   dim(nicole)
            
            
            # Lisa Houston
                  lisa=two18[which(two18$RELEASE_VESSEL == "Morning Star"), ]
            
                    sum(lisa$no_tags)
            
                      dim(lisa)
            
            
            # Manon Holmes          
            
            manonA=two19[which(two19$RELEASE_VESSEL == "Lady Melissa"), ]
            manonB=two19[which(two19$RELEASE_VESSEL == "Fundy Monarch"), ]
            
            manon = rbind(manonA, manonB)
            sum(manon$no_tags)
            dim(manon)
            
            
            
            
            # Maps of tagging location
            
            
            # Mapping
            
            can<-getData('GADM', country="CAN", level=1) # provinces
            NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]
            CP <- as(extent(-67.6, -64, 43, 45.8), "SpatialPolygons")
            proj4string(CP) <- CRS(proj4string(NBNS))
            out <- gIntersection(NBNS, CP, byid=TRUE)
            
            boxes = read.csv("grounds_.csv")
            head(boxes)
            
            ggplot(two21, aes(x=X, y=Y))+  geom_polygon(data=boxes,aes(x=X, y=Y, group=GROUND, colour = GROUND), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group), fill = "grey", colour = "black") + geom_point(aes( colour = month), size = 2) + labs(x=NULL, y=NULL)+ coord_map() 
            
            
            ggplot(relINFO, aes(x=X, y=Y))+  geom_polygon(data=boxes,aes(x=X, y=Y, group=GROUND), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group), fill = "grey", colour = "black") + geom_point(aes(colour = Year), size = 1) + labs(x=NULL, y=NULL)+ coord_map() 
            
            
            
            
            
            
            # Look at outlier
            
            head(two21)
            
            
            out1=two21[which(two21$X > -65.5), ]
            out2=out1[which(out1$Y < 44.5), ]
            
            out3=two21[which(two21$Y > 45.5), ]
            
            
            
            
            
            dates=two20[which(two20$RELEASE_DATE == "2020-06-29"), ]
            dates2=two20[which(two20$RELEASE_DATE == "2020-06-28"), ]
            dates3=two20[which(two20$RELEASE_DATE == "2020-06-30"), ]
            
            ggplot(dates3, aes(x=X, y=Y))+  geom_polygon(data=boxes,aes(x=X, y=Y, group=GROUND), fill = "white", colour = "black")  + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_point(aes(colour = Tagger), size = 2)   + geom_point(aes(colour = Tagger), size = 2) + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey82"))            
            
            
            outside=two20[which(two20$X >  -65.5), ]
            outside2=outside[which(outside$Y <  44), ]
            
            ggplot(outside2, aes(x=X, y=Y))+  geom_polygon(data=boxes,aes(x=X, y=Y, group=GROUND), fill = "white", colour = "black")  + geom_polygon(data=out,aes(x=long, y=lat, group=group))  + geom_point(aes(colour = Tagger), size = 2)   + geom_point(aes(colour = Tagger), size = 2) + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey82"))            
                        