rm(list=ls())

library(lubridate)
library(ggplot2)
library(ggplot2)
library(patchwork)
library(scales)
library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
#library(weathercan)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
#library(rgeos)
library(sf)
library(terra)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(DT)
library(dygraphs)
library(leaflet)
library(rmapshaper)
library(plotly)
library(mapproj)
library(oce) #new CTD Data package
library(pander)
library(janitor)

#Import All Boxes
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")

# Scots Bay plankton and CTD box
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]

# Scots Bay
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

#German Bank CTD box
GBCTD=boxes[which(boxes$Box == "GBocean"), ]

# German Bank      
SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")

# Seal Island      
SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")


# Run first few lines of taggingMaster first to load in relINFO

setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns"))

relINFO <- read.csv("relINFO.csv")

head(relINFO)

# Years

two16=relINFO[which(relINFO$Year == "2016"), ]
two17=relINFO[which(relINFO$Year == "2017"), ]
two18=relINFO[which(relINFO$Year == "2018"), ]
two19=relINFO[which(relINFO$Year == "2019"), ]
two20=relINFO[which(relINFO$Year == "2020"), ]
two21=relINFO[which(relINFO$Year == "2021"), ]
two22=relINFO[which(relINFO$Year == "2022"), ]
two23=relINFO[which(relINFO$Year == "2023"), ]
two24=relINFO[which(relINFO$Year == "2024"),]

# Temporal range of tagging season

unique(two16$Julian)
unique(two17$Julian)
unique(two18$Julian)
unique(two19$Julian)
unique(two20$Julian)
unique(two21$Julian)
unique(two22$Julian)
unique(two23$Julian)

#2023

head(two23)
sum(two23$no_tags)
unique(two23$RELEASE_VESSEL)

# William Cusack
william=two23[which(two23$RELEASE_VESSEL == "Canada 100"), ]
sum(william$no_tags)
head(william)
dim(william)

#Lee Surette
lee=two23[which(two23$RELEASE_VESSEL == "Lady Janice"), ]
sum(lee$no_tags)
head(lee)
dim(lee) #dimensions of the dataframe.

#Joseph Nickerson
joseph=two23[which(two23$RELEASE_VESSEL == "Lady Melissa"), ]
sum(joseph$no_tags)
head(joseph)
dim(joseph)

#Nicholas d'Entremont
nick=two23[which(two23$RELEASE_VESSEL == "Morning Star"), ]
sum(nick$no_tags)
head(nick)
dim(nick)

#Annik Doucette
annik=two23[which(two23$RELEASE_VESSEL == "Sealife"), ]
sum(lee$no_tags)
head(annik)
dim(annik)

#Dale Fitzgerald
dale=two23[which(two23$RELEASE_VESSEL == "Tasha Marie"), ]
sum(dale$no_tags)
head(dale)
dim(dale)

#2021

head(two21)
sum(two21$no_tags)
unique(two21$RELEASE_VESSEL)

#2020

head(two20)
sum(two20$no_tags)
unique(two20$RELEASE_VESSEL)



# 2019
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
            two23$month = month(two23$RELEASE_DATE)
            two23$month =as.factor(two23$month)
            head(two23)
            
            # figure
              x<-aggregate(no_tags~month, two23, FUN=sum)
              y<-aggregate(no_tags~month, two23, FUN=sum)
            
              
              head(x)
              
ggplot(y, aes(month, no_tags)) + 
  geom_point(size = 5, colour = "red") + 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), , text = element_text(size=20))

TagsPerMonth2023 <- data.frame(y) %>%
  adorn_totals("row")
datatable(TagsPerMonth2023)
              
            # Number of tags per person and event
              ggplot(relINFO, aes(month, no_tags)) + 
                geom_point(aes(size = 5, colour = "red")) + 
                theme(panel.background = element_rect(fill = "white", colour = "grey50"))
              
            # Taggers
            x<-aggregate(no_tags~Tagger, two23, FUN=sum)
            head(x)
            TotalTagger2023Table <- data.frame(x) %>%
              adorn_totals("row")
            datatable(TotalTagger2023Table)
            ggplot(x, aes(Tagger, no_tags)) + geom_point(size = 5, colour = "red") + theme(panel.background = element_rect(fill = "white", colour = "grey50"), text = element_text(size=20))
            
            
            ggplot(x, aes(x=Tagger, y=no_tags)) + 
              geom_bar(stat = "identity", fill = "blue", width = 0.4) + ggtitle("Tags Applied per Tagger in 2023")
            
            
            
            
            #tags per year
            x<-aggregate(no_tags~Year, relINFO, FUN=sum)
            head(x)
TotalYearTable <- data.frame(x) %>%
  adorn_totals("row")

datatable(TotalYearTable)
  
  
  
print(TotalYearTable)
            ggplot(x, aes(x=Year, y=no_tags)) + 
              geom_bar(stat = "identity", fill = "blue", width = 0.5) + ggtitle("Tags Applied per Year by the HSC") + 
              #aes(size = 5, colour = "red") + 
              theme(panel.background = element_rect(fill = "white", colour = "grey50"), text = element_text(size=20))
            
            
            
            
            # Number of tags per event.
            ggplot(two23, aes(Julian, no_tags)) + 
              geom_point(aes(colour = Tagger),size = 2) +
              scale_x_continuous(breaks = seq(150, 300, 10)) +
              scale_y_continuous(breaks = seq(0, 1000, 50)) +
              theme(panel.background = element_rect(fill = "white", colour = "grey50"), text = element_text(size=12)) + 
              ggtitle("2023")
              
            
            ggplot(two18, aes(Julian, no_tags)) + 
              geom_point(aes(colour = Tagger),size = 2) + 
              scale_x_continuous(breaks = seq(150, 300, 10)) +
              scale_y_continuous(breaks = seq(0, 1000, 50)) +
              theme(panel.background = element_rect(fill = "white", colour = "grey50"), text = element_text(size=12)) + 
              ggtitle("2018") 
            
                                                                
                                                                
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
            
            
# Mapping Took out gIntersections and replaced with crop as package was discontinued.
        
can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Qu?bec"),]
CP <- as(extent(-67.6, -64, 43, 45.8), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- crop(NBNS, CP, byid=TRUE)
            
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
boxes = read.csv("surveyBoxes.csv")
#boxes = read.csv("grounds_.csv")
head(boxes)

            ggplot(two23, aes(x=X, y=Y))+  
              geom_polygon(data=boxes,aes(x=X, y=Y, group=Box, colour = box), fill = "white", colour = "black") + 
              geom_polygon(data=out,aes(x=long, y=lat, group=group), fill = "grey", colour = "black") + 
              geom_point(aes( colour = month), size = 2) + 
              labs(x=NULL, y=NULL)+ coord_map() 
            
            
relINFO <- subset(relINFO, X < -63.9)
relINFO <- subset(relINFO, Y < 45.8)
            ggplot(relINFO, aes(x=X, y=Y, colour = Year))+  
              geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + 
              geom_polygon(data=out,aes(x=long, y=lat, group=group), fill = "grey", colour = "black") +
              geom_point(aes(colour = Year), size = 1) +
              labs(x=NULL, y=NULL)+ 
              coord_map() 
            
            
            
            
            
            
# Look at outlier
            
head(two21)
            
            
out1=two21[which(two21$X > -65.5), ]
out2=out1[which(out1$Y < 44.5), ]
            
out3=two21[which(two21$Y > 45.5), ]
            

            
dates=two20[which(two20$RELEASE_DATE == "2020-06-29"), ]
dates2=two20[which(two20$RELEASE_DATE == "2020-06-28"), ]
dates3=two20[which(two20$RELEASE_DATE == "2020-06-30"), ]
            
ggplot(dates3, aes(x=X, y=Y))+  
   geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black")  + 
   geom_polygon(data=out,aes(x=long, y=lat, group=group))  + 
   geom_point(aes(colour = Tagger), size = 2)   + 
   geom_point(aes(colour = Tagger), size = 2) + 
   labs(x=NULL, y=NULL) + 
   coord_map() + 
   theme(panel.background = element_rect(fill = "grey82"))            
            
            
outside=two20[which(two20$X >  -65.5), ]
outside2=outside[which(outside$Y <  44), ]
            
ggplot(outside2, aes(x=X, y=Y))+  
    geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black")  + 
    geom_polygon(data=out,aes(x=long, y=lat, group=group))  + 
    geom_point(aes(colour = Tagger), size = 2)   + 
    geom_point(aes(colour = Tagger), size = 2) + 
    labs(x=NULL, y=NULL) + 
    coord_map() + 
    theme(panel.background = element_rect(fill = "grey82"))            
                        
            
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/"))        
TaggingEvents <- read.csv("TaggingEvents.csv")  


TaggingEvents <- TaggingEvents %>% filter(Year == "2023")
TaggingEvents$Month <- month(TaggingEvents$Date)

TaggingEvents <- TaggingEvents %>% group_by("Tagger")

Annik <- subset(TaggingEvents, Tagger == "Annik Doucette")

# TaggingEventsStats <- TaggingEvents %>% group_by(Tagger) %>%
#       summarise(n_distinct(Tag_Num))
# 
# TaggingEventsStats <- TaggingEvents %>% 
#   group_by(Tagger) %>%
#   summarise(n_distinct(Tag_Num)) %>%
#   

TaggingEventsStats <- aggregate(Tag_Num ~ Tagger + Month, data = TaggingEvents, FUN = length )
TaggingEventsStats %>% group_by(Month)

print(ggplot(TaggingEventsStats, aes(x = Month, y = Tag_Num, fill = Tagger)) +
  geom_col())

datatable(TaggingEventsStats)

