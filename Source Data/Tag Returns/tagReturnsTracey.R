rm(list = ls())

#Import all the required libraries. The ones commented out have been discontinued, but left in in case something breaks and we need to find an alternative.
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
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(dplyr)


# Set working directory to github source data
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data"))

completeReturns <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/complete.returns.csv"))
TaggingEvents <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/TaggingEvents.csv"))
  TaggingEvents <- TaggingEvents %>%
      rename(TAG_NUMBER = Tag_Num)
rawReturn <- read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/rawReturn.csv"))

#Combining complete Returns and Tagging events by tag number
TagReturns <- merge(TaggingEvents, completeReturns, by = "TAG_NUMBER")

# #Import All Boxes
# setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Box Coordinates/"))
# boxes = read.csv("surveyBoxes.csv")
# 
# # Scots Bay plankton and CTD box
# SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
# SBCTD=boxes[which(boxes$Box == "SBocean"), ]
# 
# # Scots Bay
# SUA = read.csv("polygon_SBEastern.csv")
# polyEastern = as.PolySet(SUA, projection="LL")
# 
# SUA = read.csv("polygon_SBNorthern.csv")
# polyNorthern = as.PolySet(SUA, projection="LL")
# 
# SUA = read.csv("polygon_SB.csv")
# polySB_main = as.PolySet(SUA, projection="LL")
# 
# #German Bank CTD box
# GBCTD=boxes[which(boxes$Box == "GBocean"), ]
# 
# # German Bank      
# SUA = read.csv("polygon_GB.csv")
# polyGB = as.PolySet(SUA, projection="LL")
# 
# # Seal Island      
# SUA = read.csv("polygon_SI.csv")
# polySI = as.PolySet(SUA, projection="LL")
# 
# can<-getData('GADM', country="CAN", level=1)
# us = getData('GADM', country = "USA", level = 1)
# can1 = rbind(can,us)
# NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]
# 
# # All boxes
# 
# proj4string(CP) <- CRS(proj4string(NBNS))
# out <- crop(NBNS, CP, byid=TRUE)
# CP <- as(extent(-69, -63, 42, 45.5), "SpatialPolygons")
# proj4string(CP) <- CRS(proj4string(NBNS))
# out <- crop(NBNS, CP, byid=TRUE)

# Histogram showing average time differences between tagging in Days. Split into categories.
# Cateogry 1 = 0 to 2 days. These should mostly be removed from analysis due to them being caught almost instantly.
# Category 2 = 3 to 10 days. This is the period that the survey window covers.
# Cat. 3 = 11 to 30 days. Rest of the first month.
# Cat. 4 = 31 to 183 days. Caught after the first month, before the 6 month mark.
# Cat. 5 = 184 to 365. Minimum time of 6 months between tagging event and being recaptured.
# Cat. 6 = 366 to 730. Caught a minimum of a year, up to two years after the tagging event.
# Cat. 7 = Anything after two years since tagging event.

TagReturns$timeDifference <- TagReturns$DATE - TagReturns$Date
  TagReturns$timeDifference <- as.numeric(TagReturns$timeDifference)
  TagReturns <- TagReturns[TagReturns$timeDifference >= 0,]
TagReturns$category = with(TagReturns, ifelse(TagReturns$timeDifference < 3, 1,
                                              ifelse(TagReturns$timeDifference < 11 & TagReturns$timeDifference >= 3, 2,
                                                     ifelse(TagReturns$timeDifference < 31 & TagReturns$timeDifference >= 11, 3,
                                                            ifelse(TagReturns$timeDifference < 184 & TagReturns$timeDifference >= 31, 4,
                                                                   ifelse(TagReturns$timeDifference < 366 & TagReturns$timeDifference >= 184, 5,
                                                                          ifelse(TagReturns$timeDifference < 731 & TagReturns$timeDifference >= 366, 6,
                                                                                 ifelse(TagReturns$timeDifference < 1095 & TagReturns$timeDifference >= 731, 7, 8))))))))
                                                   
  
TagReturns$category <- as.numeric(TagReturns$category)

TagReturns = TagReturns %>%
  group_by(category) %>%
  mutate(AvgtimeDifference = mean(timeDifference)) %>%
  mutate(No.InCategory = length(category)) %>%
  mutate(timeDifferenceMin =min(timeDifference)) %>%
  mutate(timeDifferenceMax= max(timeDifference)) %>%
  mutate_if(is.numeric, format, digits = 2)

TagReturns$timeDifferenceMin <- as.numeric(TagReturns$timeDifferenceMin)
TagReturns$timeDifferenceMax <- as.numeric(TagReturns$timeDifferenceMax)
TagReturns$timeDifference <- as.numeric(TagReturns$timeDifference)

print(ggplot(data = TagReturns, (aes(category, fill = AvgtimeDifference))) + 
        geom_histogram(binwidth=1, colour = "white") + 
        theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
        labs(x = "Category", y="Count"))

TagReturns = TagReturns %>%
  group_by(timeDifference) %>%
  mutate(No.ofSameNumberOfDays = length(timeDifference)) %>%
  mutate_if(is.numeric, format, digits = 2)

        


Table <- TagReturns %>% 
  dplyr::select(category,
                timeDifference,
                AvgtimeDifference,
                timeDifferenceMin,
                timeDifferenceMax,
                No.InCategory,
                No.ofSameNumberOfDays)

Table1 <- unique(Table)

Table1 %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Tag Returns/tagReturnsTracey.csv"))

print(ggplot(data = Table1, (aes(category, fill = timeDifference))) + 
        geom_histogram(binwidth=1, colour = "white") + 
        theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
        labs(x = "Category", y="Count", main = ))

# ggplot(data = subset(TagReturns, Lon < -64.1), aes(x=Lon, y=Lat, xend = X)) + 
#   geom_polygon(data=polysT,aes(x=X, y=Y, group=Box, fill = Box), colour = "black") + 
#   geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') +
#   geom_point(pch=21, size = 2, fill = "White") + 
#   ggtitle("Tagging Locations") + 
#   labs(x=NULL, y=NULL) + 
#   coord_map() + 
#   theme(panel.background = element_rect(fill = "grey68"))






