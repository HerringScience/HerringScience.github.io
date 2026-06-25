


RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx")

loadfunctions( "acousticHerring")

# Mapping

can<-getData('GADM', country="CAN", level=1) # provinces
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
unique(can1$NAME_1)

      NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]

            CP <- as(extent(-69, -63, 42, 45.5), "SpatialPolygons")
            proj4string(CP) <- CRS(proj4string(NBNS))
            out <- gIntersection(NBNS, CP, byid=TRUE)

polysT = read.csv("timGrounds.csv")
polysNAFO = read.csv("NAFO_subunits.csv")
    ids = c("5Yb", "4Xr", "4Xq", "4Xs")

    polys_NAFO = polysNAFO[which((polysNAFO$Area %in% ids)), ] 

head(polysT)
head(polysNAFO)

load("relINFO.RData")
load("returnDF.RData")
rel = read.csv("TaggingEvents.csv")

head(rel)
rel$RELEASE_DATE =  as.Date(rel$RELEASE_DATE, "%Y-%m-%d")
str(rel)


# Mathc tag return to tagging event using tag number. Calculate days at Large



df1 = read.csv("tagPlots2020.csv")
head(df1)

          head(rel)
          head(returnDF)
          
          # first we need to determine days at large
          
                    retRel = merge(returnDF, rel, by = "TAG_NUMBER")
                    str(retRel)
                
                    
                    retRel$DaysAtLarge =retRel$RELEASE_DATE %--% retRel$DATE
                    retRel$DaysAtLarge = as.period(retRel$DaysAtLarge, unit = 'day')
                    retRel$DaysAtLarge = as.numeric(retRel$DaysAtLarge)
                    retRel$DaysAtLarge1 = retRel$DaysAtLarge/86400
                    retRel$DaysAtLarge = retRel$DaysAtLarge1 
                    retRel$DaysAtLarge1 = NULL
                    
                    head(retRel)
                    
                    # df with only days at large and tag number
                    days = data.frame(tag = retRel$TAG_NUMBER, DaysAtLarge = retRel$DaysAtLarge)
                    
                    returnDF$Type = 2
                    head(returnDF)
                    
                    write.table(r, file= "scanmarhh.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
                    
                    # figs is release and return info together:

save(retRel, file="retRel.RData", compress=T)

retRel = read.csv("retRel.csv")
head(retRel)


# Run tag return/relaease figures
tag.return.graph(r = retRel, boxes = polys_NAFO)

head(polys_NAFO)
head(out)

# Plot base map with Boxes    
# tag releases



# Look at individual events or return locations 
test=relINFO[which(relINFO$set == 88), ]
test = r[which(r$TAG_NUMBER == 455333), ]


ggplot(test, aes(x=X, y=Y)) + geom_polygon(data=polysT,aes(x=X, y=Y, group=Box, fill = Box), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(pch=21, size = 2, fill = "White") + ggtitle("Tag Releases") + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68"))


# Match tag return to tagging event




# Plot all maps
tag.return.graph(r)

# test individual divs.

rTest=r[which(r$div == 4), ]

ggplot(rTest, aes(x=X, y=Y))+  geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 1.1, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68"))

rTest

# Explore tag return rate with u

# Look at variability
head(u)
ggplot(u, aes(x = rate)) + geom_histogram(bins = 60) + ggtitle("Return Rate Variability")
mean(u$rate)

# Produce a bar graph, looking at all tagging events and number of tags and seeing where there was tag returns and where there wasn't
ggplot(all, aes(x = no_tags)) + geom_histogram(aes(fill = Return_),bins = 45) + ggtitle("Number of Fish Tagged Per Event")


min(u$no_tags)
mean(u$no_tags)
head(relINFO)
mean(relINFO$no_tags)
unique(relINFO$RELEASE_DATE)
100 - 2/33*100

# Look at tag return rate with number of tags
ggplot(u, aes(x = no_tags, y = rate)) + geom_point() + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "Tag Return Rate 2016", x = "No Tags")



# need to look at total number of tagging events and the ones where there were no returns

# Look how rate varys with date
ggplot(u, aes(x = release_date, y = rate)) + geom_point(pch=21, size = 3, fill = "grey70") + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "Return Rate and Date", x = "Date") + theme(text = element_text(size=15),axis.text.x = element_text(angle=90, hjust=1)) 
  # look at this topic with both return events and non return events
    # need to combine u with all
        head(u)
        

        ggplot(all, aes(x = release_date, y = rate)) + geom_point(pch=21, size = 3, fill = "grey70") + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "Return Rate and Date", x = "Date") + theme(text = element_text(size=15),axis.text.x = element_text(angle=90, hjust=1)) 

    # Look how rate varys with vessel
ggplot(u, aes(x = tagging_vessel, y = rate)) + geom_point() + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "Tag Return Rate 2016", x = "Date") + theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1)) 

# Look how no of tag returns and tags applied 
ggplot(u, aes(x = no_tags, y = no_tag_returns)) + geom_jitter(pch=21, size = 3, fill = "grey70", alpha = 0.5) + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "Number of tagged fish and returns", x = "No. Fish Tagged", y = "No. Tag Returns") + geom_smooth(method = lm)
head(u)


# explore noReturns
head(noReturns)
head(u)
head(all)


# Produce a bar graph with all the tagging events and no of tags applied

ggplot(all, aes(x = Year)) + geom_bar(aes(fill = Counted)) + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "Number of Surveys Counted for Annual Estimate 1997-2017") 

# Look at GB turnover
# recaptures in GB
no.Days  = c(33,3,4,6,12,12,12,13,14,14,7,12,11,52,88,15,30,23,2,8,3,8,11,11,2,13,34,15,6,12,1,34,11,88,22)
id = c(1:35)
to = data.frame(id, no.Days)

min(no.Days)
max(no.Days)
mean(no.Days)
ggplot(to, aes(x = no.Days)) + geom_histogram(binwidth = 5) + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "German Bank Turnover") 


# recaptures elsewhere
no.Days  = c(74,91,85,58,52,10,35,67,5)
length(no.Days)
id = c(1:9)
to = data.frame(id, no.Days)

min(no.Days)
max(no.Days)
mean(no.Days)
ggplot(to, aes(x = no.Days)) + geom_histogram(binwidth = 10) + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs(title = "German Bank Turnover") 

# look at Scots Bay tag return, 5 days at large
test=r[which(r$div == 28), ]
test1=test[which(test$group == 55), ]

w = test1[2, ]
w[1,2] = -66.48
w[1,4] = 55
w$id =2

d = rbind(test1, w)
d = d[order(d$id),]

ggplot(d, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "cornsilk1", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(aes(fill = group), pch=21, size = 2)  + labs(title = "Migration from German Bank to Scots Bay in 5 Days", x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68")) + geom_path(aes(group=group), lwd = 0.8, alpha = 0.7, colour = "blue") 

# calculate the distance between two points

# distance between GBSB and Long Island
a = earth.dist(long1 = -66.3490, lat1 = 43.4623, long2 = -66.48,lat2 = 44.3202)

# distance between Long Island and Scots Bay
b = earth.dist(long1 = -66.48,lat1 = 44.3202, long2 = -64.9667,lat2 = 45.1500)
total = a+b
# distance in meters
meters = total*1000
minutes = 1440 * 5
# meters/minute
speed = meters/minutes 
speed


# Plot distances travelled by fish using c1
ggplot(c1, aes(x = dist)) + geom_histogram(bins = 60) + ggtitle("Distance between Tagging and Recapture")
max(c1$dist)
mean(c1$dist)

test=c1[which(c1$dist > 200), ]

# Look at number of days at large and distance travelled
ggplot(c1, aes(x = daysAtLarge, y = dist)) + geom_point(pch=21, fill = "grey", size = 2) + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs( x = "Days at Large", y = "Distance Travelled") + geom_smooth(method = lm, na.rm = TRUE)
head(c1)


# Look at fish recaptured a _ amount of days after being tagged
noDays=r[which(r$daysAtLarge == 1), ]
head(noDays)

ggplot(noDays, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "cornsilk1", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(aes(fill = group), pch=21, size = 2)  + labs(title = "Tag Returns with 3 Days at Large", x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68")) + geom_path(aes(group=group), lwd = 0.8, alpha = 0.7, colour = "blue") 

      # Look at tag returns with one day at large
      dataP=c1[which(c1$daysAtLarge == 2), ]
      dataP
      ggplot(dataP, aes(x = dist)) + geom_histogram(bins = 6) + ggtitle("Distance between Tagging and Recapture")
      max(dataP$dist)
      min(dataP$speed)
      max(dataP$speed)
      mean(dataP$speed)
      length(dataP$group)
      
      
# look at speeds
      ggplot(c1, aes(x = speed)) + geom_histogram(bins = 15) + ggtitle("Overview of Speeds")
      fast=c1[which(c1$speed >  0.2), ]

# amount of tags applied per event      
         head(relINFO)   

         ggplot(relINFO, aes(x = no_tags)) + geom_histogram(bins = 20) + ggtitle("Number of Tags Applied Per Event")
         
         # Look at speed and time of year
      
      head(c1)
      
      # not sure if this really tells us anything.
      ggplot(c1, aes(x= release_month , y = speed)) + geom_jitter(pch=21, fill = "grey80", size = 2) + theme(panel.background = element_rect(fill = "white", colour = "grey50")) + labs( x = "Month", y = "Speed", title = "Release Month and Distance Travelled") 
      
      
  # Create DF for those returns that were captured in Gannet/GB and retrieved on a spawning area
      head(r)
      d1 = r[which(r$div == 1), ]
      t = d1[which(d1$group == 31), ]
      
      d2 = r[which(r$div == 3), ]
      t1 = d2[which(d2$group == 41), ]
      
      d3 = r[which(r$div == 4), ]
      t2 = d3
      
      d4 = r[which(r$div == 5), ]
      t3 = d4
      
      d5 = r[which(r$div == 6), ]
      t4 = d5[which(d5$group == 9), ]
      
      d6 = r[which(r$div == 7), ]
      t5 = d6[which(d6$group == 29), ]
      
      d7 = r[which(r$div == 8), ]
      t6 = d7[which(d7$group == 137), ]
      t7 = d7[which(d7$group == 140), ]
      t8 = d7[which(d7$group == 143), ]
      
      d8 = r[which(r$div == 9), ]
      t9 = d8    
    
      gannet = rbind(t, t1, t2, t3, t4, t5, t6, t7, t8, t9)
      dim(gannet)
    
      gannet$daysAtLarge = as.factor(gannet$daysAtLarge)
        head(gannet)
        # plot
      
        ggplot(gannet, aes(x=X, y=Y))+  geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 0.8, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68")) + ggtitle("Fish tagged on Gannet/GB, retrieved in a spawning area")
        
        write.table(gannet, file= "gannet.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
# Look at catch data
        ggplot(allGrounds, aes(x=LON, y=LAT))+  geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')+ geom_point(aes(colour = FISHING_VESSEL_1 )) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68")) + ggtitle("Detail Samples from 2016")

        head(allGrounds)
        