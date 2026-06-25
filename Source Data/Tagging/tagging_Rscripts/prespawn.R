




RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "ggthemes", "maps", "stringr", "reshape2", "pander", "xlsx", "measurements", "sp", "ggrepel")

loadfunctions( "acousticHerring")

boxes = read.csv("catchBoxes_prespawn.csv")

can<-getData('GADM', country="CAN", level=1) # provinces
NBNS <- can[can@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec"),]
CP <- as(extent(-67.6, -64, 43, 45.8), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)



load("returnComplete.RData")





# Script for subsetting the tag returns into categories to explore the data further:

#pre-spawn 

ids = c(87,3,8,10,13,24,28,30,32,1,7,9,11,20,27,29,31,33,34,40,41,42,43,45,47,48,52,58,59,61,80,102,103,141)

prespawn  = returnComplete[which((returnComplete$set %in% ids)), ]
dim(prespawn)

# prespawn scots
# try half first

ids = c(9,10,27,30,30,61,102)

# 3,7,41,42,59,87,103
      prescots  = returnComplete[which((returnComplete$set %in% ids)), ]
  
    # only plot return point

      prescots2=prescots[which(prescots$type == 2), ]


ggplot( prescots2, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_text_repel(aes(x = X, y = Y, label = TAG_NUMBER))


head(returnComplete)

tags = c(448906,448505,34171,420714,419770,448912,456509,432950,441801,421170,421619,431604,460131,440261,455887)
prescots  = returnComplete[which((returnComplete$TAG_NUMBER %in% tags)), ]


write.table(prescots, file= "prescots.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


## Plot tag returns that were tagged on feeding grounds in the prespawn season and recaptured in Scots
ggplot(prescots,aes(X, Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 0.5, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 1, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68"))


ggplot(prescots,aes(X, Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='white',colour='black') + geom_point(aes(colour = type), size = 4, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68"))


# Plot each individual tag return


prescots$type = as.factor(prescots_$type)

county_list <- unique(prescots$TAG_NUMBER)

r = prescots
r$div = prescots$TAG_NUMBER

head(r)
str(r)


# create for loop to produce ggplot2 graphs 
for (i in seq_along(county_list)) { 
  
  # create plot for each county in df 
  plot <- 
    ggplot(subset(r, r$div==county_list[i]),
           aes(X, Y)) + 
    geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 1.5, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68")) +
    
    ggtitle(paste(county_list[i]))
  
  # save plots as .png
  ggsave(plot, file=paste(results, county_list[i], ".png", sep="_"), scale=2)
  
  
  # print plots to screen
  print(plot)
}









### Now do the same for German Bank

# ids are 9,10,27,30,40,61,102,1,8,11,13,20,24,28,29,31,32,33,34,43,45,47,48,52,58,80,141
# done 29,31,32,33,34,9,10,27,30, 40,61,102,1, 8,11,13,20,24,28
ids = c(43,45,47,48,52,58,80,141)

pregerman  = returnComplete[which((returnComplete$set %in% ids)), ]
pregerman2=pregerman[which(pregerman$type == 2), ]

ggplot( pregerman2, aes(x=X, y=Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group), fill = "white") + geom_text_repel(aes(x = X, y = Y, label = TAG_NUMBER), colour = "blue", size = 4)


tags = c(445969,458887,445880,445186,457550,454452,454158,457449,459012,458336,454497,457674,458295,457506,420927,420894,420971,420925,422113,420881,420973,426821,428739,420908,440888,422345,440591,440910,440787,421877,437082,431880,435502,435040,437873,435157,431354,435461,431162,437173,437865,431545,431690,431628,421727,431647,440457,431662,431629,431509,421745,431543,431555,431699,421726,431506,421549, 419252,441773,448003,460132,419373)

pregerman3  = returnComplete[which((returnComplete$TAG_NUMBER %in% tags)), ]

pregerman_ = read.csv("pregerman3.csv")
str(pregerman_)
pregerman_$daysAtLarge = as.factor(pregerman_$daysAtLarge)

head(pregerman_)

## Plot tag returns that were tagged on feeding grounds in the prespawn season and recaptured in Scots
ggplot(pregerman_,aes(X, Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 0.5, alpha = 0.7) + geom_point(aes(colour = daysAtLarge), size = 1, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68"))

pregerman_$type=  as.factor(pregerman_$type)

ggplot(pregerman_,aes(X, Y)) + geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='white',colour='black') + geom_point(aes(colour = type), size = 4, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68"))



dim(pregerman3)
write.table(pregerman3, file= "pregerman3.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

# save figures for each individual tag return

results = "C:/Users/herri/OneDrive/Documents/Jenna/workspace/"

head(pregerman_)
str(pregerman_)

pregerman_$type = as.factor(pregerman_$type)

county_list <- unique(pregerman_$TAG_NUMBER)

r = pregerman_
r$div = pregerman_$TAG_NUMBER

head(r)
str(r)


# select only those returns who the release date was june or july

head(r)


# create for loop to produce ggplot2 graphs 
for (i in seq_along(county_list)) { 
  
  # create plot for each county in df 
  plot <- 
    ggplot(subset(r, r$div==county_list[i]),
           aes(X, Y)) + 
    geom_polygon(data=boxes,aes(x=X, y=Y, group=Box), fill = "white", colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black') + geom_path(aes(group=group, colour = daysAtLarge), lwd = 1.5, alpha = 0.7) + geom_point(aes(shape = type, colour = daysAtLarge), size = 3, alpha = 0.6) + labs(x=NULL, y=NULL) + coord_map()  + theme(panel.background = element_rect(fill = "grey68")) +
    
    ggtitle(paste(county_list[i]))
  
  # save plots as .png
  ggsave(plot, file=paste(results, county_list[i], ".png", sep="_"), scale=2)
  
  
  # print plots to screen
  print(plot)
}


sum(120,30)



# Plot just the pre-spawn feeding areas and the spawning grounds

unique(boxes$Box)

boxes = master[which((master$id %in% ids)), ]


unique(boxes$Box)

ggplot(boxes,aes(x=X, y=Y, group = Box), fill = "white", colour = "black")+ geom_polygon(aes(fill = type), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='grey75',col='black') + labs(x=NULL, y=NULL) + coord_map()  

