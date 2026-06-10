


# Load in the spreadsheet that contains all the raw cast data which is manually updated during the field season. This script formats the data frame to ready it for analysis 

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap", "oce", "ggrepel")


loadfunctions( "acousticHerring")

se <- function(x) sqrt(var(x)/length(x))

# Creation of CTD dataframes

# the version from year one are:
 # ctdData.csv and CTD_db2.csv

# CTD_Raw is the raw CTD data with; ground, plankton_ID, Date,Lat, Lon and Year added manually
# plankton_ID is either the plankton_ID that is associated with the cast, or is blank if no tow was conducted with the cast.

    # load in ctd data
    data = read.csv("CTD_Raw.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
    events = read.csv("CTD_Event.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

            # Formatting functions    
              data = ctdRaw(x = data)
              events = ctdEvent_1 (x = events)
        
              # Add max depth to events
                      
                      x = aggregate(Depth~id, data, FUN=max)
                        events = merge(events,x, by = "id")
                         
                        
                        # Will have to manually add whether the location is within a plankton box, can use this plot
                       
                        
                        
                        # load("scotsplankB.RData")
                        # load("germanplankB.RData")
                      
                        # unique(events$ground)
                        # ids = c("Lurcher", "Long Island", "NB Coastal", "Seal Island")
                        
                        eventsSub = events[which((events$ground %in% ids)), ]
                        # eventsSub$plankB = 0
                        
                        # if events$plankB  = 0, then the cast is not within a plankton box, if it equals 1 then it is the Scots Bay plankton box, and if it is 2 then it is in the German Bank plankton box
                        
                        events = rbind(eventsSub, germanplankB, scotsplankB)
                        
                        
                        # Add categories for if the cast was greater than 30m
                        
                        events  = ctdEvent_2(events = events)
                        
                        
                        
                        #  QC make sure that events and data have the same number of casts
                                        r = unique(data$id)
                                        t = unique(events$id)              
                                        
                                            str(r)
                                            str(t)

                                                #If there are differences between the data sets this function will list the missing ID
                                                setdiff(t, r)
                                                
                              
                          
                          CTDdata = data
                          head(CTDdata)
                                dim(CTDdata)
                          head(events)
                                dim(events)
                          
                              save(CTDdata, file="CTDdata.RData", compress=T)
                              save(events, file="events.RData", compress=T)
                              
               
                  
                  
# This data frame takes all cast data and applys the analyses; event data with average, se, min, max, ranges for temperature, salinity and density
                  
            
                              
                                  
            castAway = CTDdf(data = data, events  = events)
                castAway
                
                head(castAway)
                
save(castAway, file="castAway.RData", compress=T)
write.table(castAway, file= "castAway.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 






# CTD data analysis for stratification, average salinities, temperatures.

# Creation of oneOff which obtains cast data that of less than 1m for surface
# create of stratification which presents cast data from casts deeper than 30m to calculate salinity and temperature measurements at depth.


# these dataframes are created in CTDdataFrames.R
# Basic dataframes that are created from the raw data 

load("CTDdata.RData")
load("events.RData")

# Determine salinity and temperature average at 1m and 30m
# we may want to adjust the min depth to include more casts...I think I good few in 2019 were just shy of the 29m mark.


oneOff = CTDsurface(data = data, events =events)

stratification=strat(depthLowerLimit = 29.9, depthUpperLimit = 31, data = CTDdata, events = events)

stratified = merge(oneOff, stratification, by  = "id")

stratified$densDiff = (stratified$density30) - (stratified$density1) 
stratified$tempDiff = (stratified$avgTemp30) - (stratified$avgTemp1) 
stratified$salDiff = (stratified$avgSal30) - (stratified$avgSal1) 


# Make a column in events 

save(stratified, file="stratified.RData", compress=T)
save(oneOff, file="oneOff.RData", compress=T)      


write.table(stratified, file= "stratified.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
write.table(oneOff, file= "oneOff.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 



# Map the locations of each CTD cast
can<-getData('GADM', country="CAN", level=1) # provinces
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
unique(can1$NAME_1)

NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]

CP <- as(extent(-69, -63, 42, 45.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(NBNS))
out <- gIntersection(NBNS, CP, byid=TRUE)

polysT = read.csv("timGrounds.csv")         

                # load("polys_NAFO.RData")
                # load("grounds.RData")

head(oneOff)
unique(oneOff$ground)

ggplot(oneOff, aes(x=Lon, y=Lat)) + geom_polygon(data=polysT,aes(x=X, y=Y, group=Box, fill = Box), colour = "black") + geom_polygon(data=out,aes(x=long, y=lat, group=group),fill='burlywood4',col='black')  + geom_point(pch=21, size = 2, fill = "White")+ ggtitle("Tag Releases") + labs(x=NULL, y=NULL) + coord_map() + theme(panel.background = element_rect(fill = "grey68"))
