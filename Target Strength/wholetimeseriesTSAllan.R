

### Allan's TS values for the whole time series. Want to compare them to what was used. I have all the values from the reports.

# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")




# Load Data

TS1 = read.table("TS_est_SB.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
TS2 = read.table("TS_est_GB.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)

head(TS1)
TS1$DATE = as.Date(TS1$DATE,format = "%m/%d/%Y")

TS = rbind(TS1, TS2)
str(TS)

TS = na.omit(TS)


TS$Spawning.Ground = TS$S_GROUND
TS$TS = as.numeric(TS$TS)
TS$Year = year(TS$DATE)

head(TS)
head(dataC)

ggplot(dataC, aes(x=Date, y=TS)) + geom_point(aes(shape = Spawning.Ground), colour = "red", size = 2, alpha = 0.2)  + ggtitle("Reported Target Strength Versus Allan Scripted - No Filtering") + theme_bw() + geom_point(data = TS, aes(x=DATE, y = TS, shape = Spawning.Ground), colour = "blue") 

# definitely some funky things going on with Allan's calcs in a few years prior to 2010. Will need to check these. His values are also consistenly lower for these years?

# remove TS -34.5

TSA = TS[which(TS$TS < -34.5), ]
TSB = na.omit(TSA)


dataB = dataC[which(dataC$TS < -34.5), ]

ggplot(dataB, aes(x=Date, y=TS)) + geom_point(aes(shape = Spawning.Ground), colour = "red", size = 2, alpha = 0.2)  + ggtitle("Reported Target Strength Versus Allan Scripted - Juveniles Removed") + theme_bw() + geom_point(data = TSB, aes(x=DATE, y = TS, shape = Spawning.Ground), colour = "blue") 



  


  reported1<-aggregate(TS~Year+Spawning.Ground, dataB, FUN=mean)
  str(reported1)
  scripted1<-aggregate(TS~Year+Spawning.Ground, TSB, FUN=mean)
  head(TS)
  unique(TS$Spawning.Ground)
  summary(TS$Spawning.Ground)
  
  
  
  str(scripted1)
  scripted1$Year = as.factor(scripted1$Year)
  
  
  
  ggplot(reported1, aes(x=Year, y=TS)) + geom_point(aes(shape = Spawning.Ground), colour = "red", size = 4, alpha = 0.2)+ geom_point(data = scripted1, aes(x=Year, y = TS, shape = Spawning.Ground), colour = "blue", size = 4, alpha= 0.6) + ggtitle("Annual Averages with Juveniles Removed") + theme_bw() 
  
  
  + geom_point(data = TS, aes(x=DATE, y = TS, shape = Spawning.Ground), colour = "blue") 
  
  