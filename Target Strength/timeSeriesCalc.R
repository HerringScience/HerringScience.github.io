
# These function are sourced from ecomod so will only work if ecomod is set up with the Rprofile
RLibrary( "lubridate", "ggplot2", "reshape", "pastecs", "raster", "psych", "Hmisc", "plyr", "PBSmapping", "maps", "mapdata", "adehabitat", "PBSmodelling","maptools","RColorBrewer", "classInt", "rgeos", "SDMTools", "mapproj", "ggmap")

# The structure of ecomod is that a folder needs to be created, then with a 'src' folder, then '_Rfunctions' which contains all the funtions. Running this line mkaes all the functions active

loadfunctions( "acousticHerring")
loadfunctions( "polygons")



data = read.table("TSLW.csv", header=TRUE, sep=",", row.names = "id", stringsAsFactors=FALSE)
head(data)
dim(data)
data$Year = as.factor(data$Year)


str(data)

# remove rows with NA
dataC = na.omit(data)
dim(dataC)


# create German Bank and Scots Bat DFs

ids = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

German=dataC[which(dataC$Spawning.Ground == "German Bank"), ]
last10G = German[which((German$Year %in% ids)), ] 

Scots=dataC[which(dataC$Spawning.Ground == "Scots Bay"), ]
last10S = Scots[which((Scots$Year %in% ids)), ] 



# German Bank
ggplot(German, aes(x=MeanLengthmm, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10G, aes(x=MeanLengthmm, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10G, aes(x=Year, y=TS)) + geom_point(aes(colour = Year))
ggplot(last10G, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10G, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year))
ggplot(German, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(German, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year)) +geom_line() 
ggplot(NoJuvG, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year)) +geom_line()
ggplot(NoJuvG, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year)) +geom_line() 



# Scots Bay
ggplot(Scots, aes(x=MeanLengthmm, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10S, aes(x=MeanLengthmm, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10S, aes(x=Year, y=TS)) + geom_point(aes(colour = Year))
ggplot(last10S, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(last10S, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year))
ggplot(Scots, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))
ggplot(Scots, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year))
ggplot(NoJuvS, aes(x=Year, y=MeanLengthmm)) + geom_point(aes(colour = Year)) + geom_line()
ggplot(NoJuvS, aes(x=Year, y=MeanWeightgm)) + geom_point(aes(colour = Year))+geom_line()



# Remove rows with mean length <23cm

NoJuvS = Scots[Scots$MeanLengthmm > 23, ]
NoJuvG = German[German$MeanLengthmm > 23, ]

