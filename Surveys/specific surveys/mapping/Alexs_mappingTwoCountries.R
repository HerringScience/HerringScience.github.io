## you will need the sp-package
library('sp')

## load a file from GADM (you just have to specify the countries "special  
# part" of the file name, like "ARG" for Argentina. Optionally you can  
# specify which level you want to have
loadGADM <- function (fileName, level = 0, ...) {
  load(url(paste("http://gadm.org/data/rda/", fileName, "_adm", level,  
                 ".RData", sep     = "")))
  gadm
}

## the maps objects get a prefix (like "ARG_" for Argentina)
changeGADMPrefix <- function (GADM, prefix) {
  GADM <- spChFIDs(GADM, paste(prefix, row.names(GADM), sep = "_"))
  GADM
}

## load file and change prefix
loadChangePrefix <- function (fileName, level = 0, ...) {
  theFile <- loadGADM(fileName, level)
  theFile <- changeGADMPrefix(theFile, fileName)
  theFile
}

## this function creates a SpatialPolygonsDataFrame that contains all maps  
## you specify in "fileNames".
## E.g.:
## spdf <- getCountries(c("ARG","BOL","CHL"))
## plot(spdf) # should draw a map with Brasil, Argentina and Chile on it.
getCountries <- function (fileNames, level = 0, ...) {
  polygon <- sapply(fileNames, loadChangePrefix, level)
  polyMap <- do.call("rbind", polygon)
  polyMap
}

spdf <- getCountries(c("CAN", "USA" ))

save(spdf,file="gadm_data.Rdata")

##---------------------------------------------------------------

# It's likely that you could change the country to USA and specify "Maine".
# You may be able to specify 2 countries country=c("CAN","USA") and add "Maine" to the # list in line 2.

#I have started to use ggmap. Here is some sample code:
  
  # general map extent
  lat <- c(-50, 75)               
  lon <- c(-100, 40)  

### Get a map
require(ggmap)
  map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 2,
               maptype = "watercolor", source = "stamen")

# Deployment area by maturity group
data2 = data
data2 = data2[Time==0,.(Count=length(Ndays)),by=c("Mature", "Lon2","Lat2")]
BASEMAP = ggmap(map,  base_layer = ggplot(data=data2),extent="panel") +
  theme(legend.key.size = unit(1, "cm"),legend.title = element_text(size=8)) +
  #facet_wrap(~YEAR1) + xlab("")+ylab("") +
  scale_colour_manual(values=c('red','dark green','blue','purple'),name='Mature') +
  coord_map(projection="mercator",
            xlim= c(-100, 40),
            ylim= c(-5, 70)) +
  xlab("Longitude") + ylab("Latitude") +
  geom_polygon(data=PSATpoly,aes(x=x,y=y,group=ID),col='black',fill="transparent")


BASEMAPMAP = BASEMAP + geom_jitter(aes(x=Lon2,y=Lat2,col=factor(Mature),size=Count),pch=16,alpha=.6,stroke=6, width=8,height=8)+ theme(legend.position="none") +
  geom_text(data=PolyLabel, aes(x=x,y=y,label=as.character(ID)),size=2)+facet_wrap(~Mature)

print(BASEMAPMAP)