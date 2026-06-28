library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
getwd()
setwd("") #where you want the shape file located

land.all <-
  ne_countries(scale = "large",
               returnclass = "sf",
               continent = "North America")


#add survey boxes
ScotsBay_strata_box <-
  st_linestring(rbind(
    c(-65.2333,	45.0364 ),
    c(-65.2333, 45.1667	),
    c(-64.825, 45.3117),
    c(-64.6811, 45.2162),
    c(-65.2333,	45.0364)
  ))
crs_use <- st_crs(land.all)
ScotsBay_strata_box <- st_sfc(ScotsBay_strata_box, crs = crs_use)
Scots_attrib <- data.frame(name = "Scots Bay Strata Box")
Scots_sf <- st_sf(Scots_attrib, geometry = ScotsBay_strata_box)


st_write(Scots_sf, dsn = "ScotsBay.shp", layer = "Scotsbaybox.shp", driver = "ESRI Shapefile")
