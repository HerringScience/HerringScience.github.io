#### biomass calculation from transects exported from Echoview into excel
#Integrations must be placed in same folder and separated (e.g. GB and SI)

rm(list = ls())

library(dplyr)
library(geosphere)
library(sp)
#library(rgdal)
library(raster)
library(adehabitatHR)
#library(plotKML)
#library(maptools)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
#devtools::install_github("ropensci/rnaturalearthhires") needed to run this to install it with my version of R
library(rnaturalearthhires) 
library(sf)
library(ggplot2)
library(units)

#probably need a "top line broken" =T/F check here instead of diving into the code
surveydate <- "2026-06-21" #YYYY-MM-DD
surv="SB" #SB or GB or SI
year="2026"
surv.no="4"

#CODE FOR WHEN ONE BOX IS ALL ONE VESSEL 
#NorthBox<-c(grep("MS",transects$Region_name, value = TRUE, fixed = TRUE))
NorthBox <- c("MS_T02", "MS_T03", "MS_T04", "MS_T05")
EastBox <- c("FM_T02", "FM_T03", "LM_T02", "LB_T02", "LB_T03")

TS38 <- -35.5 
TS50 <- -35.5-0.10727
TS75 <- -35.5 -0.26575
TS120 <- -35.5 -0.44946

setwd(paste0("C:/Users/", Sys.info()[7],"/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " 21-06-2026/Export/"))
source(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/DFO Scripts/DFO Scripting 2026/Acoustic_biomass functions.R"))
#setwd(paste0("E:/Acoustic Index Review Files/Integration Versions/2022_07_06/2024/Scots Bay/",surveydate))
#source("E:/Acoustic Index Review Files/Rscripting and automation/2022_07_06_R_SurveyScripts/Acoustic_biomass functions.R")

file_list <- list.files(path = getwd())
transects <- data.frame()

#identify columns to keep from csv files
keep <-
  c(
    "Region_name",
    "Lat_S",
    "Lon_S",
    "Lat_E",
    "Lon_E",
    "Dist_S",
    "Dist_E",
    "Area_Backscatter_Strength",
    "Frequency",
    "Time_S",
    "Time_E",
    "Date_S"
  )

#for each file in file_list, read in csv, keep specified columns, rbind to dataset to produce one dataset
for (i in 1:length(file_list)) {
  temp_data <- read.csv(file_list[i])
  temp_data2 <- temp_data[keep]
  transects <- rbind(transects, temp_data2)
}
rm(temp_data, temp_data2)
transects

#add survey boxes
SB_survey_box <- rbind(c(45.012,-65.202), c(45.174,-65.202), c(45.315,-64.834), c(45.218,-64.672), c(45.012,-65.202))
SB_survey_box <- as.data.frame(cbind(rep(1,nrow(SB_survey_box)),SB_survey_box))
names(SB_survey_box) <- c("ID","Y","X")

#create column with boat name for plotting
transects$Boat <- as.character(transects$Region_name)
transects$Boat <- substr(transects$Boat,2,3)
transects$Boat

transect_a<-array(dim=c(2,2,length(transects$Region_name)))
ln_a <- list()
ln_b <- list()
for(i in 1:length(transects$Region_name)){
  transect_a[,,i]<-matrix(c(transects$Lon_S[i], transects$Lon_E[i], transects$Lat_S[i], transects$Lat_E[i]), ncol=2) 
  ln_a[[i]] <- Line(transect_a[,,i])
  ln_b[[i]] <- Lines(ln_a[[i]], ID = as.character(transects$Region_name[i]))
}

# Create SpatialPoints
SP <- SpatialPoints(coords = cbind(transects$Lon_S, transects$Lat_S))
# Add label variable
SP$ID <- transects$Region_name
projection(SP)<- CRS("+proj=utm +zone=19 +datum=WGS84")

sp_lns <- SpatialLines(ln_b)
projection(sp_lns) <- CRS("+proj=utm +zone=19 +datum=WGS84")

sbP<-Polygon(SB_survey_box[,3:2])
sbPs <- Polygons(list(sbP),1)
sbspS <- SpatialPolygons(list(sbPs))
projection(sbspS) <-  CRS("+proj=utm +zone=19 +datum=WGS84")
plot(sp_lns,col="red",  axes=FALSE)
plot(sbspS, add=TRUE)
pointLabel(coordinates(SpatialPoints(coords = cbind(transects$Lon_S, transects$Lat_S))),
           labels=as.character(transects$Region_name))
plot(SpatialPoints(coords = cbind(transects$Lon_S, transects$Lat_S)), add=TRUE)
axis(1, at = c(-65.3 + 0:25 *0.025), cex.axis=0.7)
axis(2, at = c(45 + 0:20 *0.05), cex.axis=0.7)

#NEED TO MANUAL EDIT FILTERS FOR SCOTS BAY UNTIL BOXES ARE DEFINED USING POLYGON SHAPES
transects_ScotsBayMain <- filter(transects, !Region_name %in% c(NorthBox,EastBox))
transects_ScotsBayNorthBox <-  filter(transects, Region_name %in% NorthBox)
transects_ScotsBayEastBox <-  filter(transects, Region_name %in% EastBox)

transects_ScotsBayMain
transects_ScotsBayNorthBox
transects_ScotsBayEastBox

#Main Box
transects_ScotsBayMain
#fix_distance_topline(transects_ScotsBayMain, topline_broken=T)

area<- area_calc(transects_ScotsBayMain)

map_area_ob_SB <- map_area_buffered(transects_ScotsBayMain, transectEastWest = T)
map_area_ob_SB[[1]]

x <- transects_ScotsBayMain
transectEastWest = T

defaultW <- getOption("warn") # turn off warnings so people don't freak out 
options(warn = -1) #same as above 
crs_use <- st_crs(land.all) # type of projection

Latitude  <- c(x$Lat_S, x$Lat_E) #brings in lat start and end for each transect
Longitude  <- c(x$Lon_S, x$Lon_E) #brings in long start and end for each transect
LatLonMat <- matrix(c( Latitude,Longitude), ncol = 2) #building polygon
colnames(LatLonMat) <- c("latitude","longitude")
LatLondf<-as.data.frame(LatLonMat)
df<-sort_points(LatLondf)
df_t<-rbind(df,df[1,]) #connecting last dot to first.

#wonky polygon but used to find centroid
transect_p <-st_polygon(list(matrix( c( df_t$longitude,df_t$latitude), nrow = length(df_t$longitude), ncol=2))) #setup to create polygon
transect_p_poly <- st_sfc(transect_p, crs = crs_use) #adds the projection to appropriately calculate the area
transect_p_attrib <- data.frame(name = paste0(surveydate)) #adds the attribute survey date
transect_p_sf <- st_sf(transect_p_attrib, geometry = transect_p_poly, crs = crs_use) #creates sf polygon

centy <- st_transform(transect_p_sf, crs_use) %>%  #find the center of the polygon
  st_centroid() %>% 
  # this is the crs from d, which has no EPSG code:
  st_transform(., "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  # since you want the centroids in a second geometry col:
  st_geometry()

centy<-unlist(centy) #give you the centroid of the polygon, so you can sort points E and W, or N and S

# Arrange by angle
if (transectEastWest) { # sorts the points to East and West
  
  West <- subset(df, longitude < centy[1])
  East <-subset(df, longitude >= centy[1])
  West<-West[order(West$latitude),]
  West$ordered <-West$latitude*West$longitude
  West<-West[order(West$ordered, decreasing=TRUE),] # manually sort. Refer to example where we manually sort.
  #West<-West[c(2,1,3:nrow(West)),]
  
  East<-East[order(East$latitude, decreasing=TRUE),]
  East$ordered <-East$latitude*East$longitude
  East<-East[order(East$ordered, decreasing=FALSE),]
  
  
  East_point <- st_as_sf(East,coords = c("longitude","latitude"),remove = F, crs = crs_use)
  West_point <- st_as_sf(West,coords = c("longitude","latitude"),remove = F, crs = crs_use)
  East_point_distance <- East_point %>% dplyr::mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
  )
  East_point_distance<-East_point_distance$dist[!is.na( East_point_distance$dist)]
  West_point_distance <- West_point %>% dplyr::mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
  )
  West_point_distance<-West_point_distance$dist[!is.na( West_point_distance$dist)]
  distance_between_transects<-c(East_point_distance, West_point_distance)
  d <- (mean(as.numeric(distance_between_transects)))/2
  FBS_east <- finalBearing(as(East_point[1,], "Spatial"),  as(East_point[length(East_point$latitude),], "Spatial")) #find the buffer GIS point for SouthEast
  FBS_east_point <- destPoint(as(East_point[length(East_point$latitude),], "Spatial"),  FBS_east, d)
  FBN_east <- finalBearing( as(East_point[length(East_point$latitude),], "Spatial"), as(East_point[1,], "Spatial"))
  FBN_east_point <- destPoint(as(East_point[1,], "Spatial"),  FBN_east, d)
  
  FBN_west <- finalBearing(as(West_point[1,], "Spatial"),  as(West_point[length(West_point$latitude),], "Spatial"))
  FBN_west_point <- destPoint(as(West_point[length(West_point$latitude),], "Spatial"), FBN_west, d)
  FBS_west <- finalBearing( as(West_point[length(West_point$latitude),], "Spatial"), as(West_point[1,], "Spatial"))
  FBS_west_point <- destPoint(as(West_point[1,], "Spatial"),  FBS_west, d)
  
  df2<- do.call("rbind", list(
    data.frame(
      longitude = FBS_west_point[1, 1],
      latitude = FBS_west_point[1, 2],
      ordered = FBS_west_point[1, 1] * FBS_west_point[1, 2]
    ),
    West,
    data.frame(
      longitude = FBN_west_point[1, 1],
      latitude = FBN_west_point[1, 2],
      ordered = FBN_west_point[1, 1] * FBN_west_point[1, 2]
    ),
    data.frame(
      longitude = FBN_east_point[1, 1],
      latitude = FBN_east_point[1, 2],
      ordered = FBN_east_point[1, 1] * FBN_east_point[1, 2]
    ),
    East,
    data.frame(
      longitude = FBS_east_point[1, 1],
      latitude = FBS_east_point[1, 2],
      ordered = FBS_east_point[1, 1] * FBS_east_point[1, 2]
    )
  ))
  
  df2 <- rbind(df2,df2[1,])
  
} else {
  
  South <- subset(df, latitude < centy[2])
  North <-subset(df, latitude >= centy[2])
  South<-South[order(South$longitude),]
  North<-North[order(North$longitude, decreasing=TRUE),]
  df2<-rbind(South,North)
  df2 <- rbind(df2,df2[1,])
  
  South_point <- st_as_sf(South,coords = c("longitude","latitude"),remove = F, crs = crs_use)
  North_point <- st_as_sf(North,coords = c("longitude","latitude"),remove = F, crs = crs_use)
  
  South_point_distance <- South_point %>% dplyr::mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
  )
  South_point_distance<-South_point_distance$dist[!is.na( South_point_distance$dist)]
  North_point_distance <- North_point %>% dplyr::mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
  )
  North_point_distance<-North_point_distance$dist[!is.na( North_point_distance$dist)]
  
  distance_between_transects<-c(South_point_distance, North_point_distance)
  d <- (mean(as.numeric(distance_between_transects)))/2
  
  FBW_south <- finalBearing(as(South_point[1,], "Spatial"),  as(South_point[length(South_point$latitude),], "Spatial"))
  FBW_south_point <- destPoint(as(South_point[length(South_point$latitude),], "Spatial"),  FBW_south, d)
  FBE_south <- finalBearing( as(South_point[length(South_point$latitude),], "Spatial"), as(South_point[1,], "Spatial"))
  FBE_south_point <- destPoint(as(South_point[1,], "Spatial"),  FBE_south, d)
  
  FBE_north <- finalBearing(as(North_point[1,], "Spatial"),  as(North_point[length(North_point$latitude),], "Spatial"))
  FBE_north_point <- destPoint(as(North_point[length(North_point$latitude),], "Spatial"), FBE_north, d)
  FBW_north <- finalBearing( as(North_point[length(North_point$latitude),], "Spatial"), as(North_point[1,], "Spatial"))
  FBW_north_point <- destPoint(as(North_point[1,], "Spatial"),  FBW_north, d)
  
  df2<- do.call("rbind", list(
    data.frame(
      longitude = FBE_south_point[1, 1],
      latitude = FBE_south_point[1, 2]
      
    ),
    South,
    data.frame(
      longitude = FBW_south_point[1, 1],
      latitude = FBW_south_point[1, 2]
      
    ),
    data.frame(
      longitude = FBW_north_point[1, 1],
      latitude = FBW_north_point[1, 2]
      
    ),
    North,
    data.frame(
      longitude = FBE_north_point[1, 1],
      latitude = FBE_north_point[1, 2]
      
    )
  ))
  df2 <- rbind(df2,df2[1,])
}

transect_p2 <-st_polygon(list(matrix( c( df2$longitude,df2$latitude), nrow = length(df2$longitude), ncol=2))) #builds polygon with buffer points
transect_p_poly2 <- st_sfc(transect_p2, crs = crs_use)
transect_p_attrib2 <- data.frame(name = paste0(surveydate))
transect_p_sf2 <- st_sf(transect_p_attrib2, geometry = transect_p_poly2)

transects_b<-list()
for(i in 1:length(x$Region_name)){
  transects_b[[i]]<-matrix(c(x$Lon_S[i], x$Lon_E[i], x$Lat_S[i], x$Lat_E[i]), ncol=2) 
}
transect_l  <-st_multilinestring(transects_b)
transect_l_line <- st_sfc(transect_l, crs = crs_use)
transect_l_attrib <- data.frame(name = paste0(surveydate))
transect_l_sf <- st_sf(transect_l_attrib, geometry = transect_l_line)
polygon_bbox <-st_bbox(transect_p_sf2)

theme_set(theme_bw())
p <- ggplot(data = transect_p_sf2) +
  geom_sf( color = "black", alpha = 0.3, fill = "tomato") +
  geom_sf(data = transect_l_sf, color = "red") +
  geom_sf(data= land.all, color = "black") +
  coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 
p
a<-st_area(transect_p_sf2)
a<-set_units(a, km^2)
a<-as.numeric(a)
sf::sf_use_s2(FALSE)
survey_box_land_substract <- st_intersection(transect_p_sf2, land.all)
a_sub<-st_area(survey_box_land_substract)
a_sub<-set_units(a_sub, km^2)
a_sub<-as.numeric(a_sub)

huate_check<-st_as_sfc(c("POINT(-64.996 45.2453333)"),crs = crs_use) %>%
  st_sf(ID = paste0("Haute Isle"))
inside_poly <-st_intersection(huate_check, transect_p_sf2)

if ( sjmisc::is_empty(inside_poly) ) {
  a<-a
} else{ a<-(a- 1.61) }
if ( sjmisc::is_empty(survey_box_land_substract) ) {
  a<-a
} else{ a<-(a- a_sub) }

options(warn = defaultW)
map_area_ob_SB  <- list(p,as.numeric(a),d, transect_p_sf2, transect_l_sf)

map_area_ob_SB[[1]]
area_MB<-map_area_ob_SB[[2]]

ggsave(paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year, "/Plots/Area_ScotsBay_Main.png"),
       map_area_ob_SB[[1]],width=7,height=7,units="in") 

#North Box
area<-area_calc(transects_ScotsBayNorthBox)
area
map_area_ob_NB <- map_area_buffered(transects_ScotsBayNorthBox, transectEastWest = T)
map_area_ob_NB[[1]]
area_NB <- map_area_ob_NB[[2]]
area_NB

ggsave(paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year, "/Plots/Area_ScotsBay_North.png"),
                      map_area_ob_NB[[1]],width=7,height=7,units="in") 

#Check for overlap between buffered areas for Scots' Bay and North box
polygon_bbox <-st_bbox(map_area_ob_SB[[4]])
polygon_bbox
overlapPlot <- map_area_ob_SB[[1]]+
  geom_sf(data = map_area_ob_NB[[4]], color = "black", alpha = 0.3, fill = "tomato") +
  geom_sf(data = map_area_ob_NB[[5]], color = "red") +
  geom_sf(data= land.all, color = "black") +
  coord_sf(xlim = c((polygon_bbox[1]-0.1), polygon_bbox[3]+0.1), ylim = c((polygon_bbox[2]-0.1),(polygon_bbox[4]+0.1))) 

overlapPlot

overlapPlot <- ggsave(paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year, "/Plots/Area_ScotsBay_overlap.png"),
                      overlapPlot,width=7,height=7,units="in")

intersecting_points<-st_intersection(map_area_ob_SB[[4]],map_area_ob_NB[[4]])
ggplot(data = intersecting_points) +
  geom_sf( color = "black", alpha = 0.3, fill = "tomato") 

sf::sf_use_s2(FALSE)
overlap_area<-st_area(intersecting_points)
overlap_area<-set_units(overlap_area, km^2)
overlap_area<-as.numeric(overlap_area)

area_MB <-   area_MB -  ((if(length(overlap_area) ==0) {0} else {overlap_area} )/2)
area_NB <-   area_NB -  ((if(length(overlap_area) ==0) {0} else {overlap_area} )/2)

#East Box
area<-area_calc(transects_ScotsBayEastBox)
area
map_area_ob <- map_area_buffered(transects_ScotsBayEastBox, transectEastWest = T)
map_area_ob[[1]]
area_EB <- map_area_ob[[2]]
ScotsBayEastBox <- biomassCalc(transects_ScotsBayEastBox, area, TS38, TS50, TS75, TS120)
ScotsBayEastBox
# 
transects_ScotsBayMain

transects_ScotsBayMain <- fix_distance_customline(transects_ScotsBayMain, topline_broken = TRUE, " MS_T01")

ScotsBayMain <- biomassCalc(transects_ScotsBayMain, area_MB, TS38, TS50, TS75, TS120)
ScotsBayMain

ScotsBayNorthBox <- biomassCalc(transects_ScotsBayNorthBox, area_NB, TS38, TS50, TS75, TS120)
ScotsBayNorthBox

ScotsBayEastBox <- biomassCalc(transects_ScotsBayEastBox, area_EB, TS38, TS50, TS75, TS120)
ScotsBayEastBox

ScotsBayMain$SurveyArea <- rep("ScotsBayMainBox",length(ScotsBayMain[,1]))
ScotsBayNorthBox$SurveyArea <- rep("ScotsBayNorthBox",length(ScotsBayNorthBox[,1]))
ScotsBayEastBox$SurveyArea <- rep("ScotsBayEastBox",length(transects_ScotsBayEastBox[,1]))
ScotsBay <-do.call("rbind", list(ScotsBayMain,ScotsBayNorthBox,ScotsBayEastBox))

ScotsBay$meanSa_added <- rep(10 * log10(sum(c(ScotsBayMain$calc_actual_mean_sa,ScotsBayNorthBox$calc_actual_mean_sa,ScotsBayEastBox$calc_actual_mean_sa))), length(ScotsBay[,1])) #Edit Accordingly
ScotsBay$areaKm_added <- rep(sum(ScotsBayMain$areaKm[1],ScotsBayNorthBox$areaKm[1],ScotsBayEastBox$areaKm[1]), length(ScotsBay[,1]))   #Edit Accordingly

ScotsBay$total_biomass_added <- rep(ScotsBayMain$total_biomass[1]+ScotsBayNorthBox$total_biomass[1]+ScotsBayEastBox$total_biomass[1], length(ScotsBay[,1]))   #Edit Accordingly
ScotsBay$TransCount_added <- rep(ScotsBayMain$TransCount[1]+ScotsBayNorthBox$TransCount[1]+ScotsBayEastBox$TransCount[1], length(ScotsBay[,1]))   #Edit Accordingly
ScotsBay$Variance_added <- rep(ScotsBayMain$Variance[1]+ScotsBayNorthBox$Variance[1]+ScotsBayEastBox$Variance[1], length(ScotsBay[,1]))   #Edit Accordingly

ScotsBay$Reader <- rep("Darren",length(ScotsBay[,1]))

### Create Biomass folder
write.csv(ScotsBay, paste0("C:/Users/", Sys.info()[7], "/Desktop/Echoview/Surveys/", year, "/", surv, surv.no, " ", day, "-", month, "-", year,"/","", ".csv"))

##Previous DFO Paths/Lines/etc
#ggsave(paste0("E:/Acoustic Index Review Files/Figures/",year(as.Date(surveydate)),"/SB/","Area_ScotsBay_North",surveydate,".png"),map_area_ob_NB[[1]],width=7,height=7,units="in")  
#ggsave(paste0("E:/Acoustic Index Review Files/Figures/",year(as.Date(surveydate)),"/SB/","Area_ScotsBay_Main",surveydate,".png"),map_area_ob_SB[[1]],width=7,height=7,units="in")  
#ScotsBay$Reader <- rep("Allan",length(ScotsBay[,1]))
#ScotsBay$Reader <- rep("Tiffany",length(ScotsBay[,1]))
#ScotsBay$Reader <- rep("Claire",length(ScotsBay[,1]))
#ScotsBay$Reader <- rep("Jenna",length(ScotsBay[,1]))

#write.csv(ScotsBay, paste0("E:/Acoustic Index Review Files/Biomass Estimates Versions/Estimated using current Rscripts/2024/Scots Bay/",surveydate,"" ,".csv"))
#overlapPlot <- ggsave(paste0("E:/Acoustic Index Review Files/Figures/",year(as.Date(surveydate)),"/SB/","Area_ScotsBay_overlap",surveydate,".png"),overlapPlot,width=7,height=7,units="in")  

# area_NB <- map_area_ob_SB[[2]]
