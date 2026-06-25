#### biomass calculation from transects exported from Echoview into excel
#Integrations must be placed in same folder and seperated (e.g. GB and SI)

rm(list=ls())

library(dplyr)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(plotKML)

setwd('D:/4VWXHER Acoustics/Allan 2020/Scots Bay/2020-07-11/2020-07-11 - with descrimination/Integration/')

TS38 <- -35.5 ###### Code directly to Length Frequences EVENTUALLY ##########
TS50 <- -35.609 ###### Code directly to Length Frequences EVENTUALLY ##########

file_list <- list.files(path = getwd())
transects <- data.frame()

#identify columns to keep from csv files
keep <- c("Region_name","Lat_S","Lon_S","Lat_E","Lon_E","Dist_S","Dist_E","Area_Backscatter_Strength", "Frequency","Time_S","Time_E")

#for each file in file_list, read in csv, keep specified columns, rbind to dataset to produce one dataset
for (i in 1:length(file_list)){
  temp_data <- read.csv(file_list[i])
  temp_data2 <- temp_data[keep]
  transects <- rbind(transects, temp_data2)
}

rm(temp_data,temp_data2)
grep(cal_file_search[i], calFiles, value = TRUE, fixed = TRUE)

outsideMain <-c(grep("LJ",transects$Region_name, value = TRUE, fixed = TRUE),
  grep("C1",transects$Region_name, value = TRUE, fixed = TRUE))

NorthBox <-c(grep("LJ",transects$Region_name, value = TRUE, fixed = TRUE))

EastBox <-c(grep("C1",transects$Region_name, value = TRUE, fixed = TRUE))

#NEED TO MANUAL EDIT FILTERS FOR SCOTS BAY UNTIL BOXES ARE DEFINED USING POLYGON SHAPES

transects_ScotsBayMain <- filter(transects, !Region_name %in% outsideMain)
transects_ScotsBayNorthBox <-  filter(transects, Region_name %in% NorthBox)
transects_ScotsBayEastBox <-  filter(transects, Region_name %in% EastBox)

transects_ScotsBayMain
transects_ScotsBayNorthBox
transects_ScotsBayEastBox

#### Scots Bay Main Box #### 

#calculate area of survey
Longitude  <-c(transects_ScotsBayMain$Lon_S,transects_ScotsBayMain$Lon_E)
Latitude  <-c(transects_ScotsBayMain$Lat_S, transects_ScotsBayMain$Lat_E)
LatLonMat <-matrix(c(Longitude,Latitude),ncol=2)
survey_coord <-SpatialPoints(LatLonMat)
crs.geo <- CRS("+proj=utm +zone=20 +datum=WGS84")

proj4string(survey_coord) <- crs.geo
is.projected(survey_coord)
summary(survey_coord)

survey_area <- mcp(survey_coord, percent = 100)
area<-survey_area$area*100000000 #Converts to km squared. originally in Hectares, but not sure why it's million too small.
area

colnames(transects_ScotsBayMain)[colnames(transects_ScotsBayMain) == 'Area_Backscatter_Strength'] <- 'ABS'
#calculate transect length
transects_ScotsBayMain$length <- (transects_ScotsBayMain$Dist_E - transects_ScotsBayMain$Dist_S)/1000

#add target strength
transects_ScotsBayMain$TS <- as.numeric(ifelse(grepl(38, transects_ScotsBayMain$Frequency), TS38, TS50))


sumTransect <- sum(transects_ScotsBayMain$length)
sumTransect
transects_ScotsBayMain$weight <- transects_ScotsBayMain$length/sumTransect
sum(transects_ScotsBayMain$weight) #should equal 1

transects_ScotsBayMain$unadjustedkgm2 <- 10^((transects_ScotsBayMain$ABS-(transects_ScotsBayMain$TS))/10)
transects_ScotsBayMain$unadjustedkgm2
transects_ScotsBayMain$weighted_biomass <- (transects_ScotsBayMain$unadjustedkgm2)*(transects_ScotsBayMain$weight)
transects_ScotsBayMain$weighted_biomass


#write.csv(transects_ScotsBayMain,'transects_ScotsBayMain.csv')

transects_ScotsBayMain$biomass <- area*transects_ScotsBayMain$weighted_biomass*1000
transects_ScotsBayMain$biomass
transects_ScotsBayMain$biomass[is.na(as.integer(transects_ScotsBayMain$biomass))] <- 0 
transects_ScotsBayMain$biomass


#### North Box #### 
#calculate area of survey
Longitude  <-c(transects_ScotsBayNorthBox$Lon_S,transects_ScotsBayNorthBox$Lon_E)
Latitude  <-c(transects_ScotsBayNorthBox$Lat_S, transects_ScotsBayNorthBox$Lat_E)
LatLonMat <-matrix(c(Longitude,Latitude),ncol=2)
survey_coord <-SpatialPoints(LatLonMat)
crs.geo <- CRS("+proj=utm +zone=20 +datum=WGS84")

proj4string(survey_coord) <- crs.geo
is.projected(survey_coord)
summary(survey_coord)

survey_area <- mcp(survey_coord, percent = 100)
area<-survey_area$area*100000000 #Converts to km squared. originally in Hectares, but not sure why it's million too small.
area

colnames(transects_ScotsBayNorthBox)[colnames(transects_ScotsBayNorthBox) == 'Area_Backscatter_Strength'] <- 'ABS'
#calculate transect length
transects_ScotsBayNorthBox$length <- (transects_ScotsBayNorthBox$Dist_E - transects_ScotsBayNorthBox$Dist_S)/1000

#add target strength
transects_ScotsBayNorthBox$TS <- as.numeric(ifelse(grepl(38, transects_ScotsBayNorthBox$Frequency), TS38, TS50))


sumTransect <- sum(transects_ScotsBayNorthBox$length)
sumTransect
transects_ScotsBayNorthBox$weight <- transects_ScotsBayNorthBox$length/sumTransect
sum(transects_ScotsBayNorthBox$weight) #should equal 1

transects_ScotsBayNorthBox$unadjustedkgm2 <- 10^((transects_ScotsBayNorthBox$ABS-(transects_ScotsBayNorthBox$TS))/10)
transects_ScotsBayNorthBox$unadjustedkgm2
transects_ScotsBayNorthBox$weighted_biomass <- (transects_ScotsBayNorthBox$unadjustedkgm2)*(transects_ScotsBayNorthBox$weight)
transects_ScotsBayNorthBox$weighted_biomass


#write.csv(transects_ScotsBayNorthBox,'transects_ScotsBayNorthBox.csv')

transects_ScotsBayNorthBox$biomass <- area*transects_ScotsBayNorthBox$weighted_biomass*1000
transects_ScotsBayNorthBox$biomass
transects_ScotsBayNorthBox$biomass[is.na(as.integer(transects_ScotsBayNorthBox$biomass))] <- 0 
transects_ScotsBayNorthBox$biomass


#### Scots Bay East Box #### 

#calculate area of survey
Longitude  <-c(transects_ScotsBayEastBox$Lon_S,transects_ScotsBayEastBox$Lon_E)
Latitude  <-c(transects_ScotsBayEastBox$Lat_S, transects_ScotsBayEastBox$Lat_E)
LatLonMat <-matrix(c(Longitude,Latitude),ncol=2)
survey_coord <-SpatialPoints(LatLonMat)
crs.geo <- CRS("+proj=utm +zone=20 +datum=WGS84")

proj4string(survey_coord) <- crs.geo
is.projected(survey_coord)
summary(survey_coord)

survey_area <- mcp(survey_coord, percent = 100)
area<-survey_area$area*100000000 #Converts to km squared. originally in Hectares, but not sure why it's million too small.
area

colnames(transects_ScotsBayEastBox)[colnames(transects_ScotsBayEastBox) == 'Area_Backscatter_Strength'] <- 'ABS'
#calculate transect length
transects_ScotsBayEastBox$length <- (transects_ScotsBayEastBox$Dist_E - transects_ScotsBayEastBox$Dist_S)/1000

#add target strength
transects_ScotsBayEastBox$TS <- as.numeric(ifelse(grepl(38, transects_ScotsBayEastBox$Frequency), TS38, TS50))


sumTransect <- sum(transects_ScotsBayEastBox$length)
sumTransect
transects_ScotsBayEastBox$weight <- transects_ScotsBayEastBox$length/sumTransect
sum(transects_ScotsBayEastBox$weight) #should equal 1

transects_ScotsBayEastBox$unadjustedkgm2 <- 10^((transects_ScotsBayEastBox$ABS-(transects_ScotsBayEastBox$TS))/10)
transects_ScotsBayEastBox$unadjustedkgm2
transects_ScotsBayEastBox$weighted_biomass <- (transects_ScotsBayEastBox$unadjustedkgm2)*(transects_ScotsBayEastBox$weight)
transects_ScotsBayEastBox$weighted_biomass


#write.csv(transects_ScotsBayEastBox,'transects_ScotsBayEastBox.csv')

transects_ScotsBayEastBox$biomass <- area*transects_ScotsBayEastBox$weighted_biomass*1000
transects_ScotsBayEastBox$biomass
transects_ScotsBayEastBox$biomass[is.na(as.integer(transects_ScotsBayEastBox$biomass))] <- 0 
transects_ScotsBayEastBox$biomass

transects_ScotsBay <- rbind(transects_ScotsBayMain, transects_ScotsBayNorthBox)
transects_ScotsBay <- rbind(transects_ScotsBay, transects_ScotsBayEastBox)
transects_ScotsBay


setwd("D:/4VWXHER Acoustics/BlindComparisons/")
write.csv(transects_ScotsBay, "ScotsBay_Allan_July11.csv")



