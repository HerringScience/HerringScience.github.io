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

setwd("D:/4VWXHER Acoustics/Jenna EVs/German Bank/Integration/")

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

transects_German <- filter(transects, Lat_S <43.67 & Lat_E < 43.67 & Lon_S < -66.259 & Lon_E < -66.259)
transects_SealIsland <-  filter(transects, !Region_name %in% as.character(transects_German$Region_name))

transects_German
transects_SealIsland

#### German Bank #### 

#calculate area of survey
Longitude  <-c(transects_German$Lon_S,transects_German$Lon_E)
Latitude  <-c(transects_German$Lat_S, transects_German$Lat_E)
LatLonMat <-matrix(c(Longitude,Latitude),ncol=2)
survey_coord <-SpatialPoints(LatLonMat)
crs.geo <- CRS("+proj=utm +zone=20 +datum=WGS84")

proj4string(survey_coord) <- crs.geo
is.projected(survey_coord)
summary(survey_coord)

survey_area <- mcp(survey_coord, percent = 100)
area<-survey_area$area*100000000 #Converts to km squared. originally in Hectares, but not sure why it's million too small.
area

colnames(transects_German)[colnames(transects_German) == 'Area_Backscatter_Strength'] <- 'ABS'
#calculate transect length
transects_German$length <- (transects_German$Dist_E - transects_German$Dist_S)/1000

#add target strength
transects_German$TS <- as.numeric(ifelse(grepl(38, transects_German$Frequency), TS38, TS50))


sumTransect <- sum(transects_German$length)
sumTransect
transects_German$weight <- transects_German$length/sumTransect
sum(transects_German$weight) #should equal 1

transects_German$unadjustedkgm2 <- 10^((transects_German$ABS-(transects_German$TS))/10)
transects_German$unadjustedkgm2
transects_German$weighted_biomass <- (transects_German$unadjustedkgm2)*(transects_German$weight)
transects_German$weighted_biomass


#write.csv(transects_German,'transects_German.csv')

transects_German$biomass <- area*transects_German$weighted_biomass*1000
transects_German$biomass
transects_German$biomass[is.na(as.integer(transects_German$biomass))] <- 0 
transects_German$biomass

transects_German

sum(transects_German$biomass)

setwd("D:/4VWXHER Acoustics/BlindComparisons/")

write.csv(transects_German, "German_Jenna_Sept13.csv")

#### SEAL ISLAND #### 
#OR outside of box


#calculate area of survey
Longitude  <-c(transects_SealIsland$Lon_S,transects_SealIsland$Lon_E)
Latitude  <-c(transects_SealIsland$Lat_S, transects_SealIsland$Lat_E)
LatLonMat <-matrix(c(Longitude,Latitude),ncol=2)
survey_coord <-SpatialPoints(LatLonMat)
crs.geo <- CRS("+proj=utm +zone=20 +datum=WGS84")

proj4string(survey_coord) <- crs.geo
is.projected(survey_coord)
summary(survey_coord)

survey_area <- mcp(survey_coord, percent = 100)
area<-survey_area$area*100000000 #Converts to km squared. originally in Hectares, but not sure why it's million too small.


colnames(transects_SealIsland)[colnames(transects_SealIsland) == 'Area_Backscatter_Strength'] <- 'ABS'
#calculate transect length
transects_SealIsland$length <- (transects_SealIsland$Dist_E - transects_SealIsland$Dist_S)/1000

#add target strength
transects_SealIsland$TS <- as.numeric(ifelse(grepl(38, transects_SealIsland$Frequency), TS38, TS50))


sumTransect <- sum(transects_SealIsland$length)
sumTransect
transects_SealIsland$weight <- transects_SealIsland$length/sumTransect
sum(transects_SealIsland$weight) #should equal 1

transects_SealIsland$unadjustedkgm2 <- 10^((transects_SealIsland$ABS-(transects_SealIsland$TS))/10)
transects_SealIsland$unadjustedkgm2
transects_SealIsland$weighted_biomass <- (transects_SealIsland$unadjustedkgm2)*(transects_SealIsland$weight)
transects_SealIsland$weighted_biomass


#write.csv(transects_SealIsland,'transects_SealIsland.csv')

transects_SealIsland$biomass <- area*transects_SealIsland$weighted_biomass*1000
transects_SealIsland$biomass
transects_SealIsland$biomass[is.na(as.integer(transects_SealIsland$biomass))] <- 0 
transects_SealIsland$biomass

transects_SealIsland

sum(transects_SealIsland$biomass)


setwd("D:/4VWXHER Acoustics/BlindComparisons/")

write.csv(transects_SealIsland, "SealIsland_Jenna_Sept13.csv")





