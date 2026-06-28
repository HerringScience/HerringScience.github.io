#### biomass calculation from transects exported from Echoview into excel
#Integrations must be placed in same folder and seperated (e.g. GB and SI)

rm(list = ls())

library(dplyr)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(adehabitatHR)
library(plotKML)
library(maptools)

surveydate <- "2020-08-09"

#TARGET STRENGTH CALCULATION
SURV <-read.csv("D:/4VWXHER Acoustics/FinalBiomass/TS_est_2020.csv")
TS38 <- SURV$TS[SURV$DATE==surveydate]
TS50 <- SURV$TS[SURV$DATE==surveydate]-0.10727
TS75 <- SURV$TS[SURV$DATE==surveydate] -0.26575
TS120 <- SURV$TS[SURV$DATE==surveydate] -0.44946
setwd(paste0("D:/4VWXHER Acoustics/Allan 2020/Scots Bay/",surveydate, "/Integration/"))
source("D:/4VWXHER Acoustics/FinalBiomass/Acoustic_biomass functions.R")



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
transects$Boat <- substr(transects$Boat,1,2)
transects$Boat

transects <-filter(transects, !Boat %in% "SL")


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



outsideMain <- NULL #c(grep("MS",transects$Region_name, value = TRUE, fixed = TRUE))

#NorthBox <-c(grep("C1",transects$Region_name, value = TRUE, fixed = TRUE))

#EastBox <-c(grep("MS",transects$Region_name, value = TRUE, fixed = TRUE))

#NEED TO MANUAL EDIT FILTERS FOR SCOTS BAY UNTIL BOXES ARE DEFINED USING POLYGON SHAPES

transects_ScotsBayMain <- filter(transects, !Region_name %in% outsideMain)
#transects_ScotsBayNorthBox <-  filter(transects, Region_name %in% NorthBox)
#transects_ScotsBayEastBox <-  filter(transects, Region_name %in% EastBox)

transects_ScotsBayMain
#transects_ScotsBayNorthBox
#transects_ScotsBayEastBox

#Main Box
area<-area_calc(transects_ScotsBayMain)
area
ScotsBayMain <- biomassCalc(transects_ScotsBayMain, area, TS38, TS50, TS75, TS120)
ScotsBayMain

#North Box
#area<-area_calc(transects_ScotsBayNorthBox)
#area
#ScotsBayNorthBox <- biomassCalc(transects_ScotsBayNorthBox, area, TS38, TS50)
#ScotsBayNorthBox

#East Box
#area<-area_calc(transects_ScotsBayEastBox)
#area
#ScotsBayEastBox <- biomassCalc(transects_ScotsBayEastBox, area, TS38, TS50)
#ScotsBayEastBox

ScotsBay <-do.call("rbind", list(ScotsBayMain))
ScotsBay$SurveyArea <- rep("ScotsBay",length(ScotsBay[,1]))

ScotsBay$Reader <- rep("Allan",length(ScotsBay[,1]))
#ScotsBay$Reader <- rep("Claire",length(ScotsBay[,1]))
#ScotsBay$Reader <- rep("Jenna",length(ScotsBay[,1]))

### Create Biomass folder
setwd(paste0("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/"))

write.csv(ScotsBay, paste0("ScotsBay_Allan_",surveydate,".csv"))
#write.csv(ScotsBay, paste0("ScotsBay_Claire_",surveydate,".csv"))
#write.csv(ScotsBay, paste0("ScotsBay_Jenna_",surveydate,".csv"))


dir.create(paste0("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Transects/Allan/"))
write.csv(ScotsBay,paste0("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Transects/Allan/",surveydate,"SB_transects.csv"))
