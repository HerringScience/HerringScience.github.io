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
library(lubridate)
library(zoo)
library(psych)

### German Bank ###

source("D:/4VWXHER Acoustics/BlindComparisons/Acoustic_biomass functions.R")

setwd("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/")
bioFiles = dir("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates", full.names = T, pattern = '.csv$')
bioFiles <-
  grep("German_Allan", bioFiles, value = TRUE, fixed = TRUE) #really odd R bug, had to code it this way.

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}

Surveys <-
  do.call("rbind", list(Survey_1, Survey_2, Survey_3, Survey_4, Survey_5)) #Need to think of something more elegant

Surveys$Date <-
  as.Date(paste0(
    left(Surveys$Date_S, 4),
    "-",
    left(right(Surveys$Date_S, 4), 2),
    "-",
    right(Surveys$Date_S, 2)
  ))
Surveys$Date <- as.Date(Surveys$Date)

Surveys$total_biomass

y_intercept <- 0.199392662629964
x_Var_1 <-0.528381832773883
daysturnover <- 31
Date <- Surveys$Date
Survey <- 1:length(Surveys$Date)
Biomass <- Surveys$total_biomass

turnoverBio(y_intercept, x_Var_1, daysturnover, Date, Survey, Biomass)

GermanBank<- Surveys

GermanBank$YEAR <- as.numeric(year(GermanBank$Date))
GermanBank$S_GROUND <- rep("GB", length(GermanBank$YEAR))

### SCOTS BAY ###


setwd("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/")
bioFiles = dir("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates", full.names = T, pattern = '.csv$')
bioFiles <-
  grep("ScotsBay_Allan_2020", bioFiles, value = TRUE, fixed = TRUE) #really odd R bug, had to code it this way.

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}

Surveys <-
  do.call("rbind", list(Survey_1, Survey_2, Survey_3, Survey_4, Survey_5, Survey_6,Survey_7,Survey_8,Survey_9)) #Need to think of something more elegant

Surveys$Date <-
  as.Date(paste0(
    left(Surveys$Date_S, 4),
    "-",
    left(right(Surveys$Date_S, 4), 2),
    "-",
    right(Surveys$Date_S, 2)
  ))
Surveys$Date <- as.Date(Surveys$Date)

Surveys$total_biomass

y_intercept <- 0.364102758434224
x_Var_1 <-0.436969270679439
daysturnover <- 29
Date <- Surveys$Date
Survey <- 1:length(Surveys$Date)
Biomass <- Surveys$total_biomass


turnoverBio(y_intercept, x_Var_1, daysturnover, Date, Survey, Biomass)




### SCOTS BAY INBOX ONLY


setwd("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/")
bioFiles = dir("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates", full.names = T, pattern = '.csv$')
bioFiles <-
  grep("ScotsBay_Allan_Inbox", bioFiles, value = TRUE, fixed = TRUE) #really odd R bug, had to code it this way.

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}

Surveys <-
  do.call("rbind", list(Survey_1, Survey_2, Survey_3, Survey_4, Survey_5, Survey_6,Survey_7,Survey_8,Survey_9)) #Need to think of something more elegant

Surveys$Date <-
  as.Date(paste0(
    left(Surveys$Date_S, 4),
    "-",
    left(right(Surveys$Date_S, 4), 2),
    "-",
    right(Surveys$Date_S, 2)
  ))
Surveys$Date <- as.Date(Surveys$Date)

Surveys$total_biomass

y_intercept <- 0.364102758434224
x_Var_1 <-0.436969270679439
daysturnover <- 29
Date <- Surveys$Date
Survey <- 1:length(Surveys$Date)
Biomass <- Surveys$total_biomass


turnoverBio(y_intercept, x_Var_1, daysturnover, Date, Survey, Biomass)




192342+101983





gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}



#Calculate 3 yr Mean
x<- c(308000,234520,280470,294325)

#Geometric mean used for index
zoo::rollapply(x, 3, geometric.mean, fill=NA, align="right")

#zoo::rollapply(x, 3, mean, fill=NA, align="right")
zoo::rollapply(x, 3, mean, fill=NA, align="right")


gm_mean(x)
mean(x)

#Seal Island

setwd("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/")
bioFiles = dir("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates", full.names = T, pattern = '.csv$')
bioFiles <-
  grep("SealIsland_Allan", bioFiles, value = TRUE, fixed = TRUE) #really odd R bug, had to code it this way.

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}

Surveys <-
  do.call("rbind", list(Survey_1, Survey_2, Survey_3, Survey_4, Survey_5)) #Need to think of something more elegant

Surveys$Date <-
  as.Date(paste0(
    left(Surveys$Date_S, 4),
    "-",
    left(right(Surveys$Date_S, 4), 2),
    "-",
    right(Surveys$Date_S, 2)
  ))
Surveys$Date <- as.Date(Surveys$Date)

Surveys$total_biomass

Date <- Surveys$Date
Survey <- 1:length(Surveys$Date)
Biomass <- Surveys$total_biomass

sum(Biomass) #whats reported for Seal Island

SealIsland <- Surveys

SealIsland$YEAR <- as.numeric(year(SealIsland$Date))
SealIsland$S_GROUND <- rep("SI", length(SealIsland$YEAR))




#Eastern Shore
rm(list = ls())

  setwd("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Gillnets/ES/")
bioFiles = dir("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Gillnets/ES",  pattern = '.csv$')

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}


total_biomass <-
  c(fileData[[1]][1, 25], 
    fileData[[2]][1, 25], 
    fileData[[3]][1, 25], 
    fileData[[4]][1, 25], 
    fileData[[5]][1, 25],
    fileData[[6]][1, 25],
    fileData[[7]][1, 25],
    fileData[[8]][1, 25])

sum(total_biomass)





#Little Hope
rm(list = ls())

setwd("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Gillnets/LH/")
bioFiles = dir("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Gillnets/LH",  pattern = '.csv$')

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}


total_biomass <-
  c(fileData[[1]][1, 25], 
    fileData[[2]][1, 25], 
    fileData[[3]][1, 25], 
    fileData[[4]][1, 25])

sum(total_biomass)



#Spec Buoy
rm(list = ls())
setwd("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Gillnets/SP/")
bioFiles = dir("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Gillnets/SP",  pattern = '.csv$')

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}


total_biomass <-
  c(fileData[[1]][1, 25], 
    fileData[[2]][1, 25], 
    fileData[[3]][1, 25], 
    fileData[[4]][1, 25])

sum(total_biomass)


192342+101983 +4487.629

#Trinity
rm(list = ls())
setwd("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Gillnets/TR/")
bioFiles = dir("D:/4VWXHER Acoustics/Allan 2020/Biomass Estimates/Gillnets/TR",  pattern = '.csv$')

fileData <- list()
for (i in 1:length(bioFiles)) {
  fileData[[i]] <- read.csv(bioFiles[i])
  assign(paste("Survey_", i, sep = ""), fileData[[i]][1, ])
}


total_biomass <-
  c(fileData[[1]][1, 25], 
    fileData[[2]][1, 25], 
    fileData[[3]][1, 25], 
    fileData[[4]][1, 25])

sum(total_biomass)

192342+101983
294+12+4

192342+101983 +4487.629 + 12468.09
