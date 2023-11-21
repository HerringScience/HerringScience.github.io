#ANOVA Survey Factors Investigations_Scots Bay Tides

library(rlang)
library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
library(weathercan)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
library(rgeos)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(readxl)
library(hms)
library(measurements)
library(ggplot2)
library(maps)
library(dplyr)
library(RColorBrewer)
library(AICcmodavg)
library(datasets)
library(multcompView)

###remove everything in environment
rm(list = ls())

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")

Survey_Factors <- read_csv("surveyFactorsAll_Tracey with SSB data.csv")

Survey_Data <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv")

#
##
### SCOTS BAY
##
#

### SCOTS BAY Julian Date and Survey Biomass

JulianAndBiomass <- subset(Survey_Factors, select=c("Survey_Date", "Survey_Area", "Julian", "DFO_Estimate"))
  JulianAndBiomass <- na.omit(JulianAndBiomass)
  JulianAndBiomass<- subset(JulianAndBiomass, Survey_Date < '2023-05-22') #As only one DFO factor for 2023 is in the system, removed to keep years nice.

ScotsBay_Turnover <- subset(JulianAndBiomass, Survey_Area=='SB' )

SBPointGraph <- ggplot(ScotsBay_Turnover, aes(Julian, DFO_Estimate)) +geom_smooth() + geom_point() 
  print(SBPointGraph + labs(y = "Survey Biomass (mt)", x = "Julian Day"))

OneWayJB <- aov(DFO_Estimate ~ Julian, data = ScotsBay_Turnover)
  summary(OneWayJB)

### SCOTS BAY Number of Vessels and Survey Biomass

SBVesselsBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Julian", "DFO_Estimate", "Survey_Area"))
  SBVesselsBiomass <- na.omit(SBVesselsBiomass)
  SBVesselsBiomass <- subset(SBVesselsBiomass, Survey_Area == "SB", Survey_Date < '2023-05-22')
  SBVesselsBiomass$No_of_Vessels <- as.factor(SBVesselsBiomass$No_of_Vessels)

SBBoxplotVessels <-boxplot(SBVesselsBiomass$DFO_Estimate~SBVesselsBiomass$No_of_Vessels, xlab="Number of Vessels in Survey", ylab="Survey Biomass (mt)")

SBOneWayVessels <- aov(DFO_Estimate ~ No_of_Vessels, data = SBVesselsBiomass)
  summary(SBOneWayVessels)
                           
###Scots Bay Survey Area, Julian, Survey Time, High Tide Time, Survey Biomass
                           
SurveyAreaTimeTideBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Survey_Area","Year", "Julian", "DFO_Estimate", "Survey_Start", "High_Tide", "Tide_Difference", "Tide_Relative" ))
  SurveyAreaTimeTideBiomass <- na.omit(SurveyAreaTimeTideBiomass)
                           
ScotsBayHighTideBiomass <- subset(SurveyAreaTimeTideBiomass, Survey_Area=="SB")
  ScotsBayHighTideBiomass <- subset(ScotsBayHighTideBiomass, Survey_Date < '2023-05-22')
  ScotsBayHighTideBiomass$Tide_Relative <- as.numeric(ScotsBayHighTideBiomass$Tide_Relative)
                           
SBTideRelativePoint <- ggplot(ScotsBayHighTideBiomass, aes(x=Tide_Relative, y=DFO_Estimate)) + geom_point(aes(group= Tide_Relative)) +geom_smooth() + geom_hline(yintercept=mean(ScotsBayHighTideBiomass$DFO_Estimate))
  print(SBTideRelativePoint + labs(y="Survey Biomass", x = "Tide Relative to Survey Start in Hours"))
                           
SBOneWayTideRelative <- aov(DFO_Estimate ~ Tide_Relative, data = ScotsBayHighTideBiomass)
  summary(SBOneWayTideRelative)
  
  
#
##
### GERMAN BANK
##
#
  
###German Bank Julian Date and Survey Biomass
  
JulianAndBiomass <- subset(Survey_Factors, select=c("Survey_Date", "Survey_Area", "Julian", "DFO_Estimate"))
  JulianAndBiomass <- na.omit(JulianAndBiomass)
  JulianAndBiomass<- subset(JulianAndBiomass, Survey_Date < '2023-05-22') #As only one DFO factor for 2023 is in the system, removed to keep years nice.
  
GermanBankTurnover <- subset(JulianAndBiomass, Survey_Area=='GB' )
  
GBPointGraph <- ggplot(GermanBankTurnover, aes(Julian, DFO_Estimate)) +geom_smooth() + geom_point() 
  print(GBPointGraph + labs(y = "Survey Biomass (mt)", x = "Julian Day"))

GBOneWayJB <- aov(DFO_Estimate ~ Julian, data = GermanBankTurnover)
  summary(GBOneWayJB)
  

### German Bank Number of Vessels and Survey Biomass
VesselsBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Julian", "DFO_Estimate", "Survey_Area"))
  VesselsBiomass <- na.omit(VesselsBiomass)
  
GBVesselsBiomass <- subset(VesselsBiomass, Survey_Area == "GB", Survey_Date < '2023-05-22')
  GBVesselsBiomass$No_of_Vessels <- as.factor(GBVesselsBiomass$No_of_Vessels)
  
GBBoxplotVessels <-boxplot(GBVesselsBiomass$DFO_Estimate~GBVesselsBiomass$No_of_Vessels, xlab="Number of Vessels in Survey", ylab="Survey Biomass (mt)")
  
GBOneWayVessels <- aov(DFO_Estimate ~ No_of_Vessels, data = GBVesselsBiomass)
  summary(GBOneWayVessels)
  
#GBSurveyTideDifference.two.way <- aov(DFO_Estimate ~ No_of_Vessels*Julian, data = GermanBank_HighTideBiomass)
  #summary(GBSurveyTideDifference.two.way)
  
###German Bank Survey Area, Julian, Survey Time, High Tide Time, DFO_Estimate
SurveyAreaTimeTideBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Survey_Area","Year", "Julian", "DFO_Estimate", "Survey_Start", "High_Tide", "Tide_Difference", "Tide_Relative" ))
  SurveyAreaTimeTideBiomass <- na.omit(SurveyAreaTimeTideBiomass)
  
GermanBankHighTideBiomass <- subset(SurveyAreaTimeTideBiomass, Survey_Area=="GB")
  GermanBankHighTideBiomass <- subset(GermanBankHighTideBiomass, Survey_Date < '2023-05-22')
  GermanBankHighTideBiomass$Tide_Relative <- as.numeric(GermanBankHighTideBiomass$Tide_Relative)
  
GBTideRelativePoint <- ggplot(GermanBankHighTideBiomass, aes(x=Tide_Relative, y=DFO_Estimate)) + geom_point(aes(group= Tide_Relative)) +geom_smooth() + geom_hline(yintercept=mean(GermanBankHighTideBiomass$DFO_Estimate))
  print(GBTideRelativePoint + labs(y="Survey Biomass", x = "Tide Relative to Survey Start (hrs)"))
  
GBSurveyTideDifference.one.way <- aov(DFO_Estimate ~ Tide_Relative, data = GermanBankHighTideBiomass)
  summary(GBSurveyTideDifference.one.way)