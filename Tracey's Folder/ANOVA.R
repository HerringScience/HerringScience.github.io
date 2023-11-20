#Two Way ANOVA Survey Factors Investigations_Scots Bay Tides

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


### IMPORTANT : SET GROUND, YEAR, AND SURVEY # HERE
#surv="GB" #SB or GB
#surv2="German Bank" #"German Bank" or "Scots Bay" as written
#year="2023"
#surv.no="8"
#adhoc = "FALSE" #true or false if an adhoc survey was completed (and "adhoc.csv" exists)

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")

Survey_Factors <- read_csv("surveyFactorsAll_Tracey with SSB data.csv")

Survey_Data <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv")

###Julian Date and DFO_Estimates (Scots Bay)

JulianAndBiomass <- subset(Survey_Factors, select=c("Survey_Date", "Survey_Area", "Julian", "DFO_Estimate"))
    JulianAndBiomass <- na.omit(JulianAndBiomass)
    JulianAndBiomass<- subset(JulianAndBiomass, Survey_Date < '2023-05-22') #As only one DFO factor for 2023 is in the system, removed to keep years nice.
ScotsBay_Turnover <- subset(JulianAndBiomass, Survey_Area=='SB' )

OneWayJB <- aov(DFO_Estimate ~ Julian, data = ScotsBay_Turnover)
summary(OneWayJB)

Point_Graph <- ggplot(ScotsBay_Turnover, aes(Julian, DFO_Estimate)) +geom_smooth() + geom_point() 
print(Point_Graph + labs(y = "Survey Biomass (mt)", x = "Julian Day"))

###Number of Vessels and Survey biomass (Scots Bay)
VesselsBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Julian", "DFO_Estimate", "Survey_Area"))
    VesselsBiomass <- na.omit(VesselsBiomass)
    VesselsBiomass <- subset(VesselsBiomass, Survey_Area == "SB", Survey_Date < '2023-05-22')
    VesselsBiomass$No_of_Vessels <- as.factor(VesselsBiomass$No_of_Vessels)

GraphVessels <- ggplot(VesselsBiomass, aes(No_of_Vessels, DFO_Estimate))+ geom_point() + geom_smooth()
print(GraphVessels)

BoxplotVessels <-boxplot(VesselsBiomass$DFO_Estimate~VesselsBiomass$No_of_Vessels, main = "Biomass recorded per number of Vessels in Survey", xlab="Number of Vessels in Survey", ylab="Survey Biomass")
OneWayVessels <- aov(DFO_Estimate ~ No_of_Vessels, data = VesselsBiomass)
  
###Scots Bay Survey Area, Julian, Survey Time, High Tide Time, DFO_Estimate
SurveyAreaTimeTideBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Survey_Area","Year", "Julian", "DFO_Estimate", "Survey_Start", "High_Tide", "Tide_Difference", "Tide_Relative" ))
  SurveyAreaTimeTideBiomass <- na.omit(SurveyAreaTimeTideBiomass)
  ScotsBay_HighTideBiomass <- subset(SurveyAreaTimeTideBiomass, Survey_Area=="SB")
  ScotsBay_HighTideBiomass <- subset(ScotsBay_HighTideBiomass, Survey_Date < '2023-05-22')
  ScotsBay_HighTideBiomass$Tide_Relative <- as.numeric(ScotsBay_HighTideBiomass$Tide_Relative)


###Scot Bay Tide Relative Point Graph
Tide_Relative_Point <- ggplot(ScotsBay_HighTideBiomass, aes(x=Tide_Relative, y=DFO_Estimate)) + geom_point(aes(group= Tide_Relative)) +geom_smooth() + geom_hline(yintercept=mean(ScotsBay_HighTideBiomass$DFO_Estimate))
print(Tide_Relative_Point + labs(y="Survey Biomass", x = "Tide Relative to Survey Start in Hours"))

###ANOVAS

#SurveyTideDifference.two.way <- aov(DFO_Estimate ~ Survey_Start*Tide_Relative, data = ScotsBay_HighTideBiomass)
SurveyTideDifference.one.way <- aov(DFO_Estimate ~ Tide_Relative, data = ScotsBay_HighTideBiomass)

boxplot(DFO_Estimate ~ High_Tide, data = ScotsBay_HighTideBiomass)

OneWayHighTide <- summary(HighTide.one.way)
TwoWayHighTide <- summary(SurveyHighTide.two.way)

OneWayTideDifference <- summary(SurveyTideDifference.one.way)
TwoWayTideDifference <- summary(SurveyTideDifference.two.way)

###Extra code that might be useful sometime

#ScotsBay_HighTideBiomassSubset <- subset(ScotsBay_HighTideBiomass, subset = Year %in% c(2012, 2013, 2014, 2015, 2016))
#mean = mean(ScotsBay_HighTideBiomass$DFO_Estimate)
#boxplot(DFO_Estimate ~ Survey_Start*Tide_Difference, data = ScotsBay_HighTideBiomass)

###Julian Date and DFO_Estimates (German Bank)

JulianAndBiomass <- subset(Survey_Factors, select=c("Survey_Date", "Survey_Area", "Julian", "DFO_Estimate"))
JulianAndBiomass <- na.omit(JulianAndBiomass)
JulianAndBiomass<- subset(JulianAndBiomass, Survey_Date < '2023-05-22') #As only one DFO factor for 2023 is in the system, removed to keep years nice.
GermanBank_Turnover <- subset(JulianAndBiomass, Survey_Area=='GB' )

GBOneWayJB <- aov(DFO_Estimate ~ Julian, data = GermanBank_Turnover)
summary(GBOneWayJB)

GB_Point_Graph <- ggplot(GermanBank_Turnover, aes(Julian, DFO_Estimate)) +geom_smooth() + geom_point() 
print(GB_Point_Graph + labs(y = "Survey Biomass (mt)", x = "Julian Day"))

###Number of Vessels and Survey biomass (German Bank)
GB_VesselsBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Julian", "DFO_Estimate", "Survey_Area"))
GB_VesselsBiomass <- na.omit(GB_VesselsBiomass)
GB_VesselsBiomass <- subset(GB_VesselsBiomass, Survey_Area == "GB", Survey_Date < '2023-05-22')
GB_VesselsBiomass$No_of_Vessels <- as.factor(GB_VesselsBiomass$No_of_Vessels)

GB_GraphVessels <- ggplot(GB_VesselsBiomass, aes(No_of_Vessels, DFO_Estimate))+ geom_point() + geom_smooth()
print(GB_GraphVessels)

GB_BoxplotVessels <-boxplot(GB_VesselsBiomass$DFO_Estimate~GB_VesselsBiomass$No_of_Vessels, main = "Biomass recorded per number of Vessels in Survey", xlab="Number of Vessels in Survey", ylab="Survey Biomass")
GB_OneWayVessels <- aov(DFO_Estimate ~ No_of_Vessels, data = GB_VesselsBiomass)
GBSurveyTideDifference.two.way <- aov(DFO_Estimate ~ No_of_Vessels*Julian, data = GermanBank_HighTideBiomass)

###German Bank Survey Area, Julian, Survey Time, High Tide Time, DFO_Estimate
GBSurveyAreaTimeTideBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Survey_Area","Year", "Julian", "DFO_Estimate", "Survey_Start", "High_Tide", "Tide_Difference", "Tide_Relative" ))
GBSurveyAreaTimeTideBiomass <- na.omit(GBSurveyAreaTimeTideBiomass)
GermanBank_HighTideBiomass <- subset(GBSurveyAreaTimeTideBiomass, Survey_Area=="GB")
GermanBank_HighTideBiomass <- subset(GermanBank_HighTideBiomass, Survey_Date < '2023-05-22')
GermanBank_HighTideBiomass$Tide_Relative <- as.numeric(GermanBank_HighTideBiomass$Tide_Relative)


###German Bank Tide Relative Point Graph
GB_Tide_Relative_Point <- ggplot(GermanBank_HighTideBiomass, aes(x=Tide_Relative, y=DFO_Estimate)) + geom_point(aes(group= Tide_Relative)) +geom_smooth() + geom_hline(yintercept=mean(GermanBank_HighTideBiomass$DFO_Estimate))
print(GB_Tide_Relative_Point + labs(y="Survey Biomass", x = "Tide Relative to Survey Start (hrs)"))

###ANOVAS

#GBSurveyTideDifference.two.way <- aov(DFO_Estimate ~ Survey_Start*Tide_Relative, data = GermanBanky_HighTideBiomass)
GBSurveyTideDifference.one.way <- aov(DFO_Estimate ~ Tide_Relative, data = GermanBank_HighTideBiomass)

boxplot(DFO_Estimate ~ High_Tide, data = GermanBank_HighTideBiomass)

GBOneWayHighTide <- summary(HighTide.one.way)
GBTwoWayHighTide <- summary(SurveyHighTide.two.way)

GBOneWayTideDifference <- summary(SurveyTideDifference.one.way)
GBTwoWayTideDifference <- summary(SurveyTideDifference.two.way)

###Extra code that might be useful sometime

#ScotsBay_HighTideBiomassSubset <- subset(ScotsBay_HighTideBiomass, subset = Year %in% c(2012, 2013, 2014, 2015, 2016))
#mean = mean(ScotsBay_HighTideBiomass$DFO_Estimate)
#boxplot(DFO_Estimate ~ Survey_Start*Tide_Difference, data = ScotsBay_HighTideBiomass)