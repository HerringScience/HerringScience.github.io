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

#remove everything in environment
rm(list = ls())

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")

Survey_Factors <- read_csv("surveyFactorsAll_Tracey with SSB data.csv")

#Just Julian Date and HSC_Turnover

JulianAndBiomass <- subset(Survey_Factors, select=c("Survey_Area", "Julian", "DFO_Estimates"))
    JulianAndBiomass <- na.omit(JulianAndBiomass)
ScotsBay_Turnover <- subset(JulianAndBiomass, Survey_Area=='SB' )


#Point Graph - Julian and Biomass
Point_Graph <- ggplot(ScotsBay_Turnover, aes(Julian, DFO_Estimates)) +geom_smooth() + geom_point() 
print(Point_Graph)

#Number of Vessels and Survey biomass
VesselsBiomass <- subset(Survey_Factors, select=c("No_of_Vessels", "Julian", "DFO_Estimates", "Survey_Area"))
    VesselsBiomass <- na.omit(VesselsBiomass)
    VesselsBiomass <- subset(VesselsBiomass, Survey_Area == "SB")
    VesselsBiomass$No_of_Vessels <- as.factor(VesselsBiomass$No_of_Vessels)

GraphVessels <- ggplot(VesselsBiomass, aes(No_of_Vessels, DFO_Estimates))+ geom_point() + geom_smooth()
print(GraphVessels)

BoxplotVessels <-boxplot(VesselsBiomass$DFO_Estimates~VesselsBiomass$No_of_Vessels, main = "Biomass recorded per number of Vessels in Survey", xlab="Number of Vessels in Survey", ylab="DFO Estimates")

#Survey Area, Julian, Survey Time, High Tide Time, DFO_Estimates
SurveyAreaTimeTideBiomass <- subset(Survey_Factors, select=c("Survey_Area","Year", "Julian", "DFO_Estimates", "Survey_Start", "High_Tide", "Tide_Difference", "Tide_Relative" ))
SurveyAreaTimeTideBiomass <- na.omit(SurveyAreaTimeTideBiomass)
ScotsBay_HighTideBiomass <- subset(SurveyAreaTimeTideBiomass, Survey_Area=="SB")


#ScotsBay_HighTideBiomassSubset <- subset(ScotsBay_HighTideBiomass, subset = Year %in% c(2012, 2013, 2014, 2015, 2016))
#mean = mean(ScotsBay_HighTideBiomass$DFO_Estimates)

#Scot Bay Tide Relative Point Graph
Tide_Relative_Point <- ggplot(ScotsBay_HighTideBiomass, aes(x=Tide_Relative, y=DFO_Estimates)) + geom_point(aes(group= Tide_Difference)) +geom_smooth() + geom_hline(yintercept=mean(ScotsBay_HighTideBiomass$DFO_Estimates))
print(Tide_Relative_Point)

#ANOVAS
two.way <- aov(DFO_Estimates ~ Survey_Start*High_Tide, data = ScotsBay_HighTideBiomass)
one.way <- aov(DFO_Estimates ~ Tide_Difference, data = ScotsBay_HighTideBiomass)
summary(one.way)
summary(two.way)


VBANOVA <- aov(DFO_Estimates ~ No_of_Vessels , data = VesselsBiomass)
summary(VBANOVA)

#One Way ANOVA - Julian and Turnover
JTANOVA <- aov(DFO_Estimates ~ Julian, data = ScotsBay_Turnover)
summary(JTANOVA)
