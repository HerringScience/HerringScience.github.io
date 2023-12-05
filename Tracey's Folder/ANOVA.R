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
#library(weathercan)
library(GGally)
library(psych)
library(sp)
library(raster)
library(PBSmapping)
#library(rgeos) - replaced by terra and sf
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
library(terra)
library(multcompView)
library(sf)
library(ggrepel)

###remove everything in environment
rm(list = ls())
options(scipen = 999)

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")

Survey_Factors <- read_csv("surveyFactorsAll_Tracey with SSB data.csv")

Survey_Data <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv")

#
##
### SCOTS BAY
##
#

### SCOTS BAY Julian Date and Survey Biomass

JulianAndBiomass <- subset(Survey_Factors, select=c("Survey_Date", "Ground", "Julian", "DFO_Estimate")) +
  JulianAndBiomass <- na.omit(JulianAndBiomass)
  JulianAndBiomass<- subset(JulianAndBiomass, Survey_Date < '2023-05-22') #As only one DFO factor for 2023 is in the system, removed to keep years nice.

ScotsBay_Turnover <- subset(JulianAndBiomass, Ground=='SB' )

SBPointGraph <- ggplot(ScotsBay_Turnover, aes(Julian, DFO_Estimate)) +geom_point() +geom_smooth(span = 1)
  print(SBPointGraph + labs(y = "Survey Biomass (mt)", x = "Julian Day"))

summary(aov(DFO_Estimate ~ Julian, data = ScotsBay_Turnover))

### SCOTS BAY Number of Vessels and Survey Biomass

SBVesselsBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Julian", "DFO_Estimate", "Ground"))
  SBVesselsBiomass <- na.omit(SBVesselsBiomass)
  SBVesselsBiomass <- subset(SBVesselsBiomass, Ground == "SB", Survey_Date < '2023-05-22')
  SBVesselsBiomass$No_of_Vessels <- as.factor(SBVesselsBiomass$No_of_Vessels)

SBBoxplotVessels <-boxplot(SBVesselsBiomass$DFO_Estimate~SBVesselsBiomass$No_of_Vessels, xlab="Number of Vessels in Survey", ylab="Survey Biomass (mt)")

SBOneWayVessels <- aov(DFO_Estimate ~ No_of_Vessels, data = SBVesselsBiomass)
  summary(SBOneWayVessels)
                           
###Scots Bay Survey Area, Julian, Survey Time, High Tide Time, Survey Biomass
                           
SurveyAreaTimeTideBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Ground","Year", "Julian", "DFO_Estimate", "Survey_Start", "High_Tide", "Tide_Difference", "Tide_Relative" ))
  SurveyAreaTimeTideBiomass <- na.omit(SurveyAreaTimeTideBiomass)
                           
ScotsBayHighTideBiomass <- subset(SurveyAreaTimeTideBiomass, Ground=="SB")
  ScotsBayHighTideBiomass <- subset(ScotsBayHighTideBiomass, Survey_Date < '2023-05-22')
  ScotsBayHighTideBiomass$Tide_Relative <- as.numeric(ScotsBayHighTideBiomass$Tide_Relative)
                           
SBTideRelativePoint <- ggplot(ScotsBayHighTideBiomass, aes(x=Tide_Relative, y=DFO_Estimate)) + geom_point(aes(group= Tide_Relative)) +stat_smooth(span = 1) + geom_hline(yintercept=mean(ScotsBayHighTideBiomass$DFO_Estimate))
  print(SBTideRelativePoint + labs(y="Survey Biomass", x = "Tide Relative to Survey Start in Hours"))
                           
SBOneWayTideRelative <- aov(DFO_Estimate ~ Tide_Relative, data = ScotsBayHighTideBiomass)
  summary(SBOneWayTideRelative)
  
  
###Scots Bay Sunset Relative
SunsetData <- subset(Survey_Factors, select = c("Survey_Date", "Year", "Ground", "DFO_Estimate", "Sunset_Relative"))
  SunsetData <- na.omit(SunsetData)
  SunsetData <- subset(SunsetData,  Survey_Date < '2023-05-22') 
  
ScotsBaySunsetData <- subset(SunsetData, Ground == "SB")
  ScotsBaySunsetData$Sunset_Relative <- as.numeric(ScotsBaySunsetData$Sunset_Relative)
  
SBSunsetRelativePoint <- ggplot(ScotsBaySunsetData, aes (x=Sunset_Relative, y=DFO_Estimate))+ geom_point(aes(group=Sunset_Relative)) +geom_smooth(span = 1)
  print(SBSunsetRelativePoint + labs(y="Survey Biomass", x="Sunset Relative to Survey Start (hrs)"))
  
SBSunsetOneWayANOVA <- aov(DFO_Estimate ~ Sunset_Relative, data = ScotsBaySunsetData)
  summary(SBSunsetOneWayANOVA)
  
###Scots Bay Peak Biomass
PeakBiomass <- subset(Survey_Factors, select = c("Survey_Date", "Year", "Julian", "Ground", "DFO_Estimate"))
  PeakBiomass <- na.omit(PeakBiomass)
  PeakBiomass <- subset(PeakBiomass, Survey_Date < '2023-05-22')  
  
ScotsBayPeakBiomass <- subset(PeakBiomass, Ground == "SB")  
  
ScotsBayPeakBiomass <- ScotsBayPeakBiomass %>% group_by(Year) %>% slice_max(DFO_Estimate)
ScotsBayPeakBiomass <-ScotsBayPeakBiomass %>% add_column(format(ScotsBayPeakBiomass$Survey_Date, "%B"))
colnames(ScotsBayPeakBiomass)[6] = "Month"
ScotsBayPeakBiomass %>% group_by(Month)

ScotsBayPeakBiomass$Month <-factor(ScotsBayPeakBiomass$Month, levels = c("June", "July", "August", "September"))
ScotsBayPeakBiomassPointGraph <- ggplot(ScotsBayPeakBiomass, aes(x=Julian, y=DFO_Estimate)) + 
                                 geom_point(aes(color=Month, size = 3)) + 
                                 geom_label_repel(aes(label=Year), nudge_x = 2)
                                 


ScotsBayPeakBiomassPointGraph + scale_fill_discrete(breaks=c("June", "July", "August", "September"))
    print(ScotsBayPeakBiomassPointGraph + labs(y="Peak Survey Biomass(mt)", x = "Julian"))
  
SBPeakBiomassANOVA = aov(DFO_Estimate~Year, data = ScotsBayPeakBiomass)
  summary(SBPeakBiomassANOVA)
  
### Scots Bay Index trend
  
TotalBiomass <- subset(Survey_Factors, select = c("Year","Ground", "DFO_Estimate", "Survey_Date"))
  TotalBiomass <- na.omit(TotalBiomass)
  TotalBiomass <- subset(TotalBiomass, Survey_Date < '2023-05-22')
  
ScotsBayTotalBiomass <- subset(TotalBiomass, Ground == "SB")
  ScotsBayTotalBiomass <- ScotsBayTotalBiomass %>% group_by(Year) %>% add_count(Year)
  aggregateSB <- aggregate(ScotsBayTotalBiomass$DFO_Estimate, list(ScotsBayTotalBiomass$Year), FUN=(sum))
  colnames(aggregateSB)<-c("Year", "DFO_Estimate")
  
SBTotalBiomass <- print(ggplot(aggregateSB, aes(x=Year, y=DFO_Estimate, col = "Yearly Total Biomass")) + 
                            geom_point(aes(size = 2)) +
                            geom_line()+
                            labs(x = "Year", y= "Total Biomass (mt)"))                    
  
PeakBiomass <- subset(Survey_Factors, select = c("Survey_Date", "Year","Ground", "DFO_Estimate"))
  PeakBiomass <- na.omit(PeakBiomass)
  PeakBiomass <- subset(PeakBiomass, Survey_Date < '2023-05-22')  
  
ScotsBayPeakBiomass <- subset(PeakBiomass, Ground == "SB")  
  ScotsBayPeakBiomass <- ScotsBayPeakBiomass %>% group_by(Year) %>% slice_max(DFO_Estimate)
  
ScotsBayPeakBiomassPointGraph <- (ggplot(ScotsBayPeakBiomass, aes(x=Year, y=DFO_Estimate, col = 'blue')) + 
                                        geom_point(colour = 'blue', size = 3) +
                                        geom_line()+
                                        legend(2000, 500000, legend= c("Scots Bay Peak Biomass")))
  
CombinedPlot2 <- merge(aggregateSB, ScotsBayPeakBiomass, by="Year" )                      
  
CombinedPlot3 <- print(ggplot(CombinedPlot2, aes(x=Year)) + 
                           geom_line(aes(y=DFO_Estimate.x, color = "blue")) + 
                           geom_line(aes(y=DFO_Estimate.y, color = "red")) +
                           labs(x = "Year", y = "Total Biomass") +
                           scale_colour_manual(values = c("blue", "red"), labels = c("Scots Bay Total Yearly Biomass", "Scots Bay Peak Biomass")))
#
##
### GERMAN BANK
##
#
  
###German Bank Julian Date and Survey Biomass
  
JulianAndBiomass <- subset(Survey_Factors, select=c("Survey_Date", "Ground", "Julian", "DFO_Estimate"))
  JulianAndBiomass <- na.omit(JulianAndBiomass)
  JulianAndBiomass<- subset(JulianAndBiomass, Survey_Date < '2023-05-22') #As only one DFO factor for 2023 is in the system, removed to keep years nice.
  
GermanBankTurnover <- subset(JulianAndBiomass, Ground=='GB' )
  
GBPointGraph <- ggplot(GermanBankTurnover, aes(Julian, DFO_Estimate)) +geom_smooth(span = 1) + geom_point() 
  print(GBPointGraph + labs(y = "Survey Biomass (mt)", x = "Julian Day"))

GBOneWayJB <- aov(DFO_Estimate ~ Julian, data = GermanBankTurnover)
  summary(GBOneWayJB)
  

### German Bank Number of Vessels and Survey Biomass
VesselsBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Julian", "DFO_Estimate", "Ground"))
  VesselsBiomass <- na.omit(VesselsBiomass)
  
GBVesselsBiomass <- subset(VesselsBiomass, Ground == "GB", Survey_Date < '2023-05-22')
  GBVesselsBiomass$No_of_Vessels <- as.factor(GBVesselsBiomass$No_of_Vessels)
  
GBBoxplotVessels <-boxplot(GBVesselsBiomass$DFO_Estimate~GBVesselsBiomass$No_of_Vessels, xlab="Number of Vessels in Survey", ylab="Survey Biomass (mt)")
  
GBOneWayVessels <- aov(DFO_Estimate ~ No_of_Vessels, data = GBVesselsBiomass)
  summary(GBOneWayVessels)

###German Bank Survey Area, Julian, Survey Time, High Tide Time, DFO_Estimate
SurveyAreaTimeTideBiomass <- subset(Survey_Factors, select=c("Survey_Date", "No_of_Vessels", "Ground","Year", "Julian", "DFO_Estimate", "Survey_Start", "High_Tide", "Tide_Difference", "Tide_Relative" ))
  SurveyAreaTimeTideBiomass <- na.omit(SurveyAreaTimeTideBiomass)
  
GermanBankHighTideBiomass <- subset(SurveyAreaTimeTideBiomass, Ground=="GB")
  GermanBankHighTideBiomass <- subset(GermanBankHighTideBiomass, Survey_Date < '2023-05-22')
  GermanBankHighTideBiomass <- subset(GermanBankHighTideBiomass, DFO_Estimate!="228870" & DFO_Estimate!="191802" & DFO_Estimate!="143937") #removed top 3 values to see if this made a difference to the ANOVA
  GermanBankHighTideBiomass$Tide_Relative <- as.numeric(GermanBankHighTideBiomass$Tide_Relative)
  
GBTideRelativePoint <- ggplot(GermanBankHighTideBiomass, aes(x=Tide_Relative, y=DFO_Estimate)) + geom_point(aes(group= Tide_Relative)) +geom_smooth(span=5) + geom_hline(yintercept=mean(GermanBankHighTideBiomass$DFO_Estimate))
  print(GBTideRelativePoint + labs(y="Survey Biomass", x = "Tide Relative to Survey Start (hrs)"))
  
GBSurveyTideDifference.one.way <- aov(DFO_Estimate ~ Tide_Relative, data = GermanBankHighTideBiomass)
  summary(GBSurveyTideDifference.one.way)
  
###Sunset Relative
SunsetData <- subset(Survey_Factors, select = c("Survey_Date", "Year", "Ground", "DFO_Estimate", "Sunset_Relative"))
  SunsetData <- na.omit(SunsetData)
  SunsetData <- subset(SunsetData,  Survey_Date < '2023-05-22') 

GermanBankSunsetData <- subset(SunsetData, Ground == "GB")
  GermanBankSunsetData$Sunset_Relative <- as.numeric(GermanBankSunsetData$Sunset_Relative)

GBSunsetRelativePoint <- ggplot(GermanBankSunsetData, aes (x=Sunset_Relative, y=DFO_Estimate))+ geom_point(aes(group=Sunset_Relative)) +geom_smooth(span=1)
  print(GBSunsetRelativePoint + labs(y="Survey Biomass", x="Sunset Relative to Survey Start (hrs)"))

GBSunsetOneWayANOVA <- aov(DFO_Estimate ~ Sunset_Relative, data = GermanBankSunsetData)
  summary(GBSunsetOneWayANOVA)
  
###German Bank Peak Biomass
PeakBiomass <- subset(Survey_Factors, select = c("Survey_Date", "Year", "Julian", "Ground", "DFO_Estimate"))
  PeakBiomass <- na.omit(PeakBiomass)
  PeakBiomass <- subset(PeakBiomass, Survey_Date < '2023-05-22')  
  
GermanBankPeakBiomass <- subset(PeakBiomass, Ground == "GB")  
  GermanBankPeakBiomass <- GermanBankPeakBiomass %>% group_by(Year) %>% slice_max(DFO_Estimate)
  GermanBankPeakBiomass <-GermanBankPeakBiomass %>% add_column(format(GermanBankPeakBiomass$Survey_Date, "%B"))
  colnames(GermanBankPeakBiomass)[6] = "Month"
  GermanBankPeakBiomass %>% group_by(Month)
  GermanBankPeakBiomass$Month <-factor(GermanBankPeakBiomass$Month, levels = c("August", "September", "October"))
  
GermanBankPeakBiomassPointGraph <- ggplot(GermanBankPeakBiomass, aes(x=Julian, y=DFO_Estimate)) + 
    geom_point(aes(color=Month, size = 3)) + 
    geom_label_repel(aes(label=Year), nudge_x = 2)
  
  GermanBankPeakBiomassPointGraph + scale_fill_discrete(breaks=c("August", "September", "October"))
    print(GermanBankPeakBiomassPointGraph + labs(y="Peak Survey Biomass(mt)", x = "Julian"))
  
  GBPeakBiomassANOVA = aov (DFO_Estimate~Year, data=GermanBankPeakBiomass)
  summary(GBPeakBiomassANOVA)
  
### German Bank Index trend
  
TotalBiomass <- subset(Survey_Factors, select = c("Year","Ground", "DFO_Estimate", "Survey_Date"))
  TotalBiomass <- na.omit(TotalBiomass)
  TotalBiomass <- subset(TotalBiomass, Survey_Date < '2023-05-22')
  
GermanBankTotalBiomass <- subset(TotalBiomass, Ground == "GB")
GermanBankTotalBiomass <- GermanBankTotalBiomass %>% group_by(Year) %>% add_count(Year)
aggregateGB <- aggregate(GermanBankTotalBiomass$DFO_Estimate, list(GermanBankTotalBiomass$Year), FUN=(sum))
colnames(aggregateGB)<-c("Year", "DFO_Estimate")

GBTotalBiomass <- print(ggplot(aggregateGB, aes(x=Year, y=DFO_Estimate, col = "Yearly Total Biomass")) + 
                    geom_point(aes(size = 2)) +
                    geom_line()+
                    labs(x = "Year", y= "Total Biomass (mt)"))                    
  
PeakBiomass <- subset(Survey_Factors, select = c("Survey_Date", "Year","Ground", "DFO_Estimate"))
  PeakBiomass <- na.omit(PeakBiomass)
  PeakBiomass <- subset(PeakBiomass, Survey_Date < '2023-05-22')  

GermanBankPeakBiomass <- subset(PeakBiomass, Ground == "GB")  
  GermanBankPeakBiomass <- GermanBankPeakBiomass %>% group_by(Year) %>% slice_max(DFO_Estimate)

GermanBankPeakBiomassPointGraph <- (ggplot(GermanBankPeakBiomass, aes(x=Year, y=DFO_Estimate, col = 'blue')) + 
  geom_point(colour = 'blue', size = 3) +
  geom_line()+
  legend(2000, 500000, legend= c("German Bank Peak Biomass")))

CombinedPlot2 <- merge(aggregateGB, GermanBankPeakBiomass, by="Year" )                      

CombinedPlot3 <- print(ggplot(CombinedPlot2, aes(x=Year)) + 
  geom_line(aes(y=DFO_Estimate.x, color = "blue")) + 
  geom_line(aes(y=DFO_Estimate.y, color = "red")) +
  labs(x = "Year", y = "Total Biomass") +
  scale_colour_manual(values = c("blue", "red"), labels = c("German Bank Total Yearly Biomass", "German Bank Peak Biomass")))

