rm(list = ls())

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

#Load Data
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Fat Data")
Connors = read_csv("Connors 2004-2017.csv")
Com1 = read_csv("Comeaus 2005-2010.csv")
Com2 = read_csv("Comeaus 2017.csv")
Com3 = read_csv("Comeaus 2018.csv")
Com4 = read_csv("Comeaus 2020.csv")
Com5 = read_csv("Comeaus 2021.csv")
Com6 = read_csv("Comeaus 2022.csv")
Scotia = read_csv("Scotia Update.csv") #manually adjusted date_time due to missing leading 0's
Unknown = read_csv("Unknown 2007-2013.csv")

#Connors QC / structuring
Connors = Connors %>% dplyr::select(Date = DATE, Vessel = BOAT, Ground = AREA, 
                            Size4.5 = Size_4.5, Size4.5_5, Size5_6, Size6_7, Size7_8, 
                            Size8_9, Size9_10, Size10_11, Size11_12, Size12 = Size_12, Month) %>%
  mutate(Year = substr(Date,1,4))
Connors = pivot_longer(data=Connors, cols=starts_with("Size"), names_to="Size", names_prefix = "Size", values_to="Fat") %>%
  filter(Fat < 40)

#ggplot(data=Connors, aes(x=Date, y=Fat, colour=Year)) +
#  geom_boxplot() +
#  labs(y="Fat Percent (%)")

#Comeau's QC / structuring
Com2 = Com2 %>% dplyr::select(Plant, Date, Method, Ground = Harvest_Site, Vessel = Boat, Bait = bait, Small = small, Med = med, Large = large)
Com2 = pivot_longer(data=Com2, cols=c("Bait", "Small", "Med", "Large"), names_to="Size", values_to="Fat") %>% filter(Fat > 0)
Com3 = Com3 %>% dplyr::select(Plant, Date, Method, Ground = Harvest_Site, Vessel = Boat, Bait = bait, Small = small, Med = med, Large = large)
Com3 = pivot_longer(data=Com3, cols=c("Bait", "Small", "Med", "Large"), names_to="Size", values_to="Fat") %>% filter(Fat > 0)
Com4 = Com4 %>% dplyr::select(Plant, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com5 = Com5 %>% dplyr::select(Plant, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com6 = Com6 %>% dplyr::select(Plant, CollectionMethod, SampleID, Date, Method, Vessel, Lat, Lon, Ground, Fat, "FishLength(cm)", "FishWeight(g)")
Com6$Lon = as.character(Com6$Lon)
Com7 = full_join(Com6, Com5)
Com8 = full_join(Com7, Com4)
Com9 = full_join(Com8, Com3)
Com10 = full_join(Com9, Com2)
Comeau = full_join(Com10, Com1)
rm(list= c("Com1", "Com2", "Com3", "Com4", "Com5", "Com6", "Com7", "Com8", "Com9", "Com10"))
