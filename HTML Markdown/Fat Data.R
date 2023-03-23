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
Main = read_csv("Master.csv")
Con = read_csv("Connors 2004-2017.csv")
Com1 = read_csv("Comeaus 2005-2010.csv")
Com2 = read_csv("Comeaus 2017.csv")
Com3 = read_csv("Comeaus 2018.csv")
Com4 = read_csv("Comeaus 2020.csv")
Com5 = read_csv("Comeaus 2021.csv")
Com6 = read_csv("Comeaus 2022.csv")
Scotia = read_csv("Scotia 2017-2018.csv")
Unkn = read_csv("Unknown 2007-2013.csv")

#Connors QC / structuring
Con = Con %>% dplyr::select(Date = DATE, Vessel = BOAT, Ground = AREA, 
                            Size4.5 = Size_4.5, Size4.5_5, Size5_6, Size6_7, Size7_8, 
                            Size8_9, Size9_10, Size10_11, Size11_12, Size12 = Size_12, Month) %>%
  mutate(Year = substr(Date,1,4))
Con = pivot_longer(data=Con, cols=starts_with("Size"), names_to="Size", names_prefix = "Size", values_to="Fat") %>%
  filter(Fat < 40)
ggplot(data=Con, aes(x=Date, y=Fat, colour=Year)) +
  geom_boxplot() +
  labs(y="Fat Percent (%)")

#Scotia QC / structuring
Scotia = Scotia %>% mutate(Average = rowMeans(dplyr::select(., "#1":"#8"))) %>%
  dplyr::select(Ground = Location, Vessel, Type, Filets = "No. Filets per kg", 
                                  Size = "Size (cm)", Date_Time = Recorded, Average)
Scotia %>% date(Date_Time)
  
