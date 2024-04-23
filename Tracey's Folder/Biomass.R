rm(list = ls())
options(scipen = 999)

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
library(zoo)
library(ggrepel)
library(tidyr)

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder")
Biomass <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Tracey's Folder/SSB Estimates.csv")

Biomass$Julian <- format(Biomass$Survey_Date, "%j")
Biomass$Day <- format(Biomass$Survey_Date, "%d")
Biomass$Month <- format(Biomass$Survey_Date, "%m")
Biomass$Year <- as.character(Biomass$Year)

Biomass <- subset(Biomass, Month == "05" | Month == "06")
Biomass <- subset(Biomass, Year == "2017" | 
                    Year == "2018" | 
                    Year == "2019" | 
                    Year == "2020" | 
                    Year == "2021" | 
                    Year == "2022" | 
                    Year == "2023")




ggplot(Biomass, aes(Julian, DFO_Estimate)) +
  geom_point(aes(colour = Year, size = 1)) + 
  scale_y_continuous(breaks = seq(0, 130000, by = 10000)) + 
  scale_fill_discrete(breaks=c("2017", "2018", "2019", "2020", "2021", "2022", "2023")) +
  labs(y = "Biomass")
  