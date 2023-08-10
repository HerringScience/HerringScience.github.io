# remove everything in the workspace
rm(list = ls())

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

#load survey and biomass data
setwd(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/"))
Survey %>% read_csv("Survey Data.csv")
SSB %>% read_csv("SSB Estimates.csv")

#add tide difference column
Survey = Survey %>%
  mutate(TideDiff = )

