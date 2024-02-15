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
#library(weathercan)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
#library(rgeos)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)

larv = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Measurements.csv"))
arc = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/ARC Data.csv"))
arc = arc %>% dplyr::select(id, Larvae_Count, Notes)
survey = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
survey = survey %>% mutate(Ground = substr(id,1,2))

larv = left_join(larv, arc, by="id")
larv = left_join(larv, survey)
larv = larv %>% dplyr::select(Ground, id, Date, Survey.No, No_jars, Lengthmm, Condition, Yolk_sac, Preservative, ARC_Count=Larvae_Count, ARC_Notes=Notes, Lon1, Lat1, Lon2, Lat2, TowTime, AvgTowDepth, MaxTowDepth, CTDAvgTemp=AvgTemp, Volume, Month, Year, Day)
larv$Date = dmy(larv$Date)
larv$Survey.No = as.factor(larv$Survey.No)
larv$Year = as.factor(larv$Year)
larv$category =  with(larv, ifelse(larv$Lengthmm < 8 , 1, 
                                   ifelse(larv$Lengthmm < 12 & larv$Lengthmm >= 8, 2, 
                                          ifelse(larv$Lengthmm >= 12  & larv$Lengthmm < 17, 3, 
                                                 ifelse(larv$Lengthmm > 17 & larv$Lengthmm < 27, 4, 5)))))
larv$category = as.factor(larv$category)
larv$hatchDate = larv$Date - 10 #incubation duration of 10 days
larv$hatchDate = ymd(larv$hatchDate)

#Calculating spawn dates
larv=larv %>% mutate(MAXspawnDate = ifelse(category == 1, hatchDate-14,
                                           ifelse(category == 2, hatchDate-35,
                                                  ifelse(category == 3, hatchDate-56,
                                                         ifelse(category == 4, hatchDate-98,
                                                                ifelse(category == 5, hatchDate-99, "NA"))))))
larv$MAXspawnDate=as.numeric(larv$MAXspawnDate)
larv$MAXspawnDate=as.Date(larv$MAXspawnDate, origin = "1970-01-01")

larv=larv %>% mutate(MINspawnDate = ifelse(category == 1, hatchDate,
                                           ifelse(category == 2, hatchDate-14,
                                                  ifelse(category == 3, hatchDate-35,
                                                         ifelse(category == 4, hatchDate-56,
                                                                ifelse(category == 5, hatchDate-98, "NA"))))))
larv$MINspawnDate=as.numeric(larv$MINspawnDate)
larv$MINspawnDate=as.Date(larv$MINspawnDate, origin = "1970-01-01")

#add Julian
larv<-larv %>% mutate(Julian = yday(Date))

#Calculating SE/mean/min/max of larval measurements
larv <- larv %>%
  group_by(Survey.No, Year, Date) %>%
  mutate(SD = sd(Lengthmm), MinLength = min(Lengthmm), MaxLength = max(Lengthmm), MeanLength = mean(Lengthmm), Abundance = length(Lengthmm)) %>%
  ungroup()

larvsummary <- larv %>% group_by(Ground, Survey.No, Year) %>%
  summarize(MinLength = mean(MinLength, na.rm = TRUE), 
            MaxLength = mean(MaxLength, na.rm = TRUE), 
            MeanLength = mean(MeanLength, na.rm = TRUE),
            SD = mean(SD, na.rm = TRUE),
            Abundance = length(Lengthmm)) %>%
  mutate(SE = SD/sqrt(Abundance))
surveysummary = survey %>% dplyr::select(Ground, Survey.No, Year) %>% group_by(Ground, Survey.No, Year) %>% summarize(Survey.No = mean(Survey.No), Year = mean(Year))
surveysummary$Year = as.factor(surveysummary$Year)
surveysummary$Survey.No = as.factor(surveysummary$Survey.No)
larvsummary = left_join(surveysummary, larvsummary)
#larvsummary %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Summary Table.csv"))

larv = larv %>%
  mutate(Larv_per_jar = Abundance/No_jars) %>%
  mutate(Volume = ifelse(Volume < 0.01, NA, Volume)) %>%
  mutate(Density = Larv_per_jar/Volume)

larv = larv %>%
  dplyr::select(Ground, id, Date, Survey.No, No_jars, Abundance, Lengthmm, category, MinLength, MaxLength, MeanLength, SD, Abundance, Larv_per_jar, Density, hatchDate, MINspawnDate, MAXspawnDate, Julian, Day, Month, Year, Condition, Yolk_sac, Preservative, ARC_Count, ARC_Notes, Lon1, Lat1, Lon2, Lat2, TowTime, AvgTowDepth, MaxTowDepth, CTDAvgTemp, Volume)
#larv %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Full Larval.csv"))
