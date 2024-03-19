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

LarvalSum = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/LarvalSum.csv"))

larv = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Measurements.csv"))
arc = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/ARC Data.csv"))
arc = arc %>% dplyr::select(id, Larvae_Count, Notes)
survey = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
survey = survey %>% mutate(Ground = substr(id,1,2))

larv = left_join(larv, arc, by="id")
larv = left_join(larv, survey)
larv = larv %>% dplyr::select(Ground, id, Date, Survey.No, No_jars, Lengthmm, Condition, Yolk_sac, Preservative, TowReplicate, TowID, ARC_Count=Larvae_Count, ARC_Notes=Notes, Lon1, Lat1, Lon2, Lat2, TowTime, AvgTowDepth, MaxTowDepth, CTDAvgTemp=AvgTemp, Volume, Month, Year, Day)
larv$Date = dmy(larv$Date)
larv$Survey.No = as.factor(larv$Survey.No)
larv$Year = as.factor(larv$Year)
larv$category =  with(larv, ifelse(larv$Lengthmm < 8 , 1, 
                                   ifelse(larv$Lengthmm < 12 & larv$Lengthmm >= 8, 2, 
                                          ifelse(larv$Lengthmm >= 12  & larv$Lengthmm < 17, 3, 
                                                 ifelse(larv$Lengthmm > 17 & larv$Lengthmm < 27, 4, 5)))))
larv$category = as.factor(larv$category)
larv$hatchDate = larv$Date - 10 #incubation duration of 10 days based on NOAA and DFO data.
larv$hatchDate = ymd(larv$hatchDate)

#Calculating spawn dates - This is from Darren. Removing this based on the calculation using the preservative.
# #larv=larv %>% mutate(MAXspawnDate = ifelse(category == 1, hatchDate-14,
#                                            ifelse(category == 2, hatchDate-35,
#                                                   ifelse(category == 3, hatchDate-56,
#                                                          ifelse(category == 4, hatchDate-98,
#                                                                 ifelse(category == 5, hatchDate-99, "NA"))))))
# larv$MAXspawnDate=as.numeric(larv$MAXspawnDate)
# larv$MAXspawnDate=as.Date(larv$MAXspawnDate, origin = "1970-01-01")
# 
# larv=larv %>% mutate(MINspawnDate = ifelse(category == 1, hatchDate,
#                                            ifelse(category == 2, hatchDate-14,
#                                                   ifelse(category == 3, hatchDate-35,
#                                                          ifelse(category == 4, hatchDate-56,
#                                                                 ifelse(category == 5, hatchDate-98, "NA"))))))
# larv$MINspawnDate=as.numeric(larv$MINspawnDate)
# larv$MINspawnDate=as.Date(larv$MINspawnDate, origin = "1970-01-01")

# if preservative is formalin, apply L  = 0.984 + 0.993 x X1. (X1 = fixed/preserved length therefore Larval$Lengthmm, L = Live length.) 
# if preservation is alcohol apply L = 0.532 + 0.989 x X1 
#This is taken from Fox 1996 alcohol vs Formalin paper. They did 5% and 5 minute net capture simulation. They did suggest that this adjustment would be less accurate the longer the tow period.
# These equations are when the maximum shrinkage has occurred.


larv$LengthAdjustment = with(larv, ifelse(larv$Preservative == "4% formalin", (0.984 + 0.993* larv$Lengthmm),
                                          ifelse(larv$Preservative == "70% alcohol", (0.532 + 0.989*larv$Lengthmm),NA)))

MeanLengthAdjustment <- aggregate(LengthAdjustment~id, larv, mean)
colnames(MeanLengthAdjustment)[2]<- "MeanLengthAdjustment"

#'Exact' spawn date. Growth rate of .24mm/day based on Chenoweth 1989 paper. 
# Paper says applies estimate growth rates to calculate the number of days back to 5mm. Took 5mm off total length to account for this.
# Assumes hatching length is 5mm, day of hatching = day 0
#Adjusted Spawn Date to account for incubation period. Using overall 10 days, as per NOAA info that says 7-10 days, and DFO stock assessment 2020 says 10-12 days.


larv <- merge(larv, MeanLengthAdjustment)
larv$AdjustedAgeInDays <- (larv$LengthAdjustment - 5)/0.24
larv$AdjustedSpawnDate <- as.Date(larv$Date) - larv$AdjustedAgeInDays 


larv$AdjustedJulianSpawnDate <- yday(larv$AdjustedSpawnDate)
larv$AdjustedJulianSpawnDate <- format(larv$AdjustedSpawnDate, "%j")



AdjustedMeanAgeInDays <- aggregate(AdjustedAgeInDays~id, larv, mean)
colnames(AdjustedMeanAgeInDays)[2]<- "AdjustedMeanAgeInDays"
AdjustedMinDateOfSpawn <- aggregate(AdjustedSpawnDate~id, larv, min)
colnames(AdjustedMinDateOfSpawn)[2] <- "AdjustedMinDateOfSpawn"
AdjustedMaxDateOfSpawn <- aggregate(AdjustedSpawnDate~id, larv, max)
colnames(AdjustedMaxDateOfSpawn)[2] <- "AdjustedMaxDateOfSpawn"

AdjustedDays <- merge(AdjustedMaxDateOfSpawn, AdjustedMinDateOfSpawn, by = 'id')
AdjustedDays <- merge(AdjustedDays, AdjustedMeanAgeInDays, by = 'id')

larv <- merge(larv, AdjustedDays, by = 'id')

#add Julian
larv<-larv %>% mutate(Julian = yday(Date))

#Calculating SE/mean/min/max of larval measurements. Changed 'group by' in both larvsummary to (id) from (Ground, Survey.No, Year)
larv <- larv %>%
  group_by(Ground, Survey.No, Year) %>%
  mutate(SD = sd(Lengthmm), MinLength = min(Lengthmm), MaxLength = max(Lengthmm), MeanLength = mean(Lengthmm), Abundance = length(Lengthmm)) %>%
  ungroup()

larv$SD[is.na(larv$SD)] <- 0
larv$SD <- as.numeric(larv$SD)

larvsummary <- larv %>% group_by(Ground, Survey.No, Year) %>%
  summarize(MinLength = mean(MinLength, na.rm = TRUE), 
            MaxLength = mean(MaxLength, na.rm = TRUE), 
            MeanLength = mean(MeanLength, na.rm = TRUE),
            SD = mean(SD, na.rm = TRUE),
            Abundance = length(Lengthmm)) %>%
  mutate(SE = SD/sqrt(Abundance))
surveysummary = survey %>% dplyr::select(id, Ground, Survey.No, Year) %>% group_by(id, Ground, Survey.No, Year) %>% summarize(Survey.No = Survey.No, Year = Year)
surveysummary$Year = as.factor(surveysummary$Year)
surveysummary$Survey.No = as.factor(surveysummary$Survey.No)
larvsummary = left_join(surveysummary, larvsummary)

larvsummary %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Summary Table.csv"))

larv = larv %>%
  mutate(Larv_per_jar = Abundance/No_jars) %>%
  mutate(Volume = ifelse(Volume < 0.01, NA, Volume)) %>%
  mutate(Density = Larv_per_jar/Volume)

#Calculating Avgerage SE/mean/min/max of larval measurements. Changed 'group by' in both larvsummary to (id) from (Ground, Survey.No, Year)
larv <- larv %>%
  group_by(Ground, Survey.No, Year) %>%
  mutate(AdjSD = sd(LengthAdjustment), AdjustedMinLength = min(LengthAdjustment), AdjustedMaxLength = max(LengthAdjustment), MeanLengthAdjustment = MeanLengthAdjustment, Abundance = length(LengthAdjustment)) %>%
  ungroup()

larv$AdjSD[is.na(larv$AdjSD)] <- 0
larv$AdjSD <- as.numeric(larv$AdjSD)

larvsummary <- larv %>% group_by(Ground, Survey.No, Year) %>%
  summarize(MeanAdjustedMinLength = mean(AdjustedMinLength, na.rm = TRUE), 
            MeanAdjustedMaxLength = mean(AdjustedMaxLength, na.rm = TRUE), 
            MeanAdjustedSD = mean(AdjSD, na.rm = TRUE),
            Abundance = length(LengthAdjustment)) %>%
  mutate(SE =  MeanAdjustedSD/sqrt(Abundance))
surveysummary = survey %>% dplyr::select(id, Ground, Survey.No, Year) %>% group_by(id, Ground, Survey.No, Year) %>% summarize(Survey.No = Survey.No, Year = Year)
surveysummary$Year = as.factor(surveysummary$Year)
surveysummary$Survey.No = as.factor(surveysummary$Survey.No)
larvsummary = left_join(surveysummary, larvsummary)

larvsummary %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Summary Table.csv"))


#larvsummary %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/LarvalSum.csv"))

larv = larv %>%
  mutate(Larv_per_jar = Abundance/No_jars) %>%
  mutate(Volume = ifelse(Volume < 0.01, NA, Volume)) %>%
  mutate(Density = Larv_per_jar/Volume)


larv = larv %>%
  dplyr::select(Ground, 
                id, 
                Date, 
                Survey.No, 
                No_jars, 
                Abundance, 
                Lengthmm, 
                category, 
                MinLength, 
                MaxLength, 
                MeanLength, 
                SD, 
                Larv_per_jar, 
                Density, 
                hatchDate, 
                # MINspawnDate, 
                # MAXspawnDate, 
                Julian, 
                Day, 
                Month, 
                Year, 
                Condition, 
                Yolk_sac, 
                Preservative, 
                ARC_Count, 
                ARC_Notes, 
                Lon1, 
                Lat1, 
                Lon2, 
                Lat2, 
                TowTime, 
                AvgTowDepth, 
                MaxTowDepth, 
                CTDAvgTemp, 
                Volume,
                TowID,
                TowReplicate,
                LengthAdjustment,
                AdjustedSpawnDate,
                AdjustedJulianSpawnDate,
                MeanLengthAdjustment,
                AdjustedMeanAgeInDays,
                AdjustedMinDateOfSpawn,
                AdjustedMaxDateOfSpawn
                )

LarvalSum <- larv %>% 
  dplyr::select(Ground,
                id, 
                Date, 
                Survey.No, 
                No_jars, 
                Abundance,
                MinLength, 
                MaxLength, 
                MeanLength, 
                SD, 
                Larv_per_jar, 
                Density,
                hatchDate,
                Julian, 
                Day, 
                Month, 
                Year, 
                Preservative, 
                Lon1, 
                Lat1, 
                Lon2, 
                Lat2, 
                TowTime, 
                AvgTowDepth, 
                MaxTowDepth, 
                CTDAvgTemp, 
                Volume,
                TowID,
                TowReplicate,
                MeanLengthAdjustment,
                AdjustedMeanAgeInDays,
                AdjustedMinDateOfSpawn,
                AdjustedMaxDateOfSpawn)

LarvalSum <- unique(LarvalSum)
                

larv %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Full Larval.csv"))
larv %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/Github/HerringScience.github.io/Source Data/Full Larval.csv"))

#LarvalSum %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/Github/HerringScience.github.io/Source Data/LarvalSum.csv"))
