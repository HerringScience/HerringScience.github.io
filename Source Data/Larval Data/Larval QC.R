# Remove the environment

rm(list = ls())

#Import all the required libraries. The ones commented out have been discontinued, but left in in case something breaks and we need to find an alternative.
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

LarvalSum = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/LarvalSum.csv"))
larv = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Measurements.csv"))
arc = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/ARC Data.csv"))
arc = arc %>% dplyr::select(id, Larvae_Count, Notes)
survey = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Survey Data.csv"))
survey = survey %>% mutate(Ground = substr(id,1,2))
  survey$Date = dmy(survey$Date)

larv = merge(larv, LarvalSum, by= c("id", "Preservative"))
  larv$Survey.No = as.character(larv$Survey.No)

#larv = left_join(larv, arc, by= "id") #Removed arc as it appears that all data used is already within larval measurements
  
larv = left_join(larv, survey)
larv1 = larv
larv = larv %>% dplyr::select(Ground, id, Date, Survey.No, No_jars, Lengthmm, MeanLengthAdjustment, AdjustedMeanAgeInDays, Number = Abundance, Condition, Yolk_sac, Preservative, TowReplicate, TowID,  Lon1, Lat1, Lon2, Lat2, TowTime, AvgTowDepth, MaxTowDepth, CTDAvgTemp=AvgTemp, Volume, Day, Month, Year) #ARC_Count=Larvae_Count, ARC_Notes=Notes,
    
  #larv$Date = dmy(larv$Date)
    #larv$Month = format(larv$Date, "%m")
    #larv$Day = format(larv$Date, "%d")
larv$Survey.No = as.factor(larv$Survey.No)
larv$Year = as.factor(larv$Year)
larv$Month = as.factor(larv$Month)
larv$category =  with(larv, ifelse(larv$Lengthmm < 8 , 1, 
                                   ifelse(larv$Lengthmm < 12 & larv$Lengthmm >= 8, 2, 
                                          ifelse(larv$Lengthmm >= 12  & larv$Lengthmm < 17, 3, 
                                                 ifelse(larv$Lengthmm > 17 & larv$Lengthmm < 27, 4, 5)))))
larv$category = as.factor(larv$category)

# if preservative is formalin, apply L  = 0.984 + 0.993 x X1. (X1 = fixed/preserved length therefore Larval$Lengthmm, L = Live length.) 
# if preservation is alcohol apply L = 0.532 + 0.989 x X1 
#This is taken from Fox 1996 alcohol vs Formalin paper. They did 5% and 5 minute net capture simulation. They did suggest that this adjustment would be less accurate the longer the tow period.
# These equations are when the maximum shrinkage has occurred.


larv$LengthAdjustment = with(larv, ifelse(larv$Preservative == "4% formalin", (0.984 + 0.993* larv$Lengthmm),
                                          ifelse(larv$Preservative == "70% alcohol", (0.532 + 0.989*larv$Lengthmm),NA)))

#'Exact' spawn date. Growth rate of .24mm/day based on Chenoweth 1989 paper. 
# Paper says applies estimate growth rates to calculate the number of days back to 5mm. Took 5mm off total length to account for this.
# Assumes hatching length is 5mm, day of hatching = day 0
# Adjusted Spawn Date to account for incubation period. Using 10 days for GB and SI, and 14 days for SB, as per paper "Fisheries Oceanography - 2022 - Burbank - Understanding factors influencing Atlantic herring Clupea harengus recruitment .pdf" and "Messieh, 1987. Some characteristics of atlantic herring spawning in the southern gulf of st lawrence.pdf".
# To get hatch date, remove the -10 or -14 that is there due to incubation period.


larv <- larv %>%
  mutate(AdjustedAgeInDays = ((LengthAdjustment - 5)/0.24))
  

# Taking this out as the dates are not working properly at this moment. Need to adjust for Spawn Date.

# larv$AdjustedSpawnDate = (with(larv, ifelse(larv$Ground == "SB", (as.Date(Date) - AdjustedAgeInDays - 14),
#                                              ifelse(larv$Ground == "GB", (as.Date(Date) - AdjustedAgeInDays - 10),
#                                                     ifelse(larv$Ground == "SI", (as.Date(Date) - AdjustedAgeInDays - 10), NA)))))
# 
# larv <- larv %>%  
#   mutate(AdjustedJulianSpawnDate = yday(as.Date(AdjustedSpawnDate))) %>%
#   mutate(Julian = yday(Date))

#Adding averages into the dataframe

# larv <- larv %>%
#   group_by(id) %>%
#   arrange(LengthAdjustment) %>%
#   mutate(MeanLengthAdjustment = mean(LengthAdjustment)) %>%
#   mutate(AdjustedMeanAgeInDays = mean(AdjustedAgeInDays)) %>%
#   mutate(AdjustedMinDateOfSpawn = min(AdjustedSpawnDate)) %>%
#   mutate(AdjustedMaxDateOfSpawn = max(AdjustedSpawnDate)) %>%
#   mutate(MinLength = min(LengthAdjustment)) %>%
#   mutate(MaxLength = max(LengthAdjustment)) %>%
#   mutate(SD = sd(LengthAdjustment)) %>%
#   mutate(Abundance = length(LengthAdjustment)) %>%
#   mutate(Larv_per_jar = Abundance/No_jars) %>%
#   mutate(Volume = ifelse(Volume < 0.01, NA, Volume)) %>%
#   mutate(Density = Larv_per_jar/Volume)
#   
# 

#####Calculating SE/mean/min/max of larval measurements.

##### This is Darren's old code. Do we still need all this?

# larv <- larv %>%
#   group_by(Ground, Survey.No, Year) %>%
#   mutate(SD = sd(Lengthmm), MinLength = min(Lengthmm), MaxLength = max(Lengthmm), MeanLength = mean(Lengthmm), Abundance = length(Lengthmm)) %>%
#   ungroup()
# 
# larv$SD[is.na(larv$SD)] <- 0
# larv$SD <- as.numeric(larv$SD)
# 
# larvsummary <- larv %>% group_by(Ground, Survey.No, Year) %>%
#   summarize(MinLength = mean(MinLength, na.rm = TRUE), 
#             MaxLength = mean(MaxLength, na.rm = TRUE), 
#             MeanLength = mean(MeanLength, na.rm = TRUE),
#             SD = mean(SD, na.rm = TRUE),
#             Abundance = length(Lengthmm)) %>%
#   mutate(SE = SD/sqrt(Abundance))
# surveysummary = survey %>% dplyr::select(id, Ground, Survey.No, Year) %>% group_by(id, Ground, Survey.No, Year) %>% summarize(Survey.No = Survey.No, Year = Year)
# surveysummary$Year = as.factor(surveysummary$Year)
# surveysummary$Survey.No = as.factor(surveysummary$Survey.No)
# larvsummary = left_join(surveysummary, larvsummary)
# 
# larv = larv %>%
#   mutate(Larv_per_jar = Abundance/No_jars) %>%
#   mutate(Volume = ifelse(Volume < 0.01, NA, Volume)) %>%
#   mutate(Density = Larv_per_jar/Volume)

###Calculating AVERAGE SE/mean/min/max of larval measurements. 
### This is all Darren's old code. Do we still need this?

# larv <- larv %>%
#   group_by(Ground, Survey.No, Year) %>%
#   mutate(AdjSD = sd(LengthAdjustment), AdjustedMinLength = min(LengthAdjustment), AdjustedMaxLength = max(LengthAdjustment), MeanLengthAdjustment = MeanLengthAdjustment, Abundance = length(LengthAdjustment)) %>%
#   ungroup()
# 
# larv$AdjSD[is.na(larv$AdjSD)] <- 0
# larv$AdjSD <- as.numeric(larv$AdjSD)
# 
# larvsummary <- larv %>% group_by(Ground, Survey.No, Year) %>%
#   summarize(MeanAdjustedMinLength = mean(AdjustedMinLength, na.rm = TRUE), 
#             MeanAdjustedMaxLength = mean(AdjustedMaxLength, na.rm = TRUE), 
#             MeanAdjustedSD = mean(AdjSD, na.rm = TRUE),
#             Abundance = length(LengthAdjustment)) %>%
#   mutate(SE =  MeanAdjustedSD/sqrt(Abundance))
# surveysummary = survey %>% dplyr::select(id, Ground, Survey.No, Year) %>% group_by(id, Ground, Survey.No, Year) %>% summarize(Survey.No = Survey.No, Year = Year)
# surveysummary$Year = as.factor(surveysummary$Year)
# surveysummary$Survey.No = as.factor(surveysummary$Survey.No)
# larvsummary = left_join(surveysummary, larvsummary)
# 
# 
# larv = larv %>%
#   mutate(Larv_per_jar = Abundance/No_jars) %>%
#   mutate(Volume = ifelse(Volume < 0.01, NA, Volume)) %>%
#   mutate(Density = Larv_per_jar/Volume)


larv = larv %>%
  dplyr::select(Ground, 
                id, 
                Date, 
                Survey.No, 
                No_jars, 
                Abundance, 
                Lengthmm, 
                category, 
                #MinLength, 
                #MaxLength, 
                #MeanLength, 
                SD, 
                Larv_per_jar, 
                Density, 
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
                Date,
                Year,
                id,
                TowReplicate,
                TowID,
                Survey.No,
                Abundance,
                Density,
                Volume,
                Preservative,
                MeanLengthAdjustment,
                AdjustedMeanAgeInDays,
                AdjustedMinDateOfSpawn,
                AdjustedMaxDateOfSpawn,
                Lon1, 
                Lat1, 
                Lon2, 
                Lat2, 
                TowTime, 
                AvgTowDepth, 
                MaxTowDepth, 
                CTDAvgTemp, 
                Volume)

LarvalSum <- unique(LarvalSum)
                

larv %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/Full Larval.csv"))
larv %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/Github/HerringScience.github.io/Source Data/Full Larval.csv"))

LarvalSum %>% write.csv(paste0("C:/Users/", Sys.info()[7],"/Documents/Github/HerringScience.github.io/Source Data/Larval Data/LarvalSum.csv"))

