

# Larval_Results



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


# Load the data in:


## Load Larval data:
# The Larval data
Larval = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Full Larval.csv"))

#original was from Main Data, Jan 2025
LarvalSum = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/LarvalSum.csv"))

## Need to bring in planktonsamplingData
plank = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/planktonsamplingData.csv"))

## Look at original Larval Measurements to check out Abundance as it seems wrong from Larval Sum
measure = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/Larval Measurements.csv"))


################################################################

head(LarvalSum)
dim(LarvalSum)


LarvalSum$Year <- as.factor(LarvalSum$Year)
Larval$Date <- lubridate::ymd(Larval$Date)
Larval <- dplyr::arrange(Larval, Date)
Larval$Year <- as.factor(Larval$Year)
Larval$category <- as.factor(Larval$category)
Larval$Survey.No <- as.factor(Larval$Survey.No)
Larval$MonthDay <- format(Larval$Date, "%m-%d")

#Changed to X and Y to fit in better with compendium code. These are the tow start and finish coordinates.

names(Larval)[names(Larval) =="Lon1"] <- "X"
names(Larval)[names(Larval) =="Lat1"] <- "Y"
names(Larval)[names(Larval) =="Lon2"] <- "Xend"
names(Larval)[names(Larval) =="Lat2"] <- "Yend"

#Seal Island Larval
LarvalSI = filter(Larval, Ground == "SI")
LarvalSI = merge(LarvalSI, LarvalSum[,c("id", "TowReplicate", "TowID")], by = "id")

Larval = merge(Larval, LarvalSum[,c("id", "TowReplicate", "TowID")], by = "id")

head(Larval)




# Calculate the average number of larvae per tow within each year. plot totals.


LarvalSumSB=LarvalSum[which(LarvalSum$Ground == "SB"), ]
#only 1 year 2019
LarvalSumSI=LarvalSum[which(LarvalSum$Ground == "SI"), ]
#only 1 year 2021
LarvalSumGB=LarvalSum[which(LarvalSum$Ground == "GB"), ]

head(LarvalSumSB)


se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

# Scots Bay
# mean number captured
mean(LarvalSumSB$Abundance)
se(LarvalSumSB$Abundance)

# mean length
mean(LarvalSumSB$LengthAdjustment)
se(LarvalSumSB$LengthAdjustment)
            
            # German Bank
            # mean number captured
            mean(LarvalSumGB$Abundance)
            se(LarvalSumGB$Abundance)
            
            # mean length
            mean(LarvalSumGB$LengthAdjustment)
            se(LarvalSumGB$LengthAdjustment)

                    # Seal Island
                          
                          # mean number captured
                          mean(LarvalSumSI$Abundance)
                          se(LarvalSumSI$Abundance)
                          
                          # mean length
                          mean(LarvalSumSI$LengthAdjustment)
                          se(LarvalSumSI$LengthAdjustment)


###

larv2017=Larval[which(Larval$Year == "2017"), ]
larv2018=Larval[which(Larval$Year == "2018"), ]
larv2019=Larval[which(Larval$Year == "2019"), ]
larv2020=Larval[which(Larval$Year == "2020"), ]
larv2021=Larval[which(Larval$Year == "2021"), ]

dat %>%
  group_by(site) %>%
  summarise(mean_length = mean(length_mm, na.rm = TRUE))


## Plankton sampling dataframe
plank$Date = as.Date(plank$Date, format = "%d/%m/%Y")

plankSB=plank[which(plank$Ground == "SB"), ]
#only 1 year 2019
plankSI=plank[which(plank$Ground == "SI"), ]
#only 1 year 2021
plankGB=plank[which(plank$Ground == "GB"), ]

plankT = rbind(plankSB, plankSI, plankGB)
dim(plankT)
head(plankT)

plankT$Year = year(plankT$Date)

unique(plankT$Year)

plankHist <- plankT[plankT$Year %in% c("2020", "2021", "2017", "2018", "2019" ), ]
unique(plankHist$Year)

colnames(plankHist)

# unique dates

length(unique(plankHist$Date))
dim(plankHist)

94-58
36/2

write.table(plankHist, file= "planktonHist.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 



plankSB2017=plankSB[which(plankSB$Year == "2017"), ]
plankSB2018=plankSB[which(plankSB$Year == "2018"), ]
plankSB2019=plankSB[which(plankSB$Year == "2019"), ]
plankSB2020=plankSB[which(plankSB$Year == "2020"), ]
plankSB2021=plankSB[which(plankSB$Year == "2021"), ]


plankH = rbind (plankSB2017, plankSB2018, plankSB2019, plankSB2020, plank2021)


#unique dates 41
dim(plankH)

unique(plankH$Ground)


dim(plank)
210-117
93/2


# Look at larval measurements and calculate abundance/id

head(measure)
measure %>%
  group_by(id) %>%
  summarise(n = n())


counts <- measure %>%
  count(id)

colnames(counts) =c("id", "Abundance_2")
head(counts)
dim(counts)

QC <- LarvalSum %>%
  left_join(counts, by = "id")

QC$Abundance
QC$Abundance_2

df_diff <- QC %>%
  mutate(different = Abundance_2 != Abundance)

write.table(df_diff, file= "abundanceDifferences.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

new_df <- QC[, c("id", "Abundance_2")]


### Need to replace Abundance in Larval Sum with Abundance_2 then save the .csv where the compendium looks
library(dplyr)

df_new <- dplyr::select(LarvalSum, -Abundance)

LS <- df_new %>%
  left_join(new_df, by = "id")

LS$Abundance_2
LS$Abundance  = LS$Abundance_2

new_df <- LS[, !names(LS) %in% "Abundance_2"]

LarvalSum = new_df


write.table(LarvalSum, file= "LarvalSum.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

            