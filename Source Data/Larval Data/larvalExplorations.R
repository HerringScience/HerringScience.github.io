## What year did we start doing larval tow replicates??

#Packages
library(ggplot2)
library(patchwork)
library(scales)
library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
library(sf)
library(terra)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(DT)
library(dygraphs)
library(leaflet)
library(rmapshaper)
library(plotly)
library(mapproj)
library(oce) #new CTD Data package
library(pander)
library(geodata)
library(pacman)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(devtools)
library(maps)
library(dplyr)


# The larval data
Larval = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Full Larval.csv"))
#original was from Main Data, Jan 2025
LarvalSum = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Main Data/LarvalSum Jan 2025.csv"))
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




replicates=Larval[which(Larval$TowReplicate == TRUE), ]
unique(replicates$Year)


larvaldepth = (Larval$id)
Larval[which(Larval$id == TRUE), ]

depth=Larval[which(Larval$TowReplicate == TRUE), ]
unique(replicates$Year)


# average tow depth figure



# use TowTime from Tow Depths.R

head(TowTimes)
years = c('2017', '2018', '2019', '2020', '2021')
TowTimes2021 = TowTimes[which((TowTimes$Year %in% years)), ]
unique(TowTimes2021$Year)
head(TowTimes2021)


write.table(TowTimes, file= "TowTimes.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


Seal=TowTimes2021[which(TowTimes2021$Ground == "SI"), ]
# summarize number of tow/ground
groundCounts <- TowTimes2021 %>%
  count(Ground)

ggplot(groundCounts, aes(x = Ground, y = n)) +
  geom_col()

# why is there two NA's? Other grounds, only in 2017 Long Island and Northeast Bank it looks like.
TowTimes2021NA=TowTimes2021[which(TowTimes2021$Ground == NA), ]

sum(35+57+2+2)


TowTimes2021$Year = as.factor(TowTimes2021$Year)
TowTimes2021 <- TowTimes2021[order(TowTimes2021$Year), ]

TowTimes2021$Year <- factor(TowTimes2021$Year, levels = TowTimes2021$Year)

ggplot(data = TowTimes2021, aes(Year, AvgTowDepth, colour = Year)) + geom_jitter(size=3)+scale_y_continuous(breaks = seq(0, 30, by = 5))+ labs(y = "Average Tow Depth (m)", x = "Year")


theme(
  panel.background = element_blank(),          # remove grey background
  panel.grid.major = element_line(),           # keep major gridlines
  panel.grid.minor = element_line(),           # keep minor gridlines
  panel.border     = element_rect(color = "black", fill = NA, size = 1)
)



quantile(TowTimes2021$AvgTowDepth, probs = c(0, 0.05, 0.25, 0.50, 0.90, 0.95, 1), na.rm=TRUE)



+
  geom_jitter(size = 1.25, width = 3) +
  geom_hline(yintercept = 8,  linetype = "longdash", size = 1, colour = "red") +
  geom_hline(yintercept = 12, linetype = "longdash", size = 1, colour = "blue") +
  geom_hline(yintercept = 17, linetype = "longdash", size = 1, colour = "forestgreen") +
  geom_hline(yintercept = 27, linetype = "longdash", size = 1, colour = "grey70") +
  scale_y_reverse() +
  labs(x = "Average Tow Depth (m)", 
       y = "Larval Length (mm)", 
       colour = "id")
)


for(i in unique(Larval$Year)) {
  
  cat("\n")
  cat("#####", i, "\n")
  cat("\n")
  
  Larval1 <- Larval %>% 
    filter(Year == i, Ground == "SB")
  
  print(
    ggplot(data = Larval1, aes(AvgTowDepth, LengthAdjustment, colour = id)) +
      geom_jitter(size = 1.25, width = 3) +
      geom_hline(yintercept = 8,  linetype = "longdash", size = 1, colour = "red") +
      geom_hline(yintercept = 12, linetype = "longdash", size = 1, colour = "blue") +
      geom_hline(yintercept = 17, linetype = "longdash", size = 1, colour = "forestgreen") +
      geom_hline(yintercept = 27, linetype = "longdash", size = 1, colour = "grey70") +
      scale_y_reverse() +
      labs(x = "Average Tow Depth (m)", 
           y = "Larval Length (mm)", 
           colour = "id")
  )
  
  cat("\n")
  
  TotalCategories <- Larval1 %>%
    group_by(category) %>%
    summarize(n = n(),
              MeanDepth = mean(AvgTowDepth, na.rm = TRUE)) %>%
    mutate(Percentage = (n/sum(n))*100) %>%
    mutate_if(is.numeric, format, digits = 1) %>%
    relocate(MeanDepth, .after = Percentage)
  
  print(
    kbl(TotalCategories,
        col.names = c("Category", "Abundance", "%", "Avg Tow Depth (m)"),
        align = "c") %>%
      kable_paper("striped", full_width = FALSE)
  )
  
  cat("\n")
}
               