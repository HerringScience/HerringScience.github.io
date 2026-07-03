# Global options
rm(list = ls())


#Import libraries

library(cli)
library(car)
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
library(measurements)
library(geodata)
library(terra)
library(sf)
library(rnaturalearth)

setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/"))

#Files to import

TotalFatData <- read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/Total Fat Data.csv")

TotalFatData <- TotalFatData %>%
  tidyr::drop_na('FishWeight(g)', 'FishLength(cm)')


#Create scatterplot to compare lengths and when they dropped to fillet weight.

ggplot (TotalFatData, aes(x = `FishWeight(g)`, y = `FishLength(cm)`, colour = factor(Year))) +
  geom_point(size = 1, alpha = .7) +
  labs (
    title = "Fish Length by Fish Weight",
    x = "Fish Weight (g)",
    y = "Fish Length (cm)"
  ) +
  theme_minimal()


ggsave(
  filename = paste0(
    "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Fat Data/Fish Length by Weight graphs/Fish length by fish weight.png"
  ),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

# Split by 5cm size classes

TotalFatData <- TotalFatData %>%
  mutate(LengthClass = cut(
    `FishLength(cm)`,
    breaks = seq(
      floor(min(`FishLength(cm)`, na.rm = TRUE) / 5) * 5,
      ceiling(max(`FishLength(cm)`, na.rm = TRUE) / 5) * 5,
      by = 5
    ),
    include.lowest = TRUE
  ))


for (grp in unique(TotalFatData$LengthClass)) {
  
  p <- ggplot(
    subset(TotalFatData, LengthClass == grp),
    aes(x = `FishWeight(g)`,
        y = `FishLength(cm)`,
        colour = factor(Year))
  ) +
    geom_point(size = 3) +
    labs(
      title = paste("Length Class", grp),
      x = "Fish Weight (g)",
      y = "Fish Length (cm)",
      colour = "Year"
    ) +
    theme_minimal()
  
  print(p)
  
  ggsave(
    filename = paste0(
      "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Fat Data/Fish Length by Weight graphs/Length Class_",
      grp,
      ".png"
    ),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
}
