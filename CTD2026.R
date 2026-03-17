
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
library(coin)


# CTD DATA

# Look at DFO and HSC data

source("build_Oceans_df.R")


Oceans  = build_Oceans_df(ctd_path = "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv", dfo_paths = c("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/herringLarvalSurveyClimateData.csv", "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/herringLarvalSurvey2009Data.csv"))

head(Oceans)
unique(Oceans$Year)



#QC oceans for extreme temps
colnames(Oceans)
hist(Oceans$Temperature)

#OceansCheck=Oceans[which(Oceans$Temperature > 25), ]
#OceansSB=Oceans[which(Oceans$ground == "Scots Bay"), ]
#OceansGB=Oceans[which(Oceans$ground == "German Bank"), ]

se <- function(x, na.rm = TRUE) {
  x <- if (na.rm) x[!is.na(x)] else x
  stats::sd(x) / sqrt(length(x))
}


## Look at depth 

ggplot(data = Oceans, aes(x = JulianDay, y = Depth, colour = Source)) + geom_jitter() + ggtitle("Depth Distribution of Samples in German Bank")

# Add lat and long to these dataframes
source("surface_Oceans.R")
source("depth_Oceans.R")
source("strat_Oceans.R")

#Run Functions on Oceans

#1.)
surface <- surface_Oceans(Oceans)

#2.)
atDep <- depth_Oceans(Oceans, depth_lower = 29, depth_upper = 31)

#3.)
strat <- strat_index(
  data      = Oceans,
  upper_min = 0,
  upper_max = 5,
  lower_min = 20,
  lower_max = 30
)


write.table(surface, file= "surface.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

write.table(atDep, file= "atDep.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

write.table(strat, file= "strat.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 



#################################################
# Surface Works:

surface <- surface %>%
  mutate(
    JD_bin = case_when(
      JulianDay >= 181 & JulianDay <= 220 ~ "EarlySummer",
      JulianDay >= 221 & JulianDay <= 260 ~ "LateSummer",
      JulianDay >= 261 & JulianDay <= 300 ~ "Fall",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(JD_bin))

hist(surface$depth1)

# table for paper
bin_counts_wide <- bin_counts %>%
  tidyr::pivot_wider(
    names_from = Source,
    values_from = n_samples,
    values_fill = 0
  )

          bin_counts_wide
          
          bin_counts_wide <- bin_counts_wide %>%
            mutate(
              usable_for_inference = DFO >= 10 & HSC >= 10
            )
          
          bin_counts_wide
          
          bin_years <- surface %>%
            filter(!is.na(JD_bin)) %>%
            group_by(ground, Source, JD_bin) %>%
            summarise(
              years = paste(sort(unique(Year)), collapse = ", "),
              .groups = "drop"
            )
          
          write.table(bin_years, file= "surfaceBinYears.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

#Bin Stats:

unique(surface$JD_bin)          
          
          
valid_bins <- tibble::tribble(
  ~ground,         ~JD_bin, ~use,
  "Scots Bay",     "EarlySummer",   TRUE,
  "Scots Bay",     "LateSummer",  TRUE,
  "Scots Bay",     "Fall",  FALSE,
  "German Bank",   "EarlySummer",   FALSE,
  "German Bank",   "LateSummer",  TRUE,
  "German Bank",   "Fall",  TRUE
)

source("run_perm_test.R")

results <- surface %>%
  filter(Source %in% c("DFO", "HSC")) %>%
  inner_join(valid_bins, by = c("ground", "JD_bin")) %>%
  filter(use) %>%
  group_by(ground, JD_bin) %>%
  group_modify(~ run_perm_test(.x, response = "avgTemp1")) %>%
  ungroup()

results

sample_sizes <- surface %>%
  filter(Source %in% c("DFO", "HSC")) %>%
  group_by(ground, JD_bin, Source) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Source, values_from = n)

final_results <- results %>%
  left_join(sample_sizes, by = c("ground", "JD_bin"))

final_results <- final_results %>%
  mutate(p_adj = p.adjust(p_value, method = "BH"))


final_results

# Seasonality and results
#Is SST changing through the season within a ground?
ggplot(surface, aes(JulianDay, avgTemp1, colour = Source)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~ ground) +
  labs(
    x = "Julian Day",
    y = "SST (0–5 m)",
    colour = "Data source"
  ) +
  theme_bw()

colnames(surface)

# Model

lm_season <- lm(
  avgTemp1 ~ JulianDay * Source,
  data = surface
)

summary(lm_season)

### QC

names(surface)
unique(surface$ground)
unique(surface$Source)

# If JD_bin exists:
if ("JD_bin" %in% names(surface)) unique(surface$JD_bin)

anti_join(
  valid_bins,
  surface %>% distinct(ground, JD_bin),
  by = c("ground", "JD_bin")
)





#SBsurface=surface[which(surface$ground == "Scots Bay"), ]
#hist(SBsurface$avgTemp1)

#GBsurface=surface[which(surface$ground == "German Bank"), ]
#hist(GBsurface$avgTemp1)

# Surface density: Differences in surface density between HSC and DFO were assessed using Welch two‑sample t‑tests and Mann–Whitney U tests within the overlapping sampling period (Julian days 210–238). No significant differences were detected (Welch t‑test, p = 0.63; Mann–Whitney U test, p = 0.42), and effect sizes were small (Cohen’s d = 0.12).”

# surface temp: Conclusion (Scots Bay): No statistically significant difference in surface temperature (avgTemp1) between HSC and DFO in the overlap season (210–238).
# Conclusion (Scots Bay): After controlling for Julian day, no significant HSC vs DFO temperature difference.

## surface density "For German Bank, surface density differed significantly between HSC and DFO within the overlapping sampling period (Julian days 223–308). Welch two‑sample t‑tests (p < 10⁻⁷) and Mann–Whitney U tests (p < 10⁻⁸) both indicated higher densities in DFO observations. The magnitude of the difference was large (Cohen’s d = −1.15)

# temperature:
#Conclusion (German Bank): HSC temperatures are significantly higher than DFO by ~2.22 °C within the overlap season (223–308), with a very large effect size. [herringsci...epoint.com]
#Conclusion (German Bank): Even after controlling for Julian day, HSC is significantly warmer than DFO by ~2°C.
#Conclusion (German Bank): The HSC‑warmer‑than‑DFO result is consistent in both early and late bins, and the difference is large.






# GB at depth:

GBdepth=atDep[which(atDep$ground == "German Bank"), ]

str(GBdepth$JulianDay)
colnames(GBdepth)

GBdepth2 <- GBdepth %>%
  dplyr::filter(JulianDay >= 200, JulianDay <= 350)

ggplot(GBdepth2, aes(x = JulianDay, y = avgTemp, colour = Source)) +
  geom_text(
    aes(label = Year),
    position = position_jitter(width = 6, height = 0.25),
    size = 3   # ← reduce this
  ) +
  ggtitle("Temporal Distribution of Samples in German Bank - 30m")



# GB Stratification
GBstrat=strat[which(strat$ground == "German Bank"), ]

GBstrat2 <- GBstrat %>%
  dplyr::filter(JulianDay >= 200, JulianDay <= 350)


ggplot(GBstrat2, aes(x = JulianDay, y = strat_density, colour = Source)) +
  geom_text(
    aes(label = Year),
    position = position_jitter(width = 6, height = 0.25),
    size = 3   # ← reduce this
  ) +
  ggtitle("Temporal Distribution of Samples in German Bank - Stratification Index")











#pre 2010
#post 2010

SBsurfaceH <- SBsurface[as.numeric(as.character(SBsurface$Year)) < 2010, ]

SBsurfaceP <- SBsurface %>% dplyr::filter(Year > 2010)

hist(SBsurfaceH$avgTemp1)
hist(SBsurfaceP$avgTemp1)


unique(Oceans$datatype)




######################################


CTD = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/CTD 30m.csv"))

CTD = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv"))




##### cont here:


### Sounds speed function
sound_speed_chen_millero <- function(T, S, z = 0) {
  # Pure water sound speed
  c_w <- 1402.388 +
    5.03830 * T -
    5.81090e-2 * T^2 +
    3.3432e-4 * T^3 -
    1.47797e-6 * T^4 +
    3.1419e-9 * T^5
  
  # Salinity correction
  A <- 1.389 +
    1.262e-2 * T +
    7.166e-5 * T^2 +
    2.008e-6 * T^3 -
    3.21e-8 * T^4
  
  B <- 9.4742e-5 -
    1.2580e-5 * T -
    6.4885e-8 * T^2 +
    1.0507e-8 * T^3 -
    2.0122e-10 * T^4
  
  C <- -1.922e-2 - 4.42e-5 * T
  
  c_s <- A * S + B * S^(3/2) + C * S^2
  
  # Pressure/depth correction
  D <- 1.727e-3 - 7.9836e-6 * T
  c_p <- D * z
  
  c_w + c_s + c_p
}



CTD$SoundSpeed = sound_speed_chen_millero(CTD$Temperature, CTD$Salinity, z=30)




CTDSB=CTD[which(CTD$Ground == "Scots Bay"), ]
CTDGB=CTD[which(CTD$Ground == "German Bank"), ]

# shapiro-wilk normality test
# p>0.05 Normal distribution
# p<0.05 Doesn't have a normal distribution

shapiro.test(CTDSB$StratTemp)
shapiro.test(CTDGB$StratTemp)

hist(CTDSB$StratTemp)
mean(CTDSB$StratTemp)
hist(CTDGB$StratTemp)
mean(CTDGB$StratTemp)


# Non parametric alternative to t-test - ranked sum - independent samples
# p > 0.05 : No significant difference
# p < 0.05 : Signficant differences

wilcox.test(CTDSB$StratTemp, CTDGB$StratTemp)

#significantly different

dim(CTDGB)
dim(CTDSB)

## SST
shapiro.test(CTDSB$SST)
shapiro.test(CTDGB$SST)

hist(CTDSB$SST)
mean(CTDSB$SST)
hist(CTDGB$SST)
mean(CTDGB$SST)

wilcox.test(CTDSB$SST, CTDGB$SST)
# significantly different


## Temp at depth (Temperature)


shapiro.test(CTDSB$Temperature)
shapiro.test(CTDGB$Temperature)

hist(CTDSB$Temperature)
hist(CTDGB$Temperature)

wilcox.test(CTDSB$Temperature, CTDGB$Temperature)
# NOT significantly different


# Sounds Speed
head(CTD)

hist(CTDSB$SoundSpeed)
hist(CTDGB$SoundSpeed)

shapiro.test(CTDSB$SoundSpeed)
shapiro.test(CTDGB$SoundSpeed)

wilcox.test(CTDSB$SoundSpeed, CTDGB$SoundSpeed)
# no statistical difference!




### Salinity
shapiro.test(CTDSB$SurfaceSalinity)
shapiro.test(CTDGB$SurfaceSalinity)

#these are normally distributed
# classical F-test for equal variances
var.test(CTDSB$SurfaceSalinity, CTDGB$SurfaceSalinity)   
# variances are equal enough for parametric testing

t.test(CTDSB$SurfaceSalinity, CTDGB$SurfaceSalinity, var.equal = TRUE)
# NOT singificantly different
hist(CTDSB$SurfaceSalinity)
hist(CTDGB$SurfaceSalinity)


