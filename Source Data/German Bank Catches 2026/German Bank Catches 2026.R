rm(list=ls())

repo <-  file.path(
  path.expand("~"),
  "GitHub",
  "HerringScience.github.io",
  "Source Data",
  "German Bank Catches 2026"
)

library(lubridate)
library(ggplot2)
library(patchwork)
library(scales)
library(cli)
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
library(oce)
library(pander)
library(gt)

insideTheBox <- read_csv(file.path(repo, "In the Box catches.csv"))
outsideTheBox <- read_csv(file.path(repo, "Outside the box catches.csv"))



insideTheBoxPlot <- 
  ggplot() +
  geom_line(data = insideTheBox,
             aes(x = `Length (cm)`, y = `29-06-2026`, colour = "29-06-2026"),
             size = .5) +
  geom_point(data = insideTheBox,
             aes(x = `Length (cm)`, y = `29-06-2026`, colour = "29-06-2026"), 
             size = 1) +
  geom_line(data = insideTheBox,
            aes(x = `Length (cm)`, y = `30-06-2026`, colour = "30-06-2026"), 
            size = .5) +
  geom_point(data = insideTheBox,
             aes(x = `Length (cm)`, y = `30-06-2026`, colour = "30-06-2026"),
             size = 1) +
  geom_line(data = insideTheBox,
            aes(x = `Length (cm)`, y = `11-08-2026`, colour = "11-08-2026"), 
            size = .5) +
  geom_point(data = insideTheBox,
             aes(x = `Length (cm)`, y = `11-08-2026`, colour = "11-08-2026"), 
             size = 1) +
  geom_line(data = insideTheBox,
            aes(x = `Length (cm)`, y = Total, colour = "Total"), 
            size = .5) +
  geom_point(data = insideTheBox,
             aes(x = `Length (cm)`, y = Total, colour = "Combined Total"), 
             size = 1) +
  scale_x_continuous(
    "Length (cm)", seq(17.5, 32, by = 1)) +
  scale_y_continuous(
    breaks = seq(0, 200, by = 20)) +
  labs(title = "2026 Length Frequencies that were caught within the German Bank/Seal Island box.", 
       x = "Length of fish (cm)", 
       y = "Abundance of fish in length class"
  ) +
  scale_colour_manual(
    name = "Survey Date",
    values = c(
      "29-06-2026" = "blue",
      "30-06-2026" = "red",
      "11-08-2026" = "green",
      "Combined Total" = "black"
    )
  ) +
  theme_bw()

print(insideTheBoxPlot)

insideTheBoxTable <- 
  insideTheBox %>%
  gt() %>%
  tab_header(
    title = "Length Frequencies Within the German Bank/Seal Island Box"
  )

print(insideTheBoxTable)

outsideTheBoxPlot <-
  ggplot() +
  geom_line(data = outsideTheBox,
            aes(x = `Length (cm)`, y = `2026-06-10`, colour = "2026-06-10"),
            size = .5) +
  geom_point(data = outsideTheBox,
             aes(x = `Length (cm)`, y = `2026-06-10`, colour = "2026-06-10"), 
             size = 1) +
  geom_line(data = outsideTheBox,
            aes(x = `Length (cm)`, y = `2026-07-31`, colour = "2026-07-31"), 
            size = .5) +
  geom_point(data = outsideTheBox,
             aes(x = `Length (cm)`, y = `2026-07-31`, colour = "2026-07-31"),
             size = 1) +
  geom_line(data = outsideTheBox,
            aes(x = `Length (cm)`, y = `2026-06-11`, colour = "2026-06-11"), 
            size = .5) +
  geom_point(data = outsideTheBox,
             aes(x = `Length (cm)`, y = `2026-06-11`, colour = "2026-06-11"), 
             size = 1) +
  geom_line(data = outsideTheBox,
            aes(x = `Length (cm)`, y = `2026-07-09`, colour = "2026-07-09"), 
            size = .5) +
  geom_point(data = outsideTheBox,
             aes(x = `Length (cm)`, y = `2026-07-09`, colour = "2026-07-09"), 
             size = 1) +
  geom_line(data = outsideTheBox,
            aes(x = `Length (cm)`, y = `2026-07-01`, colour = "2026-07-01"), 
            size = .5) +
  geom_point(data = outsideTheBox,
             aes(x = `Length (cm)`, y = `2026-07-01`, colour = "2026-07-01"), 
             size = 1) +
    geom_line(data = outsideTheBox,
            aes(x = `Length (cm)`, y = Total, colour = "Total"), 
            size = .5) +
  geom_point(data = outsideTheBox,
             aes(x = `Length (cm)`, y = Total, colour = "Combined Total"), 
             size = 1) +
  scale_x_continuous(
    "Length (cm)", seq(17.5, 32, by = 1)) +
  scale_y_continuous(
    breaks = seq(0, 200, by = 20)) +
  labs(title = "2026 Length Frequencies that were caught near by the German Bank/Seal Island box.", 
       x = "Length of fish (cm)", 
       y = "Abundance of fish in length class"
  ) +
  scale_colour_manual(
    name = "Survey Date",
    values = c(
      "2026-06-10" = "blue",
      "2026-07-31" = "red",
      "2026-06-11" = "green",
      "2026-07-09" = "brown",
      "2026-07-01" = "purple",
      "Combined Total" = "black"
    )
  ) +
  theme_bw()

print(outsideTheBoxPlot)

outsideTheBoxTable <- 
  outsideTheBox %>%
  gt() %>%
  tab_header(
    title = "Length Frequencies nearby the German Bank/Seal Island Box"
  )

print(outsideTheBoxTable)

ggsave(
  filename = file.path(repo, "2026 Length Frequencies that were caught within the German Bank or Seal Island box.png"),
  plot = insideTheBoxPlot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(repo, "2026 Length Frequencies that were caught just outside the German Bank or Seal Island box.png"),
  plot = outsideTheBoxPlot,
  width = 10,
  height = 6,
  dpi = 300
)
