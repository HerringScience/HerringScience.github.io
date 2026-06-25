### Survey Plans

#Import all packages and data
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

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates")

boxes = read.csv("surveyBoxes.csv")
head(boxes)
unique(boxes$Box)

SBctd=boxes[which(boxes$Box == "SBocean"), ]
north=boxes[which(boxes$Box == "SBocean"), ]
SBctd=boxes[which(boxes$Box == "SBocean"), ]

# Scots Bay CTD Box

poly_coords <- matrix(
  c(
    -65.218, 45.062,  # top-left
    -65.218, 45.050,  # bottom-left
    -65.200, 45.050,  # bottom-right
    -65.200, 45.062,  # top-right
    -65.218, 45.062   # close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

SB_CTD <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "Scots Bay", geometry = _)





# Scots Bay survey box main
SUA = read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")
surveySB_poly <- polySB_main %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(NBNS_sf)) %>%
  group_by(PID) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# German Bank survey Box
SUA = read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")
surveyGB_poly <- polyGB %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(NBNS_sf)) %>%
  group_by(PID) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Seal Island survey box
SUA = read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")
surveySI_poly <- polySI %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(NBNS_sf)) %>%
  group_by(PID) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# German Bank CTD Box
df <- tribble(
  ~id, ~Box, ~Y, ~X,
  34, "German Bank", 43.7, -66.229,
  35 , "German Bank",43.56667, -66.229,
  36 , "German Bank",43.56667, -66.075,
  37, "German Bank", 43.233, -66.075,
  38, "German Bank", 43.233, -66.55,
  39, "German Bank", 43.7, -66.55,
  40, "German Bank", 43.7, -66.229   # closes the polygon
)
# Convert to polygon
poly_coords <- df %>%
  arrange(id) %>%
  select(X, Y) %>%
  as.matrix()

GB_CTD <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "German Bank", geometry = _)



# need eastern and northern boxes in Scots Bay

# Northern

poly_coords <- matrix(
  c(
    -65.0540, 45.247,   # SE
    -65.0599, 45.293,   # east-mid
    -64.8800, 45.344,   # NW
    -64.8300, 45.314,
    -65.0540, 45.247    # close
  ),
  ncol = 2, byrow = TRUE
)

nSB_poly <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "Northern Box", geometry = _)



# Eastern Box
poly_coords <- matrix(
  c(
    -64.76500, 45.276,
      -64.68200, 45.218,   # start (south-ish)
    -64.54990, 45.255,   # northeast-ish
    -64.549, 45.34,    # north
    -64.765, 45.276    # close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

eSB_poly <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "Eastern Box", geometry = _)

# now need plankton tow box
df <- tribble(
  ~id, ~Box, ~Y, ~X,
  17, "SB_Plankton", 45.030, -65.255,
  18 , "SB_Plankton",45.070, -65.110,
  19 , "SB_Plankton",45.113, -65.110,
  20, "SB_Plankton", 45.075, -65.255,
  21, "SB_Plankton", 45.030, -65.255
  # closes the polygon
)
# Convert to polygon
poly_coords <- df %>%
  arrange(id) %>%
  select(X, Y) %>%
  as.matrix()

SB_plank <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "SB Plankton", geometry = _)




####################
# Load land data
can <- gadm(country = "CAN",
            level   = 1,
            path    = "geodata_default_path",
            version = "latest",
            resolution = 1)

NBNS <- can[can$NAME_1 %in% c("New Brunswick", "Nova Scotia"), ]

NBNS_sf <- st_as_sf(NBNS)

# Scots Bay
CP <- ext(-65.4, -64.5, 45, 45.5)
CP <- as.polygons(CP)
crs(CP) <- crs(NBNS)
CP_sf <- st_as_sf(CP)

NBNS_crop <- st_intersection(NBNS_sf, CP_sf)


# ALL map

#all
CP <- ext(-66.5, -62, 44.5, 45.5)


ggplot(NBNS_sf) +
  geom_sf(data = NBNS_sf, fill = "lightgray", color = "black") +
  geom_sf(data = SB_CTD, fill = "lightblue", color = "black") + geom_sf(data = GB_CTD, fill = "purple", color = "black")+ geom_sf(data = surveyGB_poly, fill = "pink", color = "black") + geom_sf(data = surveySI_poly, fill = "pink", color = "black") + geom_sf(data = surveySB_poly, fill = "navy", color = "black") + geom_sf(data = SB_plank, fill = "navy", color = "black") + geom_sf(data = nSB_poly, fill = "navy", color = "black") + geom_sf(data = eSB_poly, fill = "navy", color = "black") 



#now we need our lines:
setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Surveys/2026/SB3")

lines = read.csv("june7_2026Plan.csv")

# Scots Bay
ggplot(NBNS_crop) +
  geom_sf(data = NBNS_crop, fill = "tan", color = "black") + geom_sf(data = surveySB_poly, fill = "ivory2", color = "black") + geom_sf(data = SB_plank, fill = "ivory2", color = "black") + geom_sf(data = nSB_poly, fill = "ivory2", color = "black") + geom_sf(data = eSB_poly, fill = "ivory2", color = "black")  +
  geom_sf(data = SB_CTD, fill = "ivory2", color = "black") + 
  geom_segment(data=lines, aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size=0.6) +labs(x=NULL, y=NULL, caption = "Figure 1. Survey lines to be completed by participating vessels.")

library(readr)
library(dplyr)
library(geosphere)

# Calculate distance for each survey line (meters)
lines_dist <- lines %>%
  mutate(
    distance_m = distGeo(
      p1 = cbind(X, Y),
      p2 = cbind(Xend, Yend)
    ),
    distance_km = distance_m / 1000
  )

# Total distance by vessel
boat_totals <- lines_dist %>%
  group_by(Vessel) %>%
  summarise(
    n_lines = n(),
    total_km = sum(distance_km, na.rm = TRUE),
    mean_line_km = mean(distance_km, na.rm = TRUE)
  ) %>%
  arrange(desc(total_km))

boat_totals










#######################

# Scots Bay plankton and CTD box
SBplankton=boxes[which(boxes$Box == "SBPlanktonBox"), ]
SBCTD=boxes[which(boxes$Box == "SBocean"), ]
GBCTD=boxes[which(boxes$Box == "GBocean"), ]

# Scots Bay
SUA = read.csv("polygon_SBEastern.csv")
polyEastern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SBNorthern.csv")
polyNorthern = as.PolySet(SUA, projection="LL")

SUA = read.csv("polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")

# German Bank      

SUA = read.csv("polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")

# Seal Island      

SUA = read.csv("polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")



















########


# Proper coordinates for Scots Bay
CP <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")

# look closely at the eastern box
CP <- as(extent(-64.8, -64.5, 45.2, 45.4), "SpatialPolygons")

# look closely at the northern box
CP <- as(extent(-65.25, -64.75, 45.2, 45.4), "SpatialPolygons")

# Proper coordinates for German Bank
CP <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")

#Land Data - loads the land data the same way as Jenna's previous scripts
setwd(paste0("C:/Users/", Sys.info()[7], "/Documents/GitHub/HerringScience.github.io/"))
can<-getData('GADM', country="CAN", level=1)
us = getData('GADM', country = "USA", level = 1)
can1 = rbind(can,us)
NBNS <- can1[can1@data$NAME_1%in%c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Québec", "Maine"),]

# # Proper coordinates for German Bank
# GBMap <- as(extent(-66.5, -65.5, 43, 44), "SpatialPolygons")
# proj4string(GBMap) <- CRS(proj4string(NBNS))
# #GBout <- gIntersection(NBNS, GBMap, byid=TRUE)
# GBout <-crop(NBNS, GBMap, byid=TRUE)

# Proper coordinates for Scots Bay
SBMap <- as(extent(-65.5, -64.5, 45, 45.5), "SpatialPolygons")
proj4string(SBMap) <- CRS(proj4string(NBNS))
#SBout <- gIntersection(NBNS, SBMap, byid=TRUE)
SBout <- crop(NBNS, SBMap, byid=TRUE)

#Make CTD Cast Box (GB or SB)
a = c("4334.320", "4333.600", "4333.600", "4334.320")
b = c("6622.080", "6622.080", "6621.000", "6621.000")
c = c("4503.432", "4503.000", "4503.000", "4503.432")
d = c("6513.048", "6513.048", "6512.000", "6512.000")


#changed gIntersection to crop as it was discontinued
proj4string(CP) <- CRS(proj4string(NBNS))
out <- crop(NBNS, CP, byid=TRUE)


# Example of a Scots Bay Survey 

# Plan with all the transects, main, northern and eastern:
 setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Surveys/2024/SB2/"))
#trackSB = read.csv("july7_plan_ScotsBay.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
 
 #trackSB = read.csv("may29_2022Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
 
 trackSB = read.csv("May_20_2024SurveyPlanMain.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)



# Example of a German Bank/Seal Island Plan
# trackGB = read.csv("exampleGBSIplan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# 
# trackGB = read.csv("aug30_2020Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# trackGB = read.csv("oct25_2020Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# 
# 
# trackGB = read.csv("sep13_2020Plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# 
# # Plot the transects: German Bank/Seal Island
# ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=GBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")
# 
# 
# # Plot the transects: German Bank/Seal Island
# ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=GBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")
# 
# 
# ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")
# 


# Load in all the survey polygons 


# Plot the transects: Scots Bay

setwd(paste0("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Surveys/2024/SB2/"))

track1 = read.csv("May_20_2024SurveyPlanNorthxlsx.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

track1 = read.csv("May_20_2024SurveyPlanEastern.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

track1 = read.csv("May_20_2024SurveyPlanMain.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)



# This map was taken from Tag Returns and works. X and Ys were flipped.
ggplot(track1, aes(x=X, y=Y)) + 
  geom_polygon(data=SBout,aes(x=long, y=lat, group=group), fill = "gray") + 
  geom_polygon(data=SBplankton,aes(x=X, y=Y, group=PID),fill='white',col='black') +
  geom_polygon(data=SBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black') +
  geom_segment(aes(x=X, y=Y, xend=Xend, yend=Yend, colour=Vessel), size=0.5) +
  coord_map() + 
  labs(x=NULL, y=NULL, caption = "Figure 1. Survey lines to be completed by participating vessels.")


# 
# ggplot(trackSB, aes(x=X, y=Y))+ geom_polygon(data=polyEastern,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=SBplankton,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=SBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("") + ggtitle("Scots Bay Survey Plan July 7, 2019")
# 
# 
# # May 2021
# ggplot(trackSB, aes(x=X, y=Y))+ geom_polygon(data=polyNorthern,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("") + ggtitle("Scots Bay Survey Plan May 2021")
# 
# 
# 
# 
# trackSB = read.csv("sep20_2020plan.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# trackSB = read.csv("planSep20_20.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# trackSB = read.csv("CplanSep20_20.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# 
# ggplot(trackSB, aes(x=X, y=Y))+ geom_polygon(data=polySB_main,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77") + geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("") + ggtitle("Option C")
# 
# 
# 
# 
# # Plot the transects: German Bank/Seal Island
# # ggplot(trackGB, aes(x=X, y=Y))+ geom_polygon(data = out, aes(x=long, y=lat, group=group), fill = "grey77")+ geom_polygon(data=polyGB,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_polygon(data=polySI,aes(x=X, y=Y, group=PID),fill='white',col='black') + geom_polygon(data=GBCTD,aes(x=X, y=Y, group=PID),fill='white',col='black')+ geom_segment(aes(x = X, y = Y, xend = Xend, yend = Yend, colour = Vessel), size = 1) + coord_map() + xlab("")+ ylab("")+ ggtitle("German Bank Seal Island Survey Plan August 30, 2020")
# # 
# # 
# 
# 
# # Determine the ordering of polygon points
# ggplot(polyEastern, aes(x=X, y=Y))+ geom_polygon(fill="transparent")+ geom_text(aes(label=POS))
# 
# # Lines for the Eastern and Northern Boxes
# track = read.csv("easternLines2020.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)
# track = read.csv("northernLines2020.csv", header=TRUE, sep=",", row.names = NULL, stringsAsFactors=FALSE)

