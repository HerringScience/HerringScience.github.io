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


# This is total number of larval
groundCounts0 <- Larval %>%
  count(Ground)

unique(Larval$Year)

counts = unique(Larval$id)

counts2 <- data.frame(col = counts)
sum(37-3-2)
####################

# When did we start doing replicates?

replicates=Larval[which(Larval$TowReplicate == TRUE), ]
unique(replicates$Year)


larvaldepth = (Larval$id)
Larval[which(Larval$id == TRUE), ]

depth=Larval[which(Larval$TowReplicate == TRUE), ]
unique(replicates$Year)


# average tow depth figure



# use TowTime from Tow Depths.R (needs to be loaded from that script)

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
  geom_col()+ labs(y = "Number of Tows", x = "Ground") 


# why is there two NA's? Other grounds, only in 2017 Long Island and Northeast Bank it looks like.
TowTimes2021NA=TowTimes2021[which(TowTimes2021$Ground == NA), ]

sum(35+57+2)


TowTimes2021$Year = as.factor(TowTimes2021$Year)
TowTimes2021 <- TowTimes2021[order(TowTimes2021$Year), ]

TowTimes2021$Year <- factor(TowTimes2021$Year, levels = TowTimes2021$Year)

ggplot(data = TowTimes2021, aes(Year, AvgTowDepth, colour = Year)) + geom_jitter(size=3)+scale_y_continuous(breaks = seq(0, 30, by = 5))+ labs(y = "Average Tow Depth (m)", x = "Year")

quantile(TowTimes2021$AvgTowDepth, probs = c(0, 0.05, 0.25, 0.50, 0.90, 0.95, 1), na.rm=TRUE)

head(TowTimes2021)






## Map all tows

# Map data
# Need to update the markdown file with this mapping code-
can <- gadm(country = "CAN",
            level   = 1,
            path    = "geodata_default_path",
            version = "latest",
            resolution = 1)

NBNS <- can[can$NAME_1 %in% c("New Brunswick", "Nova Scotia"), ]

NBNS_sf <- st_as_sf(NBNS)
####
CP <- ext(-66.5, -62, 44.5, 45.5)
CP <- as.polygons(CP)
crs(CP) <- crs(NBNS)
CP_sf <- sf::st_as_sf(CP)




# Grounds Data for Polygons
polysT = read_csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Main Data/timGrounds.csv")
grounds <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/timGrounds.csv")



## need to add boxes for the spawning grounds
boxes <- read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/surveyBoxes.csv")

GermanCatchBox =boxes[which(boxes$Box == "GBCatchBox"), ]

df <- tribble(
  ~id, ~Box, ~Y, ~X,
  41, "GBCatchBox", 43.75, -66.8330,
  42, "GBCatchBox", 43.75, -66.0833,
  43, "GBCatchBox", 43.0, -67.0833,
  44, "GBCatchBox", 43.0, -67.8330,
  45, "GBCatchBox", 43.75, -66.8330,
     )

# Convert to polygon
poly_coords <- df %>%
  arrange(id) %>%
  select(X, Y) %>%
  as.matrix()

GBCatch_sf <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "GBCatchBox", geometry = _)


Scots=grounds[which(grounds$Box == "Scots Bay"), ]
German =grounds[which(grounds$Box == "German Bank"), ]

## Scots Bay

df <- tribble(
  ~id, ~Box, ~Y, ~X,
  46, "Scots Bay", 45.500, -66.000,
  47, "Scots Bay", 45.000, -66.000,
  48, "Scots Bay", 45.000, -65.833,
  49, "Scots Bay", 44.833, -65.833,
  50, "Scots Bay", 44.833, -66.000,
  51, "Scots Bay", 44.667, -66.000,
  52, "Scots Bay", 44.667, -65.833,
  53, "Scots Bay", 44.500, -65.833,
  54, "Scots Bay", 45.000, -64.000,
  55, "Scots Bay", 45.500, -64.000,
  56, "Scots Bay", 45.500, -66.000
)


# change a little:

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

# 35 , "German Bank",43.7, -66.229,


# Convert to polygon
poly_coords <- df %>%
  arrange(id) %>%
  select(X, Y) %>%
  as.matrix()

GermanBank_sf <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "German Bank", geometry = _)






df <- tribble(
  ~id, ~Box, ~Y, ~X,
  53, "Scots Bay", 44.7500, -65.61,
  54, "Scots Bay", 45.000, -64.67,
  55, "Scots Bay", 45.500, -64.67,
  56, "Scots Bay", 45.5, -65.61
)

sf_points <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)

# Ensure points are in the correct order
poly_coords <- sf_points %>%
  arrange(id) %>%              # or whatever ordering defines the boundary
  st_coordinates() %>%
  as.matrix()

# Close the polygon by repeating the first coordinate
poly_coords <- rbind(poly_coords, poly_coords[1, ])

# Build polygon
sf_poly <- st_polygon(list(poly_coords)) |> 
  st_sfc(crs = 4326) |> 
  st_sf(Box = unique(sf_points$Box), geometry = _)

ggplot() +
  geom_sf(data = GermanBank_sf, fill = "lightblue", color = "black")+
  geom_sf(data = sf_poly, fill = "lightblue", color = "black")+
  
   geom_sf(data = NBNS_sf, fill = "lightgray", color = "black") + 
  
  geom_sf(data = polySB_main_sf, fill = "lightgreen", color = "black")+
 
  geom_point(data = Larval, aes(x = X, y = Y, fill = Ground),
             pch = 21, alpha = 0.6, size = 2) +
  geom_point(data = CTD, aes(x=Lon, y=Lat, fill=Ground), pch=21, alpha=0.6, size=2)+
  coord_sf(xlim = c(-67, -63.5), ylim = c(43, 45.5)) 

# geom_sf(data = polyGB_sf, fill = "lightgreen", color = "black")+
#geom_sf(data = polySI_sf, fill = "lightgreen", color = "black")+










df <- tribble(
  ~id, ~Box, ~Y, ~X,
  34, "German Bank", 43.5, -66.167,
  35, "German Bank", 43.0, -66.167,
  36, "German Bank", 43.0, -67.833,
  37, "German Bank", 43.5, -67.833,
  38, "German Bank", 43.5, -66.167   # closes the polygon
)

# Convert to polygon
poly_coords <- df %>%
  arrange(id) %>%
  select(X, Y) %>%
  as.matrix()

GermanBank_sf <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "German Bank", geometry = _)





ggplot() +
  geom_sf(data = NBNS_sf, fill = "lightgray", color = "black")+ 
  geom_sf(data = sf_poly, fill = "lightblue", color = "black")+
geom_sf(data = GBCatch_sf, fill = "lightgreen", color = "black") + geom_point(data = Larval,aes(x = X, y = Y, fill = Ground),pch = 21, alpha = 0.6, size = 3) +
  coord_sf(xlim = c(-67, -64.5), ylim = c(43, 45.5)) +
  geom_text(data = labels_df,
            aes(x = x, y = y, label = label),
            size = 5) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 



# Survey Boxes with larval tows points:


CTD = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/CTD 30m.csv"))


SUA = read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/polygon_SB.csv")
polySB_main = as.PolySet(SUA, projection="LL")
polySB_main_sf <- polySB_main %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(NBNS_sf)) %>%
  group_by(PID) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

SUA = read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/polygon_GB.csv")
polyGB = as.PolySet(SUA, projection="LL")
polyGB_sf <- polyGB %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(NBNS_sf)) %>%
  group_by(PID) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

SUA = read.csv("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Box Coordinates/polygon_SI.csv")
polySI = as.PolySet(SUA, projection="LL")
polySI_sf <- polySI %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(NBNS_sf)) %>%
  group_by(PID) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")


# plottext

labels_df <- data.frame(
  x = c(-66.5, -65.5),
  y = c(45.5, 44.5),
  label = c("New Brunswick", "Nova Scotia")
)



ggplot(NBNS_sf) +
  geom_sf(data = NBNS_sf, fill = "lightgray", color = "black") +
  geom_sf(data = sf_poly, fill = "lightblue", color = "black")+
  geom_sf(data = polySI_sf, fill = "lightblue", color = "black") +geom_sf(data = polyGB_sf, fill = "LightSalmon", color = "black")+ geom_sf(data = polySB_main_sf, fill = "lightgreen", color = "black")+
  geom_point(data = Larval, aes(x = X, y = Y, fill = Ground),
             pch = 21, alpha = 0.6, size = 3) +
  geom_point(data = CTD, aes(x=Lon, y=Lat, fill=Ground), pch=21, alpha=0.6, size=3)+
  coord_sf(xlim = c(-67, -64.5), ylim = c(43, 45.5)) +
  geom_text(data = labels_df, aes(x = x, y = y, label = label), size = 5) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank()) + ggtitle("Figure 1. Spawning Ground Locations")

head(CTD)


# Plot the at depth temperatures used to help inform larval growth rate/incubation period



### CTD data that corresponds to larval data
# only look at years that correspond with the current larval data

head(CTD)
years = c('2017', '2018', '2019', '2020', '2021')
CTD_ = CTD[which((CTD$Year %in% years)), ]
unique(CTD_$Year)
head(CTD_)

CTD_=CTD

CTD_$Year = as.factor(CTD_$Year)

ggplot(data = CTD_, aes(Julian, Temperature,  colour = Ground )) +geom_point(size = 2)+scale_y_continuous(breaks = seq(0, 14, by = 1))+ labs(y = "At Depth Temperature (30m) Degrees Celcius", x = "Julian Day")



