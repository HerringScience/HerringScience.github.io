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


### Mapping for CTD data with DFO


# Mapping

one=DFO[which(DFO$stn_id == "001"), ]
one$time

ggplot(one, aes(x = temperature, y = depth)) +
  geom_path(color = "red", linewidth = 1) +
  scale_y_reverse() +
  labs(
    x = "Temperature (°C)",
    y = "Depth (m)",
    title = "CTD Temperature Profile"
  ) +
  theme_classic()



# Plot positions

#load base data
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


# Scots Bay CTD Box
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
SB_poly <- st_polygon(list(poly_coords)) |> 
  st_sfc(crs = 4326) |> 
  st_sf(Box = unique(sf_points$Box), geometry = _)

ScotsBay=SB_poly


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

GermanBank_sf <- st_polygon(list(poly_coords)) |>
  st_sfc(crs = 4326) |>
  st_sf(Box = "German Bank", geometry = _)



# actual map
ggplot(NBNS_sf) +
  geom_sf(data = NBNS_sf, fill = "lightgray", color = "black") +
  geom_sf(data = SB_poly, fill = "lightblue", color = "black")+ geom_sf(data = GermanBank_sf, fill = "purple", color = "black")+ geom_sf(data = surveyGB_poly, fill = "pink", color = "black")+ geom_sf(data = surveySI_poly, fill = "pink", color = "black") + geom_sf(data = surveySB_poly, fill = "navy", color = "black") +
  geom_point(data = CTD, aes(x=Lon, y=Lat), size=2, colour="red")+
  geom_sf(data = DFO)+
  coord_sf(xlim = c(-67, -64.5), ylim = c(43, 45.5)) +
  geom_text(data = labels_df, aes(x = x, y = y, label = label), size = 5) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank()) + ggtitle("Figure 1. Spawning Ground Locations")




# select DFO points only within SB_poly or GermanBank_sf
# Only 2 days October 28 and November 9

dim(DFO)
points <- data.frame(
  id = 1:8352,
  lon = DFO$longitude,
  lat = DFO$latitude
)

points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)

inside <- st_within(points_sf, GermanBank_sf, sparse = FALSE)
points_inside <- points_sf[inside, ]

dim(DFO)
DFO$id = 1:8352

t = unique(points_inside$id)

germanB = DFO[which((DFO$id %in% t)), ] 

#export points_inside
write.table(germanB, file= "2009CTDDFO.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 






## geom_point(data = Larval, aes(x = X, y = Y),pch = 21, alpha = 0.6, size = 3) +

### +
+geom_sf(data = polyGB_sf, fill = "LightSalmon", color = "black")+ geom_sf(data = polySB_main_sf, fill = "lightgreen", color = "black")