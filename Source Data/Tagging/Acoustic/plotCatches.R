

library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
install.packages("ggspatial")

library(ggspatial)
library(lubridate)

setwd("C:/Users/herri/OneDrive/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Acoustic")

# Read data
df <- read.csv("offshoreCatches_Comeaus.csv")

str(df)

df$Date = as.Date(df$Date)
df$Year = year(df$Date)
# Your fishing data
df_map <- df %>%
  filter(
    !is.na(Lat),
    !is.na(Lon),
    Lat != 0,
    Lon != 0
  )

points_sf <- st_as_sf(
  df_map,
  coords = c("Lon", "Lat"),
  crs = 4326,
  remove = FALSE
)

# Land
land <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Official offshore wind areas
wind <- st_read("Designated_WEAs_25_07_29.shp")

# Make CRS consistent
wind <- st_transform(wind, 4326)

# Look at the attribute names
names(wind)
print(wind)

# Filter areas which have been chosen
middle_bank <- wind %>%
  filter(WEA == "Middle Bank")
sydney <- wind %>%
  filter(WEA == "Sydney Bight")

# Create Plot

ggplot() +
  
  # Ocean/land
  geom_sf(
    data = land,
    fill = "grey90",
    color = "grey40",
    linewidth = 0.3
  ) +
  
  # all offshore wind area
  geom_sf(
    data = wind,
    fill = "gold",
    color = "darkorange",
    alpha = 0.30,
    linewidth = 1
  ) +
  
  # Middle Bank offshore wind area
  geom_sf(
    data = middle_bank,
    fill = "green",
    color = "forestgreen",
    alpha = 0.30,
    linewidth = 1
  ) +
  
  # Sydney Bight offshore wind area
  geom_sf(
    data = sydney,
    fill = "green",
    color = "forestgreen",
    alpha = 0.30,
    linewidth = 1
  ) +
  
  # Fishing locations
  geom_sf(
    data = points_sf,
    color = "purple",
    size = 1.5,
    alpha = 0.8
  ) +
  
  # Zoom to your fishing area
  coord_sf(
    xlim = c(-65, -59),
    ylim = c(42, 48),
    expand = FALSE
  ) +
  
  theme_bw() +
  
  theme(
    panel.background = element_rect(fill = "lightblue")
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    unit_category = "metric"
  ) +
  
  labs(
    title = "Fishing Locations and Wind Energy Areas",
    subtitle = "Nova Scotia Offshore",
    x = "Longitude",
    y = "Latitude"
  )







################## Look at dates

library(dplyr)
library(ggplot2)
library(lubridate)

df2 <- df %>%
  mutate(
    Date = as.Date(Date),
    Month = month(Date),
    Month_name = month(Date, label = TRUE, abbr = TRUE)
  )

monthly <- df2 %>%
  count(Year, Month, Month_name)

ggplot(monthly,
       aes(x = Month,
           y = n,
           color = factor(Year),
           group = Year)) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    x = "Month",
    y = "Number of catch records",
    color = "Year",
    title = "Seasonal Distribution of Catch Records"
  ) +
  theme_bw()



################## Heat map:

ggplot(monthly,
       aes(x = Month,
           y = factor(Year),
           fill = n)) +
  geom_tile(color = "white") +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_fill_viridis_c(
    name = "Catch\nrecords"
  ) +
  labs(
    x = "Month",
    y = "Year",
    title = "Temporal Distribution of Catch Records"
  ) +
  theme_bw()

