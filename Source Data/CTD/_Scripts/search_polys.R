









### TIGHTEN boxes then apply to code at the beginning:

german_points <- bind_rows(
  lurcher_df %>% select(longitude, latitude),
  hsc_german %>% select(longitude, latitude)
)

pad <- 0.05

german_xmin <- min(german_points$longitude, na.rm = TRUE) - pad
german_xmax <- max(german_points$longitude, na.rm = TRUE) + pad
german_ymin <- min(german_points$latitude, na.rm = TRUE) - pad
german_ymax <- max(german_points$latitude, na.rm = TRUE) + pad

german_box <- matrix(
  c(
    german_xmin, german_ymax,
    german_xmax, german_ymax,
    german_xmax, german_ymin,
    german_xmin, german_ymin,
    german_xmin, german_ymax
  ),
  ncol = 2,
  byrow = TRUE
)

germanseal_poly <- st_polygon(list(german_box)) |>
  st_sfc(crs = 4326)


### scots:
SB_xmin <- min(scots_df$longitude, na.rm = TRUE)-pad
SB_xmax <- max(scots_df$longitude, na.rm = TRUE)+pad
SB_ymin <- min(scots_df$latitude, na.rm = TRUE)-pad
SB_ymax <- max(scots_df$latitude, na.rm = TRUE)+pad

SB_box <- matrix(
  c(
    SB_xmin, SB_ymax,
    SB_xmax, SB_ymax,
    SB_xmax, SB_ymin,
    SB_xmin, SB_ymin,
    SB_xmin, SB_ymax
  ),
  ncol = 2,
  byrow = TRUE
)

SB_poly <- st_polygon(list(SB_box)) |>
  st_sfc(crs = 4326)

search_polys <- st_sf(
  area_poly = c("German Bank", "Scots Bay"),
  geometry = c(germanseal_poly, SB_poly)
)

saveRDS(search_polys, "search_polys.rds")



library(sf)
library(dplyr)
library(units)

search_polys <- search_polys %>%
  mutate(
    area_km2 = set_units(st_area(geometry), km^2)
  )

search_polys



###################################### previously manually determined and larger:


# Polygon coordinates (longitude, latitude)
# German/Seal
polygon_coords <- matrix(
  c(
    -66.229, 43.70000,
    -66.229, 43.56667,
    -66.075, 43.56667,
    -66.075, 43.23300,
    -66.550, 43.23300,
    -66.550, 43.70000,
    -66.229, 43.70000
  ),
  ncol = 2,
  byrow = TRUE
)

germanseal_poly <- st_polygon(list(polygon_coords)) |>
  st_sfc(crs = 4326)

# Scots Bay
SB_box <- matrix(
  c(
    -65.61, 44.7500,
    -64.67, 45.000,
    -64.67,45.500,
    -65.61, 45.5,
    -65.61, 44.7500
  ),
  ncol = 2,
  byrow = TRUE
)

SB_poly<- st_polygon(list(SB_box)) |>
  st_sfc(crs = 4326)


# Combine into 1:
search_polys <- st_sf(
  area_poly = c("German Bank", "Scots Bay"),
  geometry = c(germanseal_poly, SB_poly)
)