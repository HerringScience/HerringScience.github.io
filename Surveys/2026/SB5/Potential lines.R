# Install required packages if you don't have them:
# install.packages(c("leaflet", "dplyr"))

library(leaflet)
library(dplyr)

# 1. Input your exact newest dataset in order
vessel_data <- data.frame(
  Vessel    = c("V1", "v2", "v3", "v4", "v5", "v6", "v5", "v6", "V1", "V2", "V3", "V4", "v1", "v2", "v3", "v4", "v5", "v6"),
  Start.Lat = c(45.03636667, 45.06005758, 45.08374848, 45.10743939, 45.25, 45.26305, 45.15482121, 45.31166667, 45.26375, 45.23927778, 45.25397222, 45.26866667, 45.22489394, 45.24224848, 45.25960303, 45.27695758, 45.1311303, 45.14297576),
  Start.Lon = c(-65.23333333, -65.23333333, -65.23333333, -65.23333333, -65.05, -65.05, -65.23333333, -64.825, -64.55, -64.71327778, -64.73522222, -64.75716667, -64.69415152, -64.72032121, -64.74649091, -64.77266061, -65.23333333, -65.23333333),
  End.Lat   = c(45.21621667, 45.23357121, 45.25092576, 45.2682803, 45.31666667, 45.32445, 45.30298939, 45.16666667, 45.22458333, 45.28527778, 45.30680556, 45.32833333, 45.04821212, 45.07190303, 45.09559394, 45.11928485, 45.28563485, 45.29431212),
  End.Lon   = c(-64.68106667, -64.70723636, -64.73340606, -64.75957576, -64.83333333, -64.85, -64.81191515, -65.23333333, -64.69133333, -64.55, -64.55, -64.55, -65.23333333, -65.23333333, -65.23333333, -65.23333333, -64.78574545, -64.7988303),
  stringsAsFactors = FALSE
)

# 2. Convert vessel labels to uppercase and assign chronological line IDs
vessel_data <- vessel_data %>%
  mutate(
    Vessel = toupper(Vessel),
    Line_ID = row_number()
  )

# 3. Define the distinct color palette for the fleet
vessel_colors <- c(
  "V1" = "#E41A1C", # Red
  "V2" = "#377EB8", # Blue
  "V3" = "#4DAF4A", # Green
  "V4" = "#FF7F00", # Orange
  "V5" = "#984EA3", # Purple
  "V6" = "#A65628"  # Brown
)

# 4. Initialize the leaflet map canvas (Centered on the Bay of Fundy)
map <- leaflet() %>%
  addTiles() %>%  
  setView(lng = -64.9, lat = 45.18, zoom = 10)

# 5. Iteratively build out the lines and directional points
for (i in 1:nrow(vessel_data)) {
  row <- vessel_data[i, ]
  v_color <- vessel_colors[[row$Vessel]]
  
  map <- map %>%
    # Plot track survey lines
    addPolylines(
      lng = c(row$Start.Lon, row$End.Lon),
      lat = c(row$Start.Lat, row$End.Lat),
      color = v_color,
      weight = 3.5,
      opacity = 0.85,
      label = paste("Vessel:", row$Vessel, "| Line:", row$Line_ID)
    ) %>%
    # Start node (White interior = Direction start)
    addCircleMarkers(
      lng = row$Start.Lon, lat = row$Start.Lat,
      radius = 5, color = v_color, fillColor = "white", fillOpacity = 1, weight = 2,
      popup = paste("<b>Line ", row$Line_ID, " (Start)</b><br>Vessel: ", row$Vessel)
    ) %>%
    # End node (Solid fill color = Direction end)
    addCircleMarkers(
      lng = row$End.Lon, lat = row$End.Lat,
      radius = 5, color = v_color, fillColor = v_color, fillOpacity = 1, weight = 1,
      popup = paste("<b>Line ", row$Line_ID, " (End)</b><br>Vessel: ", row$Vessel)
    )
}

# 6. Add the map's interactive color legend
map <- map %>%
  addLegend(
    position = "topright",
    colors = vessel_colors,
    labels = names(vessel_colors),
    title = "Survey Fleet"
  )

# 7. Print and view map
map


# Install required packages if you don't have them:
# install.packages(c("leaflet", "dplyr"))

library(leaflet)
library(dplyr)

# 1. Input your exact dataset (Line 16/NB2 removed, new vessel mapping)
vessel_data <- data.frame(
  Vessel    = c("v6", "v1", "v6", "v1", "v2", "v1", "v2", "v3", "v2", "v3", "v4", "v3", "v5", "v4", "v4", "v5", "v5"),
  Start.Lat = c(45.03636667, 45.22458333, 45.04821212, 45.06005758, 45.23927778, 45.07190303, 45.08374848, 45.25397222, 45.09559394, 45.10743939, 45.26866667, 45.11928485, 45.25, 45.15482121, 45.1311303, 45.16666667, 45.14297576),
  Start.Lon = c(-65.23333333, -64.69133333, -65.23333333, -65.23333333, -64.71327778, -65.23333333, -65.23333333, -64.73522222, -65.23333333, -65.23333333, -64.75716667, -65.23333333, -65.05, -65.23333333, -65.23333333, -65.23333333, -65.23333333),
  End.Lat   = c(45.21621667, 45.26375, 45.22489394, 45.23357121, 45.28527778, 45.24224848, 45.25092576, 45.30680556, 45.25960303, 45.2682803, 45.32833333, 45.27695758, 45.31666667, 45.30298939, 45.28563485, 45.31166667, 45.29431212),
  End.Lon   = c(-64.68106667, -64.55, -64.69415152, -64.70723636, -64.55, -64.72032121, -64.73340606, -64.55, -64.74649091, -64.75957576, -64.55, -64.77266061, -64.83333333, -64.81191515, -64.78574545, -64.825, -64.7988303),
  stringsAsFactors = FALSE
)

# 2. Standardize vessel labels to uppercase and assign chronological line IDs
vessel_data <- vessel_data %>%
  mutate(
    Vessel = toupper(Vessel),
    Line_ID = row_number()
  )

# 3. Define a distinct color palette for the 6 vessels
vessel_colors <- c(
  "V1" = "#E41A1C", # Red
  "V2" = "#377EB8", # Blue
  "V3" = "#4DAF4A", # Green
  "V4" = "#FF7F00", # Orange
  "V5" = "#984EA3", # Purple
  "V6" = "#A65628"  # Brown
)

# 4. Initialize the leaflet map canvas (Centered near your grid)
map <- leaflet() %>%
  addTiles() %>%  
  setView(lng = -64.9, lat = 45.18, zoom = 10)

# 5. Iteratively build out the lines and directional points
for (i in 1:nrow(vessel_data)) {
  row <- vessel_data[i, ]
  v_color <- vessel_colors[[row$Vessel]]
  
  map <- map %>%
    # Plot active track survey lines
    addPolylines(
      lng = c(row$Start.Lon, row$End.Lon),
      lat = c(row$Start.Lat, row$End.Lat),
      color = v_color,
      weight = 3.5,
      opacity = 0.85,
      label = paste("Vessel:", row$Vessel, "| Sequence Line:", row$Line_ID)
    ) %>%
    # Start node (White interior = Survey line start point)
    addCircleMarkers(
      lng = row$Start.Lon, lat = row$Start.Lat,
      radius = 5, color = v_color, fillColor = "white", fillOpacity = 1, weight = 2,
      popup = paste("<b>Line ", row$Line_ID, " (Start)</b><br>Vessel: ", row$Vessel)
    ) %>%
    # End node (Solid fill color = Survey line endpoint)
    addCircleMarkers(
      lng = row$End.Lon, lat = row$End.Lat,
      radius = 5, color = v_color, fillColor = v_color, fillOpacity = 1, weight = 1,
      popup = paste("<b>Line ", row$Line_ID, " (End)</b><br>Vessel: ", row$Vessel)
    )
}

# 6. Add the map's interactive color legend
map <- map %>%
  addLegend(
    position = "topright",
    colors = vessel_colors,
    labels = names(vessel_colors),
    title = "Survey Fleet Plan"
  )

# 7. Render map
map