


# load Oceans dataframe from CTD2026.R
# manually change from salinity to temperature with # out


# casts with only 1 measurement are showing up as blank in the plot


# Remove casts that do not reach at least 1m Depth
# might only want to do this for HSC data casts?
valid_casts <- Oceans_filtered %>%
  group_by(id) %>%
  summarise(max_depth = max(Depth, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(max_depth), max_depth >= 1) %>%
  pull(id)

out_dir <- "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/Casts/LateSummer_Fall"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

failed_casts <- c()



for (cast in valid_casts) {
  
  
  tryCatch({
  
  
  cast_data <- Oceans %>% 
    filter(id == cast) %>% 
    arrange(Depth)   # <-- replace with your actual Depth column name
  
  
  # Skip empty casts
  if (nrow(cast_data) == 0) stop("No rows for this cast")
  
  
  
  
  # Extract unique location

  loc <- cast_data %>%
    summarise(
      Lat = first(na.omit(Lat)),
      Lon = first(na.omit(Lon))
    )
  
  
  if (nrow(loc) == 0 || is.na(loc$Lat) || is.na(loc$Lon)) {
    stop("Missing Lat/Lon")
  }
  
  # Safe file name
  safe_cast <- gsub("[^A-Za-z0-9_\\-]", "_", cast)
  
  
  
  # -------------------------
  # 1. MAP PANEL
  # -------------------------
  p_map <- ggplot() +
    borders("world", colour = "grey70", fill = "grey90") +
    geom_point(data = loc, aes(x = Lon, y = Lat),
               color = "red", size = 3) +
    coord_quickmap(
      xlim = c(loc$Lon - 0.5, loc$Lon + 0.5),
      ylim = c(loc$Lat - 0.5, loc$Lat + 0.5)
    ) +
    labs(title = paste("Cast Location -", cast),
         x = "Longitude", y = "Latitude") +
    theme_bw()
  
  # -------------------------
  # 2. PROFILE PANEL
  # Choose ONE: Temperature or Salinity
  # -------------------------
  
  # Temperature profile
  p_Tprofile <- ggplot(cast_data, aes(x = Temperature, y = Depth)) +
    geom_path(color = "red", linewidth = 1) +
    geom_hline(yintercept = 5, linetype = "dashed", colour = "black", linewidth = 0.8) +
    scale_y_reverse() +
    labs(
      title = paste("Temperature Profile -", cast),
      x = "Temperature (°C)", y = "Depth (m)"
    ) +
    theme_bw()
  
  
  
  
  # Salinity
  
  p_Sprofile <- ggplot(cast_data, aes(x = Salinity, y = Depth)) +
    geom_path(color = "blue", linewidth = 1) +
    geom_hline(yintercept = 5, linetype = "dashed", colour = "black", linewidth = 0.8) +
    scale_y_reverse() +
    labs(
      title = paste("Salinity Profile -", cast),
      x = "Salinity (PSU)", y = "Depth (m)"
    ) +
    theme_bw()
  
  
  #Density
  
  p_Dprofile <- ggplot(cast_data, aes(x = Density, y = Depth)) +
    geom_path(color = "green", linewidth = 1) +
    geom_hline(yintercept = 5, linetype = "dashed", colour = "black", linewidth = 0.8) +
    scale_y_reverse() +
    labs(
      title = paste("Density Profile -", cast),
      x = "Density (kg m-3)", y = "Depth (m)"
    ) +
    theme_bw()
  
  
  
  
  # -------------------------
  # 3. COMBINE PANELS
  # -------------------------
  combined <- p_map + p_Tprofile + p_Sprofile + p_Dprofile + plot_layout(widths = c(1, 1.2))
  
  # -------------------------
  # 4. SAVE OUTPUT
  # -------------------------
  ggsave(
    filename = file.path(out_dir, paste0("Cast_", cast, "_Map_Profile.png")),
    plot = combined,
    width = 12, height = 6, dpi = 300
  )
  
  }, error = function(e) {
    message("Failed cast: ", cast, " | ", e$message)
    failed_casts <<- c(failed_casts, cast)
  })
}


