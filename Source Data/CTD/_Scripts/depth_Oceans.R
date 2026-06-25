

depth_Oceans <- function(x, events = NULL,
                         depth_lower = 0,
                         depth_upper = 1) {
  
  # ---- FORCE NUMERIC TYPES ----
  x <- x %>%
    dplyr::mutate(
      Depth       = as.numeric(Depth),
      Temperature = as.numeric(Temperature),
      Salinity    = as.numeric(Salinity),
      Density     = as.numeric(Density),
      ground      = as.factor(ground),
      JulianDay   = as.numeric(JulianDay),
      Source      = as.factor(Source)
    )
  
  # Safety checks
  required_cols <- c("id", "Depth", "Temperature", "Salinity",
                     "Density", "ground", "Source",
                     "Year", "Date", "JulianDay")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop("Ocean is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # ---- DEPTH RANGE FILTER ----
  depth_data <- x %>%
    dplyr::filter(
      Depth >= depth_lower,
      Depth <= depth_upper
    )
  
  # ---- SUMMARY BY CAST ----
  depth_summary <- depth_data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      depth_min  = min(Depth, na.rm = TRUE),
      depth_max  = max(Depth, na.rm = TRUE),
      
      avgTemp    = mean(Temperature, na.rm = TRUE),
      avgSal     = mean(Salinity, na.rm = TRUE),
      avgDensity = mean(Density, na.rm = TRUE),
      
      seTemp     = se(Temperature),
      seSal      = se(Salinity),
      seDensity  = se(Density),
      
      ground     = dplyr::first(ground),
      Source     = dplyr::first(Source),
      Date       = dplyr::first(Date),
      Year       = dplyr::first(Year),
      JulianDay  = dplyr::first(JulianDay),
      Lat = dplyr::first(Lat),
      Lon = dplyr::first(Lon),
      
      .groups = "drop"
    )
  
  # Optional merge with events
  if (!is.null(events)) {
    depth_summary <- dplyr::left_join(depth_summary, events, by = "id")
  }
  
  return(depth_summary)
}
