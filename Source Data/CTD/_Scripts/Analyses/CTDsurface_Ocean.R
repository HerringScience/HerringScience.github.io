

CTDsurface_Ocean <- function(Ocean, events = NULL, surface_depth = 1) {
  
  # ---- FORCE NUMERIC TYPES (EXACT FIX) ----
  Ocean <- Ocean %>%
    dplyr::mutate(
      Depth       = as.numeric(Depth),
      Temperature = as.numeric(Temperature),
      Salinity    = as.numeric(Salinity),
      Density     = as.numeric(Density),
      ground = as.factor(ground),
      Source = as.factor(Source)
    )
  
  
  
  # Safety checks
  required_cols <- c("id", "Depth", "Temperature", "Salinity", "Density", "ground", "Source", "Year", "Date", "Year")
  missing_cols <- setdiff(required_cols, names(Ocean))
  if (length(missing_cols) > 0) {
    stop("Ocean is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # Surface data (≤ surface_depth m)
  SST <- Ocean %>%
    dplyr::filter(Depth <= surface_depth)
  
  # Summary by cast (id)
  surface_summary <- SST %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      depth1      = max(Depth, na.rm = TRUE),
      avgTemp1    = mean(Temperature, na.rm = TRUE),
      avgSal1     = mean(Salinity, na.rm = TRUE),
      seTemp1     = se(Temperature),
      seSal1      = se(Salinity),
      density1    = mean(Density, na.rm = TRUE),
      seDensity1  = se(Density),
      ground = ground,
      Source = Source,
      Date = Date,
      Year=Year,
      .groups = "drop"
    )
  # Optional merge with events
  if (!is.null(events)) {
    surface_summary <- dplyr::left_join(surface_summary, events, by = "id")
  }
  
  return(surface_summary)
}
