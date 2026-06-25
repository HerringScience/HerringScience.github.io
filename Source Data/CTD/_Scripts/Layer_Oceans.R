
Layer_Oceans <- function(x,
                         events = NULL,
                         mode = c("surface", "range"),
                         surface_depth = 5,
                         depth_lower = 0,
                         depth_upper = 1,
                         prefix = NULL,
                         # --- NEW: bin / inference controls ---
                         add_JD_bin = TRUE,
                         drop_outside_bins = FALSE,
                         min_n = 5,
                         return = c("summary", "both", "usable_bins"),
                         join_usable_flag = TRUE) {
  
  mode   <- match.arg(mode)
  return <- match.arg(return)
  
  # ---- FORCE NUMERIC / FACTOR TYPES ----
  x <- x %>%
    dplyr::mutate(
      Depth       = as.numeric(Depth),
      Temperature = as.numeric(Temperature),
      Salinity    = as.numeric(Salinity),
      Density     = as.numeric(Density),
      ground      = as.factor(ground),
      JulianDay   = as.numeric(JulianDay),
      Source      = as.factor(Source),
      Sound_velocity = as.numeric(Sound_velocity)
    )
  
  # ---- SAFETY CHECKS ----
  required_cols <- c("id", "Depth", "Temperature", "Salinity",
                     "Density", "ground", "Source",
                     "Year", "Date", "JulianDay", "Lat", "Lon", "Sound_velocity")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop("Ocean is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # ---- DEPTH FILTER ----
  filtered <- if (mode == "surface") {
    x %>% dplyr::filter(Depth <= surface_depth)
  } else {
    x %>% dplyr::filter(Depth >= depth_lower, Depth <= depth_upper)
  }
  
  # ---- SUMMARY BY CAST (id) ----
  out <- filtered %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      depth_min  = min(Depth, na.rm = TRUE),
      depth_max  = max(Depth, na.rm = TRUE),
      
      avgTemp    = mean(Temperature, na.rm = TRUE),
      avgSal     = mean(Salinity, na.rm = TRUE),
      avgDensity = mean(Density, na.rm = TRUE),
      avgSoundSpeed = mean(Sound_velocity, na.rm = TRUE),
      
      seTemp     = se(Temperature),
      seSal      = se(Salinity),
      seDensity  = se(Density),
      
      ground     = dplyr::first(ground),
      Source     = dplyr::first(Source),
      Date       = dplyr::first(Date),
      Year       = dplyr::first(Year),
      JulianDay  = dplyr::first(JulianDay),
      Lat        = dplyr::first(Lat),
      Lon        = dplyr::first(Lon),
      
      .groups = "drop"
    )
  
  # ---- OPTIONAL MERGE WITH EVENTS ----
  if (!is.null(events)) {
    out <- dplyr::left_join(out, events, by = "id")
  }
  
  # ---- NEW: ADD JD BINS ----
  
  # ---- ADD JD BINS (AND DROP OUTSIDE-BIN CASTS) ----
  if (isTRUE(add_JD_bin)) {
    out <- out %>%
      dplyr::mutate(
        JD_bin = dplyr::case_when(
          JulianDay >= 181 & JulianDay <= 220 ~ "EarlySummer",
          JulianDay >= 221 & JulianDay <= 260 ~ "LateSummer",
          JulianDay >= 261 & JulianDay <= 300 ~ "Fall"
        )
      ) %>%
      # DROP casts outside inference window
      dplyr::filter(!is.na(JD_bin))
  }
  
  
  
  # ---- NEW: BUILD USABLE_BINS TABLE (HSC vs DFO within ground × bin) ----
  usable_bins <- NULL
  if (isTRUE(add_JD_bin)) {
    usable_bins <- out %>%
      dplyr::filter(!is.na(JD_bin)) %>%
      dplyr::count(ground, JD_bin, Source, name = "n") %>%
      tidyr::pivot_wider(names_from = Source, values_from = n, values_fill = 0) %>%
      dplyr::mutate(
        usable_for_inference = (HSC >= min_n & DFO >= min_n)
      ) %>%
      dplyr::arrange(ground, JD_bin)
    
    # Optional: attach the usable flag to each cast row
    if (isTRUE(join_usable_flag)) {
      out <- out %>%
        dplyr::left_join(
          usable_bins %>% dplyr::select(ground, JD_bin, usable_for_inference),
          by = c("ground", "JD_bin")
        )
    }
  }
  
  # ---- OPTIONAL PREFIX TO AVOID NAME COLLISIONS ----
  # (does NOT prefix id or metadata columns; also does not prefix JD_bin / usable flags)
  if (!is.null(prefix) && nzchar(prefix)) {
    keep <- c("id", "ground", "Source", "Date", "Year", "JulianDay", "Lat", "Lon",
              "JD_bin", "usable_for_inference")
    rename_these <- setdiff(names(out), keep)
    
    out <- out %>%
      dplyr::rename_with(~ paste0(prefix, .x), dplyr::all_of(rename_these))
  }
  
  # ---- RETURN CONTROL ----
  if (return == "summary") {
    return(out)
  } else if (return == "usable_bins") {
    return(usable_bins)
  } else {
    return(list(summary = out, usable_bins = usable_bins))
  }
}