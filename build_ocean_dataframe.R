


build_ocean_dataframe <- function(
    ctd_path,
    dfo_paths,                      # <- can be a single path OR a vector of paths
    crs = 4326,
    predicate = "intersects",
    keep_source = TRUE,             # <- add source filename column to DFO
    ground_labels = c(ScotsBay = "Ground1", GermanBank = "Ground2"),  # <- customizable labels
    other_label = "Other"
) {
  
  suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(sf)
    library(oce)
    library(gsw)
  })
  
  # ---- Helper: robust CSV reader for DFO files ----
  
  read_dfo_one <- function(path) {
    x <- readr::read_csv(path, show_col_types = FALSE)
    
    # Only coerce if the column exists
    if ("stn_id" %in% names(x)) {
      x$stn_id <- as.character(x$stn_id)
    }
    
    if (keep_source) x$.source_file <- basename(path)
    x
  }
  
  # ---- Load CTD ----
  CTD <- readr::read_csv(ctd_path, show_col_types = FALSE)
  if ("stn_id" %in% names(CTD)) {
    CTD$stn_id <- as.character(CTD$stn_id)
  } else {
    CTD$stn_id <- NA_character_
  }
  
  
  # ---- Load 1+ DFO files and bind them ----
  if (length(dfo_paths) < 1) stop("dfo_paths must contain at least one file path.")
  DFO_raw <- purrr::map_dfr(dfo_paths, read_dfo_one)
  
  # ---- DFO formatting ----
  DFO <- DFO_raw %>%
    mutate(
      Lat = as.numeric(latitude),
      Lon = as.numeric(longitude),
      time = as.POSIXct(time),
      Date = as.Date(paste(year, month, day, sep = "-")),
      datatype = as.factor(datatype),
      cruise_id = as.factor(cruise_id),
      
      Depth = swDepth(pressure, Lat),
      
      Temperature = temperature,
      Salinity = salinity,
      Pressure = pressure,
      
      id = paste(cruise_id, stn_id, sep = "-"),
      Year = year(Date)
    ) %>%
    dplyr::select(-latitude, -longitude, -temperature, -salinity,
           -pressure, -year, -month, -day)
  
  DFO <- DFO %>%
    mutate(stn_id = as.character(stn_id))
  # ---- Derived physical variables (matches your intent) ----
  DFO$Sound_velocity <- swSoundSpeed(
    salinity = DFO$Salinity,
    temperature = DFO$Temperature,
    pressure = DFO$Pressure
  )
  
  # NOTE: gsw_rho expects SA/CT ideally; keeping your existing usage for continuity.
  DFO$Density <- gsw_rho(
    DFO$Salinity,
    DFO$Temperature,
    DFO$Pressure
  )
  
  # Conductivity in µS/cm (aligns with typical CTD outputs)
  DFO$Conductivity <- gsw_C_from_SP(DFO$Salinity, DFO$Temperature, DFO$Pressure) * 1000
  
  # Keep your existing column name too (even though it's the same quantity here)
  DFO$Specific_conductance <- DFO$Conductivity
  
  # ---- Build polygons ----
  # Scots Bay polygon
  SB_coords <- matrix(
    c(-65.61, 44.75,
      -64.67, 45.00,
      -64.67, 45.50,
      -65.61, 45.50,
      -65.61, 44.75),
    ncol = 2, byrow = TRUE
  )
  SB_poly <- st_sf(
    Box = "Scots Bay",
    geometry = st_sfc(st_polygon(list(SB_coords)), crs = crs)
  )
  
  # German Bank polygon
  GB_coords <- matrix(
    c(-66.229, 43.7,
      -66.229, 43.56667,
      -66.075, 43.56667,
      -66.075, 43.233,
      -66.55,  43.233,
      -66.55,  43.7,
      -66.229, 43.7),
    ncol = 2, byrow = TRUE
  )
  GermanBank_sf <- st_sf(
    Box = "German Bank",
    geometry = st_sfc(st_polygon(list(GB_coords)), crs = crs)
  )
  
  # ---- Ground labeling ----
  pts <- st_as_sf(DFO, coords = c("Lon", "Lat"), crs = crs, remove = FALSE)
  
  # Ensure CRS match (defensive)
  if (st_crs(SB_poly) != st_crs(pts)) SB_poly <- st_transform(SB_poly, st_crs(pts))
  if (st_crs(GermanBank_sf) != st_crs(pts)) GermanBank_sf <- st_transform(GermanBank_sf, st_crs(pts))
  
  pred_fun <- if (predicate == "within") st_within else st_intersects
  
  in_SB <- lengths(pred_fun(pts, SB_poly)) > 0
  in_GB <- lengths(pred_fun(pts, GermanBank_sf)) > 0
  
  # Priority: Scots Bay (Ground1) wins if overlap
  DFO$ground <- ifelse(
    in_SB, ground_labels[["ScotsBay"]],
    ifelse(in_GB, ground_labels[["GermanBank"]], other_label)
  )
  DFO$ground <- factor(DFO$ground)
  
  # ---- Final DFO cleanup to match your pipeline ----
  # Keep .source_file if enabled; remove flag if present
  if ("flag" %in% names(DFO)) DFO$flag <- NULL
  
  DFO <- DFO %>%
    mutate(
      plankton_ID = NA,
      Survey = cruise_id
    ) %>%
    dplyr::select(-cruise_id)
  
  # ---- CTD formatting ----
  CTD <- CTD %>%
    mutate(
      cruise_time = as.POSIXct(NA),
      datatype    = "CD",
      stn_id      = if ("stn_id" %in% names(.)) stn_id else NA_character_,
      time        = as.POSIXct(NA)
    )
  
  # Add maximum depth per id
  max_depth <- aggregate(Depth ~ id, data = CTD, max, na.rm = TRUE)
  colnames(max_depth) <- c("id", "maximum_depth")
  CTD <- merge(CTD, max_depth, by = "id", all.x = TRUE)
  
  # ---- Combine (ROBUST FIX) ----
  # ---- FINAL TYPE HARMONIZATION (DO NOT SKIP) ----
  if (!"stn_id" %in% names(CTD)) CTD$stn_id <- NA_character_
  if (!"stn_id" %in% names(DFO)) DFO$stn_id <- NA_character_
  
  CTD$stn_id <- as.character(CTD$stn_id)
  DFO$stn_id <- as.character(DFO$stn_id)
  
  # ---- FINAL TYPE HARMONIZATION (EXACT FIX) ----
  
  # Ensure Survey exists in both
  if (!"Survey" %in% names(CTD)) CTD$Survey <- NA_character_
  if (!"Survey" %in% names(DFO)) DFO$Survey <- NA_character_
  
  # Force identical type
  CTD$Survey <- as.factor(CTD$Survey)
  DFO$Survey <- as.factor(DFO$Survey)
  
  # (keep stn_id harmonization too)
  if (!"stn_id" %in% names(CTD)) CTD$stn_id <- NA_character_
  if (!"stn_id" %in% names(DFO)) DFO$stn_id <- NA_character_
  
  CTD$stn_id <- as.factor(CTD$stn_id)
  DFO$stn_id <- as.factor(DFO$stn_id)
  
  common <- intersect(names(CTD), names(DFO))
  CTD[common] <- lapply(CTD[common], as.character)
  DFO[common] <- lapply(DFO[common], as.character)
  
  CTD$Source = "HSC"
  DFO$Source = "DFO"
  
  Ocean <- dplyr::bind_rows(CTD, DFO)
  
  # format cols
  
  Ocean$Lat = as.numeric(Ocean$Lat)
  Ocean$Lon = as.numeric(Ocean$Lon)
  Ocean$ground= as.factor(Ocean$ground)
  
  Ocean <- Ocean %>%
    dplyr::filter(ground != "Other")
  

  
  return(Ocean)
}