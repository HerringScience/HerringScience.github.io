

build_Oceans_df <- function (
    ctd_path,
    dfo_paths,  crs = 4326,
    predicate = "intersects",
    keep_source = TRUE)     
      

{
 
   read_dfo_one <- function(path) {
    x <- readr::read_csv(path, show_col_types = FALSE)
    
    # Only coerce if the column exists
    if ("stn_id" %in% names(x)) {
      x$stn_id <- as.character(x$stn_id)
    }
    
    if (keep_source) x$.source_file <- basename(path)
    x
  }
  
  
  
  # Load HSC data:
  CTD <- readr::read_csv(ctd_path, show_col_types = FALSE)
  
  # ---- Load 1+ DFO files and bind them ----
  if (length(dfo_paths) < 1) stop("dfo_paths must contain at least one file path.")
  DFO <- purrr::map_dfr(dfo_paths, read_dfo_one)
  
  
  # ---- DFO formatting ----
  
  DFO$latitude = as.numeric(DFO$latitude)
  DFO$longitude = as.numeric(DFO$longitude)
  DFO$Lat = DFO$latitude
  DFO$Lat = as.numeric(DFO$Lat)
  DFO$Lon = DFO$longitude
  DFO$Lon = as.numeric(DFO$Lon)
  DFO$latitude = NULL
  DFO$longitude = NULL
  DFO$time = as.POSIXct(DFO$time)
  DFO$Date <- paste(DFO$year, DFO$month, DFO$day, sep = "-")
  DFO$year = NULL
  DFO$month = NULL
  DFO$day = NULL
  DFO$Date = as.Date(DFO$Date)
  DFO$datatype = as.factor(DFO$datatype)
  DFO$cruise_id = as.factor(DFO$cruise_id)
  DFO$Depth = swDepth(DFO$pressure, DFO$Lat)
  DFO$Temperature = DFO$temperature
  DFO$temperature = NULL
  DFO$Salinity = DFO$salinity
  DFO$salinity = NULL
  DFO$Pressure = DFO$pressure
  DFO$pressure = NULL
  
  electrical_conductivity <- function(T, S, depth, lat) {
    # depth: meters
    # latitude: degrees (needed for pressure conversion)
    # Step 1: depth → pressure (dbar)
    P <- swPressure(depth, lat)
    # Step 2: conductivity ratio (dimensionless)
    R <- swCSTp(S, T, P)
    # Step 3: convert ratio → electrical conductivity (mS/cm)
    # Standard seawater conductivity = 42.914 mS/cm
    C_mScm <- R * 42.914
    # Return conductivity in µS/cm (most CTDs use this)
    C_uScm <- C_mScm * 1000
    return(C_uScm)
  }

  
  DFO$Conductivity <- electrical_conductivity(DFO$Temperature,DFO$Salinity, DFO$Depth, DFO$Lat)
  
  DFO$id = paste(DFO$cruise_id, DFO$stn_id, sep = "-")
  DFO$Year <- lubridate::year(DFO$Date)
  DFO$Sound_velocity = swSoundSpeed(salinity=DFO$Salinity, temperature = DFO$Temperature, pressure = DFO$Pressure)
  DFO$Density= gsw_rho(DFO$Salinity, DFO$Temperature, DFO$Pressure)
  DFO$Specific_conductance = gsw_C_from_SP(DFO$Salinity, DFO$Temperature, DFO$Pressure)*1000
  # Returns conductivity [mS/cm]
  
  
  # Load polygons for Scots Bay and German Bank
  
  library(dplyr)
  library(sf)
  library(tibble)
  
  # Polygon (lon/lat) from the vertices
  
  #Scots Bay
        df <- tribble(
          ~id, ~Box, ~Y, ~X,
          53, "Scots Bay", 44.7500, -65.61,
          54, "Scots Bay", 45.000, -64.67,
          55, "Scots Bay", 45.500, -64.67,
          56, "Scots Bay", 45.5, -65.61
        )
        
        SB_poly <- df %>% arrange(id) %>% summarise(
            Box = first(Box),
            geometry = st_sfc(
              st_polygon(list(
                rbind(
                  as.matrix(dplyr::select(dplyr::cur_data(), X, Y)),
                  as.matrix(dplyr::select(dplyr::cur_data(), X, Y))[1, ]
                )
              )),
              crs = 4326
            ),
            .groups = "drop"
          ) %>%
          st_as_sf()
        
        
  # German Bank
        df <- tribble(
          ~id, ~Box, ~Y, ~X,
          34, "German Bank", 43.7, -66.229,
          35 , "German Bank",43.56667, -66.229,
          36 , "German Bank",43.56667, -66.075,
          37, "German Bank", 43.233, -66.075,
          38, "German Bank", 43.233, -66.55,
          39, "German Bank", 43.7, -66.55,
        )
  
        GB_poly <- df %>% arrange(id) %>% summarise(
          Box = first(Box),
          geometry = st_sfc(
            st_polygon(list(
              rbind(
                as.matrix(dplyr::select(dplyr::cur_data(), X, Y)),
                as.matrix(dplyr::select(dplyr::cur_data(), X, Y))[1, ]
              )
            )),
            crs = 4326
          ),
          .groups = "drop"
        ) %>%
          st_as_sf()
  
        
        
        
# Categorize the DFO Data into Grounds, Scots Bay, German Bank or Other. Can add other polygons in the future if needed.
        
        label_scotsbay_germanbank <- function(df,
                                              lon = "lon", lat = "lat",
                                              ScotsBay, GermanBank,
                                              out_col = "region",
                                              crs_points = 4326,
                                              predicate = c("intersects", "within")) {
          predicate <- match.arg(predicate)
          
          # Points to sf
          pts <- st_as_sf(df, coords = c(lon, lat), crs = crs_points, remove = FALSE)
          
          # Ensure CRS matches
          if (st_crs(ScotsBay) != st_crs(pts)) ScotsBay <- st_transform(ScotsBay, st_crs(pts))
          if (st_crs(GermanBank) != st_crs(pts)) GermanBank <- st_transform(GermanBank, st_crs(pts))
          
          # Choose predicate
          pred_fun <- if (predicate == "within") st_within else st_intersects
          
          in_scots <- lengths(pred_fun(pts, ScotsBay)) > 0
          in_german <- lengths(pred_fun(pts, GermanBank)) > 0
          
          # Priority rule if overlap occurs: ScotsBay wins (you can swap if desired)
          df[[out_col]] <- ifelse(in_scots, "ScotsBay",
                                  ifelse(in_german, "GermanBank", "Other"))
          df
        }
        
        
        DFO <- label_scotsbay_germanbank(
          DFO,
          lon = "Lon",
          lat = "Lat",
          ScotsBay = SB_poly,
          GermanBank = GB_poly,
          out_col = "polygon",
          crs_points = crs,
          predicate = predicate
        )
        
        
# More formatting
        DFO$ground = DFO$polygon 
        DFO$polygon = NULL
        DFO$ground = as.factor(DFO$ground)
        
        levels(DFO$ground)[levels(DFO$ground) == "ScotsBay"] <- "Scots Bay"
        levels(DFO$ground)[levels(DFO$ground) == "GermanBank"] <- "German Bank"
        
        DFO$plankton_ID = NA
        DFO$Survey = DFO$cruise_id
        DFO$cruise_id = NULL
        DFO$flag = NULL
        DFO$Source = "DFO"
        
        
        # Now some mods for CTD data
        CTD$cruise_time = NA
        CTD$datatype = "CD"
        x =  aggregate(Depth ~ id, data = CTD, max, na.rm = TRUE)
        colnames(x) = c("id", "maximum_depth")
        CTD = merge(CTD, x, by = "id")
        CTD$stn_id = NA
        CTD$time = NA
        CTD$Source = "HSC"
        CTD$.source_file <- basename(ctd_path)
        
        # finally, rbind
        
        colnames(CTD)
        colnames(DFO)
        
        Ocean1 = rbind(CTD, DFO)
        Oceans <- Ocean1 %>%
          mutate(JulianDay = lubridate::yday(as.Date(Date)))
        
        # Removes data from grounds other than Scots Bay and German Bank
        Oceans <- Oceans %>%
          dplyr::filter(ground != "Other")
        
        return(Oceans)

}
