

strat_index <- function(data,
                        upper_min, upper_max,
                        lower_min, lower_max) {
  
  data %>%
    dplyr::mutate(Depth = as.numeric(Depth)) %>%
    dplyr::filter(!is.na(Density)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      
      dens_upper = mean(
        Density[Depth >= upper_min & Depth <= upper_max],
        na.rm = TRUE
      ),
      
      dens_lower = mean(
        Density[Depth >= lower_min & Depth <= lower_max],
        na.rm = TRUE
      ),
      
      strat_density = dens_upper - dens_lower,
      
      ground    = dplyr::first(ground),
      Source    = dplyr::first(Source),
      Year      = dplyr::first(Year),
      JulianDay = dplyr::first(JulianDay),
      Lat = dplyr::first(Lat),
      Lon = dplyr::first(Lon),
      
      .groups = "drop"
    )
}
