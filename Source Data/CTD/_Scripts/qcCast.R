
library(dplyr)
library(slider)
library(purrr)
library(tidyr)

detect_stability_depth <- function(dat,
                                   temp_jump = 0.015,   # deg C between adjacent points
                                   sal_jump  = 0.020,   # PSU between adjacent points
                                   dens_inv  = 0.005,   # kg m-3 allowed inversion tolerance
                                   temp_sd   = 0.015,   # rolling SD threshold
                                   sal_sd    = 0.020,   # rolling SD threshold
                                   roll_n    = 5,       # rolling window size
                                   consec_n  = 8) {     # stable points in a row needed
  
  dat2 <- dat %>%
    arrange(Depth) %>%
    filter(
      !is.na(Depth),
      !is.na(Temperature),
      !is.na(Salinity),
      !is.na(Density)
    ) %>%
    mutate(
      dTemp = c(NA, diff(Temperature)),
      dSal  = c(NA, diff(Salinity)),
      dDens = c(NA, diff(Density)),
      
      roll_sd_temp = slide_dbl(
        Temperature, 
        ~sd(.x, na.rm = TRUE), 
        .before = roll_n - 1, 
        .complete = TRUE
      ),
      roll_sd_sal = slide_dbl(
        Salinity, 
        ~sd(.x, na.rm = TRUE), 
        .before = roll_n - 1, 
        .complete = TRUE
      ),
      
      unstable = case_when(
        is.na(dTemp) | is.na(dSal) | is.na(dDens) ~ TRUE,
        abs(dTemp) > temp_jump ~ TRUE,
        abs(dSal)  > sal_jump  ~ TRUE,
        dDens < -dens_inv      ~ TRUE,      # density drops too much with depth
        roll_sd_temp > temp_sd ~ TRUE,
        roll_sd_sal  > sal_sd  ~ TRUE,
        TRUE ~ FALSE
      ),
      
      stable = !unstable
    )
  
  # Find first depth where there are consec_n stable points in a row
  stable_run <- slider::slide_lgl(
    dat2$stable,
    ~ all(.x, na.rm = TRUE),
    .before = 0,
    .after = consec_n - 1,
    .complete = TRUE
  )
  
  if (any(stable_run, na.rm = TRUE)) {
    first_idx <- which(stable_run)[1]
    stab_depth <- dat2$Depth[first_idx]
  } else {
    stab_depth <- NA_real_
  }
  
  tibble(
    stabilization_depth = stab_depth,
    n_points = nrow(dat2),
    any_stable_run = any(stable_run, na.rm = TRUE)
  )
}
``