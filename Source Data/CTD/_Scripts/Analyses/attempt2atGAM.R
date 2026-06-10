

#Step1: 
surface_clean <- surface %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("German Bank", "Scots Bay"),
    !is.na(avgTemp),
    !is.na(JulianDay)
  )



#Step 2:

seasonal_overlap <- function(dat) {
  
  jd_hsc <- dat %>% filter(Source == "HSC") %>% pull(JulianDay)
  jd_dfo <- dat %>% filter(Source == "DFO") %>% pull(JulianDay)
  
  if (length(jd_hsc) < 2 | length(jd_dfo) < 2) {
    return(tibble(
      overlap_width = NA_real_,
      overlap_fraction = NA_real_
    ))
  }
  
  min_overlap <- max(min(jd_hsc), min(jd_dfo))
  max_overlap <- min(max(jd_hsc), max(jd_dfo))
  
  overlap_width <- max(0, max_overlap - min_overlap)
  
  full_range <- max(c(jd_hsc, jd_dfo)) - min(c(jd_hsc, jd_dfo))
  
  overlap_fraction <- overlap_width / full_range
  
  tibble(
    overlap_width = overlap_width,
    overlap_fraction = overlap_fraction
  )
}




# Step 3
seasonality_test <- function(dat, n_perm = 9999) {
  
  dat <- dat %>%
    filter(!is.na(JulianDay), Source %in% c("HSC", "DFO")) %>%
    mutate(Source = droplevels(factor(Source)))
  
  if (nlevels(dat$Source) != 2) {
    return(tibble(p_season = NA_real_, bias_flag = NA))
  }
  
  pt <- permTS(
    JulianDay ~ Source,
    data = dat,
    method = "exact.mc",
    control = permControl(nmc = n_perm)
  )
  
  p_val <- pt$p.value
  
  tibble(
    p_season = p_val,
    bias_flag = p_val < 0.05
  )
}



# Step 3.5
temp_comparison <- function(dat, bias_flag, n_perm = 9999, k_max = 6) {
  
  dat <- dat %>%
    filter(!is.na(avgTemp), !is.na(JulianDay), Source %in% c("HSC", "DFO")) %>%
    mutate(Source = droplevels(factor(Source)))
  
  mean_dfo <- mean(dat$avgTemp[dat$Source == "DFO"])
  mean_hsc <- mean(dat$avgTemp[dat$Source == "HSC"])
  effect   <- mean_hsc - mean_dfo
  
  # If no seasonal bias -> permutation test
  if (!isTRUE(bias_flag)) {
    
    pt <- perm::permTS(
      avgTemp ~ Source,
      data = dat,
      method = "exact.mc",
      control = perm::permControl(nmc = n_perm)
    )
    
    return(tibble(
      test_type = "Permutation test",
      mean_dfo = mean_dfo,
      mean_hsc = mean_hsc,
      effect_estimate = effect,
      p_value = pt$p.value
    ))
  }
  
  # If seasonal bias -> adjust for JulianDay
  n_uniqueJD <- dplyr::n_distinct(dat$JulianDay)
  
  # Need enough unique x values to support a spline basis
  # Rule of thumb: use k <= uniqueJD; also keep k at least 3 if smoothing
  k_use <- min(k_max, max(3, n_uniqueJD - 1))
  
  # If too few unique days, smooth isn't identifiable -> use linear adjustment
  if (n_uniqueJD < 4) {
    
    lm_fit <- stats::lm(avgTemp ~ Source + JulianDay, data = dat)
    p_val  <- summary(lm_fit)$coefficients["SourceHSC", "Pr(>|t|)"]
    
    return(tibble(
      test_type = "LM (Julian day linear; too few unique days for GAM)",
      mean_dfo = mean_dfo,
      mean_hsc = mean_hsc,
      effect_estimate = effect,
      p_value = p_val
    ))
  }
  
  # Otherwise fit GAM safely
  gam_out <- tryCatch({
    
    gam_fit <- mgcv::gam(
      avgTemp ~ Source + mgcv::s(JulianDay, k = k_use),
      data = dat,
      method = "REML"
    )
    
    p_val <- summary(gam_fit)$p.table["SourceHSC", "Pr(>|t|)"]
    
    tibble(
      test_type = paste0("GAM (Julian day adjusted; k=", k_use, ")"),
      mean_dfo = mean_dfo,
      mean_hsc = mean_hsc,
      effect_estimate = effect,
      p_value = p_val
    )
    
  }, error = function(e) {
    
    # Final fallback: linear if GAM still fails for any reason
    lm_fit <- stats::lm(avgTemp ~ Source + JulianDay, data = dat)
    p_val  <- summary(lm_fit)$coefficients["SourceHSC", "Pr(>|t|)"]
    
    tibble(
      test_type = paste0("LM fallback (GAM failed: ", conditionMessage(e), ")"),
      mean_dfo = mean_dfo,
      mean_hsc = mean_hsc,
      effect_estimate = effect,
      p_value = p_val
    )
  })
  
  gam_out
}


# Step 4

bias_table <- surface_clean %>%
  group_by(ground, JD_bin) %>%
  group_modify(~ {
    
    overlap <- seasonal_overlap(.x)
    season_test <- seasonality_test(.x)
    
    temp_test <- temp_comparison(
      .x,
      bias_flag = season_test$bias_flag
    )
    
    tibble(
      overlap_width = overlap$overlap_width,
      overlap_fraction = overlap$overlap_fraction,
      DFO_n = sum(.x$Source == "DFO"),
      HSC_n = sum(.x$Source == "HSC"),
      seasonal_bias_flag = paste0(
        season_test$bias_flag,
        " (p=", round(season_test$p_season, 3), ")"
      ),
      test_type = temp_test$test_type,
      mean_DFO = temp_test$mean_dfo,
      mean_HSC = temp_test$mean_hsc,
      effect_estimate = temp_test$effect_estimate,
      p_value = temp_test$p_value,
      significant_0.01 = temp_test$p_value < 0.01
    )
    
  }) %>%
  ungroup()

bias_table



write.table(bias_table, file= "bias_table.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
