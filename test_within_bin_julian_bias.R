


test_within_bin_julian_bias <- function(data,
                                        temp_var = "avgTemp",
                                        min_n = 10,
                                        use_gam = TRUE,
                                        k = 4) {
  
  stopifnot(all(c("ground", "JD_bin", "Source", "JulianDay", temp_var) %in% names(data)))
  
  fit_model <- function(df) {
    
    mean_HSC <- mean(df[[temp_var]][df$Source == "HSC"], na.rm = TRUE)
    mean_DFO <- mean(df[[temp_var]][df$Source == "DFO"], na.rm = TRUE)
    diff_unadjusted <- mean_HSC - mean_DFO
    
    if (use_gam) {
      m <- mgcv::gam(
        as.formula(paste0(temp_var, " ~ Source + s(JulianDay, k = ", k, ")")),
        data = df,
        method = "REML"
      )
      smry <- summary(m)$p.table
    } else {
      m <- lm(
        as.formula(paste0(temp_var, " ~ Source + JulianDay")),
        data = df
      )
      smry <- summary(m)$coefficients
    }
    
    est <- smry["SourceHSC", "Estimate"]
    se  <- smry["SourceHSC", "Std. Error"]
    p   <- smry["SourceHSC", "Pr(>|t|)"]
    
    tibble::tibble(
      mean_HSC = mean_HSC,
      mean_DFO = mean_DFO,
      diff_unadjusted = diff_unadjusted,
      diff_adjusted   = est,
      se_adjusted     = se,
      p_adjusted      = p,
      delta_due_to_JD = est - diff_unadjusted
    )
  }
  
  data %>%
    dplyr::group_by(ground, JD_bin) %>%
    dplyr::filter(
      sum(Source == "HSC" & !is.na(.data[[temp_var]])) >= min_n,
      sum(Source == "DFO" & !is.na(.data[[temp_var]])) >= min_n
    ) %>%
    dplyr::group_modify(~ fit_model(.x)) %>%
    dplyr::ungroup()
}

