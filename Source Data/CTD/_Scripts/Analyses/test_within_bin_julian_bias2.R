test_within_bin_julian_bias2 <- function(data,
                                        temp_var = "avgTemp",
                                        min_n = 10,
                                        use_gam = TRUE,
                                        k = 4,
                                        make_plots = TRUE,
                                        n_grid = 100,
                                        save_plots = TRUE,
                                        plot_dir = "within_bin_models",
                                        plot_ext = "png",
                                        plot_width = 7,
                                        plot_height = 5,
                                        plot_dpi = 300) {
  
  stopifnot(all(c("ground", "JD_bin", "Source", "JulianDay", temp_var) %in% names(data)))
  
  # helper to make filenames safe across OS
  safe_name <- function(x) {
    x <- as.character(x)
    x <- gsub("[^A-Za-z0-9]+", "_", x)  # replace non-alphanumeric with _
    x <- gsub("^_+|_+$", "", x)         # trim underscores
    x
  }
  
  fit_model <- function(df, keys) {
    
    # Ensure consistent factor coding so SourceHSC exists
    df <- df |>
      dplyr::mutate(Source = stats::relevel(factor(Source), ref = "DFO"))
    
    # unadjusted means & difference
    mean_HSC <- mean(df[[temp_var]][df$Source == "HSC"], na.rm = TRUE)
    mean_DFO <- mean(df[[temp_var]][df$Source == "DFO"], na.rm = TRUE)
    diff_unadjusted <- mean_HSC - mean_DFO
    
    # model fit
    if (use_gam) {
      m <- mgcv::gam(
        stats::as.formula(paste0(temp_var, " ~ Source + s(JulianDay, k = ", k, ")")),
        data = df,
        method = "REML"
      )
      smry <- summary(m)$p.table
    } else {
      m <- stats::lm(
        stats::as.formula(paste0(temp_var, " ~ Source + JulianDay")),
        data = df
      )
      smry <- summary(m)$coefficients
    }
    
    est <- smry["SourceHSC", "Estimate"]
    se  <- smry["SourceHSC", "Std. Error"]
    p   <- smry["SourceHSC", "Pr(>|t|)"]
    
    # Build plot (data + fitted lines + CI ribbon)
    plt <- NULL
    if (make_plots) {
      jd_seq <- seq(min(df$JulianDay, na.rm = TRUE),
                    max(df$JulianDay, na.rm = TRUE),
                    length.out = n_grid)
      
      newdat <- tidyr::expand_grid(
        JulianDay = jd_seq,
        Source = factor(c("DFO", "HSC"), levels = levels(df$Source))
      )
      
      pred <- predict(m, newdata = newdat, se.fit = TRUE, type = "response")
      newdat$fit <- pred$fit
      newdat$se  <- pred$se.fit
      
      newdat <- newdat |>
        dplyr::mutate(
          lower = fit - 1.96 * se,
          upper = fit + 1.96 * se
        )
      
      title_txt <- paste0(keys$ground, " — ", keys$JD_bin,
                          if (use_gam) paste0(" (GAM, k=", k, ")") else " (LM)")
      
      plt <- ggplot2::ggplot(df, ggplot2::aes(x = JulianDay, y = .data[[temp_var]], colour = Source)) +
        ggplot2::geom_point(alpha = 0.6, size = 1.6) +
        ggplot2::geom_ribbon(
          data = newdat,
          ggplot2::aes(x = JulianDay, ymin = lower, ymax = upper, fill = Source),
          alpha = 0.18, colour = NA,
          inherit.aes = FALSE
        ) +
        ggplot2::geom_line(
          data = newdat,
          ggplot2::aes(x = JulianDay, y = fit, colour = Source),
          linewidth = 1.0,
          inherit.aes = FALSE
        ) +
        ggplot2::labs(title = title_txt, y = temp_var, x = "Julian day") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")
      
      # ---- AUTO-SAVE PLOT ----
      if (save_plots) {
        # create directory if needed (works even if it already exists)
        if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
        
        fname <- paste0(
          safe_name(keys$ground), "__",
          safe_name(keys$JD_bin), "__",
          safe_name(temp_var), "__",
          if (use_gam) paste0("GAM_k", k) else "LM",
          ".", plot_ext
        )
        
        ggplot2::ggsave(
          filename = fname,
          plot = plt,
          path = plot_dir,
          width = plot_width,
          height = plot_height,
          dpi = plot_dpi,
          units = "in"
        )
        # ggsave defaults & args documented here [1](https://ggplot2.tidyverse.org/reference/ggsave.html)[2](https://rdrr.io/cran/ggplot2/man/ggsave.html)
      }
    }
    
    out <- tibble::tibble(
      mean_HSC = mean_HSC,
      mean_DFO = mean_DFO,
      diff_unadjusted = diff_unadjusted,
      diff_adjusted   = est,
      se_adjusted     = se,
      p_adjusted      = p,
      delta_due_to_JD = est - diff_unadjusted
    )
    
    # return model and plot too (optional but handy)
    out$model <- list(m)
    out$plot  <- list(plt)
    
    out
  }
  
  data |>
    dplyr::group_by(ground, JD_bin) |>
    dplyr::filter(
      sum(Source == "HSC" & !is.na(.data[[temp_var]])) >= min_n,
      sum(Source == "DFO" & !is.na(.data[[temp_var]])) >= min_n
    ) |>
    dplyr::group_modify(~ fit_model(.x, keys = .y)) |>
    dplyr::ungroup()
}

colnames(surface)

# run it
res <- test_within_bin_julian_bias2(
  surface,
  temp_var   = "avgTemp",
  use_gam    = TRUE,
  k          = 4,
  save_plots = TRUE,
  plot_dir   = "within_bin_models",
  plot_ext   = "png"   # or "pdf"
)




### look at the contributions of salinity and temp to the large changes in density in german bank


temp <- read.csv("test_within_julian_bias_Surfacetemp.csv")
sal  <- read.csv("test_within_julian_bias_SurfaceSalinity.csv")
dens <- read.csv("test_within_julian_bias_SurfaceDensity.csv") 
# assuming this is the table you created for avgDensity

library(dplyr)

combined <- temp %>%
  select(ground, JD_bin,
         dT = diff_adjusted) %>%
  left_join(
    sal %>% select(ground, JD_bin,
                   dS = diff_adjusted),
    by = c("ground", "JD_bin")
  ) %>%
  left_join(
    dens %>% select(ground, JD_bin,
                    dRho = diff_adjusted),
    by = c("ground", "JD_bin")
  )

res_export <- res |>
  dplyr::select(
    ground, JD_bin,
    mean_HSC, mean_DFO,
    diff_unadjusted,
    diff_adjusted,
    se_adjusted,
    p_adjusted,
    delta_due_to_JD
  )

write.csv(
  res_export,
  "test_within_julian_bias_Surfacetemp2.csv",
  row.names = FALSE
)

sens <- lm(dRho ~ dT + dS, data = combined)
summary(sens)


coefs <- coef(sens)

combined <- combined %>%
  mutate(
    rho_T = coefs["dT"] * dT,
    rho_S = coefs["dS"] * dS,
    rho_sum = rho_T + rho_S,
    unexplained = dRho - rho_sum
  )
