
run_JDbin_bias_overlap_GAM <- function(surface,
                                       response = "avgTemp",
                                       grounds = c("Scots Bay", "German Bank"),
                                       sources = c("HSC", "DFO"),
                                       jd_levels = c("EarlySummer", "LateSummer", "Fall"),
                                       # If you want to compute JD_bin inside the function:
                                       make_bins = FALSE,
                                       # Bin definitions (used only if make_bins = TRUE)
                                       bin_breaks = list(
                                         EarlySummer = c(181, 220),
                                         LateSummer  = c(221, 260),
                                         Fall        = c(261, 300)
                                       ),
                                       # GAM settings / inference
                                       min_n = 5,
                                       k_cap = 6,
                                       adjust_method = "BH",
                                       # Exclusions are OPTIONAL (NULL means no exclusions)
                                       exclude = NULL,
                                       # export
                                       export_csv = FALSE,
                                       export_path = "Appendix_JDbin_bias_overlap_GAM_filtered.csv") {
  
  # ---- dependencies ----
  req_pkgs <- c("dplyr", "tidyr", "tibble", "mgcv")
  missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
         ". Please install them.")
  }
  
  # ---- optional: create JD_bin if not already present ----
  dat0 <- surface
  
  if (isTRUE(make_bins)) {
    dat0 <- dat0 |>
      dplyr::mutate(
        JD_bin = dplyr::case_when(
          JulianDay >= bin_breaks$EarlySummer[1] & JulianDay <= bin_breaks$EarlySummer[2] ~ "EarlySummer",
          JulianDay >= bin_breaks$LateSummer[1]  & JulianDay <= bin_breaks$LateSummer[2]  ~ "LateSummer",
          JulianDay >= bin_breaks$Fall[1]        & JulianDay <= bin_breaks$Fall[2]        ~ "Fall",
          TRUE ~ NA_character_
        )
      )
  }
  
  # ============================================================
  # 1) PREP (this is your “first part of the script”)
  # ============================================================
  dat <- dat0 |>
    dplyr::filter(
      Source %in% sources,
      ground %in% grounds,
      !is.na(JulianDay),
      !is.na(JD_bin)
    ) |>
    dplyr::mutate(
      JD_bin = factor(JD_bin, levels = jd_levels),
      Source = factor(Source, levels = c("DFO", "HSC")),  # DFO baseline
      is_HSC = as.integer(Source == "HSC")
    )
  
  # ============================================================
  # 2) OPTIONAL EXCLUSIONS
  # exclude should be a data.frame/tibble with columns: ground, JD_bin
  # Example:
  # exclude = tibble::tibble(ground=c("German Bank","Scots Bay"), JD_bin=c("EarlySummer","Fall"))
  # ============================================================
  if (!is.null(exclude) && nrow(exclude) > 0) {
    dat <- dat |> dplyr::anti_join(exclude, by = c("ground", "JD_bin"))
  }
  
  # helpful counts (you printed this in your script)
  counts_table <- dat |>
    dplyr::count(ground, JD_bin, Source, name = "n") |>
    dplyr::arrange(ground, JD_bin, Source)
  
  # ============================================================
  # 3) A) OVERLAP + JulianDay summaries by Source within ground × JD_bin
  # ============================================================
  summ_by_source <- dat |>
    dplyr::group_by(ground, JD_bin, Source) |>
    dplyr::summarise(
      n = dplyr::n(),
      jd_min    = min(JulianDay, na.rm = TRUE),
      jd_q25    = stats::quantile(JulianDay, 0.25, na.rm = TRUE),
      jd_median = stats::median(JulianDay, na.rm = TRUE),
      jd_q75    = stats::quantile(JulianDay, 0.75, na.rm = TRUE),
      jd_max    = max(JulianDay, na.rm = TRUE),
      .groups = "drop"
    )
  
  summ_wide <- summ_by_source |>
    tidyr::pivot_wider(
      names_from = Source,
      values_from = c(n, jd_min, jd_q25, jd_median, jd_q75, jd_max),
      names_sep = "_"
    ) |>
    dplyr::mutate(
      median_diff_HSC_minus_DFO = jd_median_HSC - jd_median_DFO,
      overlap_start = pmax(jd_min_HSC, jd_min_DFO, na.rm = TRUE),
      overlap_end   = pmin(jd_max_HSC, jd_max_DFO, na.rm = TRUE),
      overlap_width = pmax(0, overlap_end - overlap_start),
      combined_span = pmax(jd_max_HSC, jd_max_DFO, na.rm = TRUE) -
        pmin(jd_min_HSC, jd_min_DFO, na.rm = TRUE),
      overlap_fraction = dplyr::if_else(combined_span > 0,
                                        overlap_width / combined_span,
                                        NA_real_),
      window_summary = paste0(
        "DFO: ", jd_min_DFO, "–", jd_max_DFO, " (med ", jd_median_DFO, ") | ",
        "HSC: ", jd_min_HSC, "–", jd_max_HSC, " (med ", jd_median_HSC, ") | ",
        "Overlap: ", overlap_start, "–", overlap_end,
        " (w=", round(overlap_width, 1), ")"
      )
    )
  
  # ============================================================
  # 4) B) GAM-based bias test per ground × JD_bin
  # “Seasonality bias within bin” implemented as:
  #   m0: y ~ Source + s(JulianDay)
  #   m1: y ~ Source + s(JulianDay, by = Source)
  # p_value = does allowing different smooths by Source improve fit?
  # ============================================================
  fit_bias_gam <- function(df) {
    
    d <- df |>
      dplyr::filter(!is.na(.data[[response]]),
                    !is.na(JulianDay),
                    !is.na(Source)) |>
      dplyr::mutate(Source = droplevels(Source))
    
    n_by <- d |> dplyr::count(Source, name = "n")
    HSC_n <- n_by$n[n_by$Source == "HSC"]; if (length(HSC_n) == 0) HSC_n <- 0L
    DFO_n <- n_by$n[n_by$Source == "DFO"]; if (length(DFO_n) == 0) DFO_n <- 0L
    n_tot <- nrow(d)
    
    if (HSC_n < min_n || DFO_n < min_n) {
      return(tibble::tibble(
        n = n_tot, DFO_n = DFO_n, HSC_n = HSC_n,
        k_used = NA_integer_, dev_expl = NA_real_,
        p_value = NA_real_,
        note = "insufficient samples for GAM"
      ))
    }
    
    k_used <- min(k_cap, max(3, length(unique(d$JulianDay)) - 1))
    
    m0 <- mgcv::gam(
      stats::as.formula(paste0(response, " ~ Source + s(JulianDay, k=", k_used, ")")),
      data = d, method = "REML"
    )
    m1 <- mgcv::gam(
      stats::as.formula(paste0(response, " ~ Source + s(JulianDay, by=Source, k=", k_used, ")")),
      data = d, method = "REML"
    )
    
    cmp <- mgcv::anova.gam(m0, m1, test = "Chisq")
    p <- suppressWarnings(as.numeric(cmp$`Pr(>Chi)`[2]))
    
    tibble::tibble(
      n = n_tot, DFO_n = DFO_n, HSC_n = HSC_n,
      k_used = k_used,
      dev_expl = as.numeric(summary(m1)$dev.expl),
      p_value = p,
      note = NA_character_
    )
  }
  
  bias_table <- dat |>
    dplyr::group_by(ground, JD_bin) |>
    dplyr::group_modify(~ fit_bias_gam(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      p_adj = stats::p.adjust(p_value, method = adjust_method),
      bias_flag_p05 = !is.na(p_value) & p_value < 0.05
    )
  
  # ============================================================
  # 5) C) MERGE to appendix table (+ rounding like your script)
  # ============================================================
  appendix_table <- summ_wide |>
    dplyr::left_join(bias_table, by = c("ground", "JD_bin")) |>
    dplyr::arrange(ground, JD_bin) |>
    dplyr::mutate(
      median_diff_HSC_minus_DFO = round(median_diff_HSC_minus_DFO, 1),
      overlap_width = round(overlap_width, 1),
      overlap_fraction = round(overlap_fraction, 3),
      dev_expl = round(dev_expl, 3),
      p_value = signif(p_value, 3),
      p_adj = signif(p_adj, 3)
    )
  
  # ============================================================
  # 6) D) REDUCED export table (exact columns you used)
  # ============================================================
  appendix_table_reduced <- appendix_table |>
    dplyr::select(
      ground,
      JD_bin,
      DFO_n,
      HSC_n,
      overlap_width,
      overlap_fraction,
      median_diff_HSC_minus_DFO,
      p_adj,
      bias_flag_p05
    ) |>
    dplyr::arrange(ground, JD_bin)
  
  if (isTRUE(export_csv)) {
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop("Package 'readr' is required for export_csv = TRUE. Please install it.")
    }
    readr::write_csv(appendix_table_reduced, export_path)
  }
  
  # return everything (so you can inspect like before)
  list(
    dat = dat,
    counts_table = counts_table,
    summ_by_source = summ_by_source,
    summ_wide = summ_wide,
    bias_table = bias_table,
    appendix_table = appendix_table,
    appendix_table_reduced = appendix_table_reduced
  )
}