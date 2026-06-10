
make_si_like_surface <- function(surface, deep,
                                 id_col  = "id",
                                 rho_col = "avgDensity",
                                 method  = c("delta_rho", "abs_delta_rho"),
                                 drop_na_ids = TRUE) {
  
  method <- match.arg(method)
  
  # sanity checks
  stopifnot(id_col  %in% names(surface),
            id_col  %in% names(deep),
            rho_col %in% names(surface),
            rho_col %in% names(deep))
  
  # 1) one row per id in surface (keep all original columns)
  s <- surface |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  # 2) one deep density per id (in case of duplicates)
  d_rho <- deep |>
    dplyr::select(dplyr::all_of(id_col), rho_deep = dplyr::all_of(rho_col)) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(
      rho_deep = dplyr::first(rho_deep[!is.na(rho_deep)]),
      .groups = "drop"
    )
  
  # 3) join deep density onto surface template
  s2 <- dplyr::left_join(s, d_rho, by = id_col)
  
  # 4) compute stratification value (density difference)
  rho_surface <- s2[[rho_col]]
  delta_rho   <- s2$rho_deep - rho_surface
  
  strat_value <- if (method == "abs_delta_rho") abs(delta_rho) else delta_rho
  
  # 5) optionally drop IDs with NA in either layer density (=> NA strat_value)
  if (drop_na_ids) {
    keep <- !is.na(rho_surface) & !is.na(s2$rho_deep) & !is.na(strat_value)
    s2   <- s2[keep, , drop = FALSE]
    strat_value <- strat_value[keep]
  }
  
  # 6) overwrite rho_col in the surface-shaped dataset; drop helper column
  s2[[rho_col]] <- strat_value
  s2$rho_deep   <- NULL
  
  # 7) return EXACTLY the original surface columns + order
  s2 <- s2[, names(surface), drop = FALSE]
  
  # optional metadata without changing columns
  attr(s2, "si_definition") <- paste0(
    "SI(", method, ") = ", rho_col, "_deep - ", rho_col, "_surface; stored in column ", rho_col
  )
  
  s2
}
