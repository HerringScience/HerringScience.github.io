

library(dplyr)
library(coin)
library(tibble)

library(dplyr)
library(coin)
library(tibble)

run_perm_test <- function(data, response, alpha = 0.01, nperm = 9999) {
  
  # Count n per group
  n_table <- table(data$Source)
  
  # Means + sample sizes (always computed)
  means <- data %>%
    group_by(Source) %>%
    summarise(mean = mean(.data[[response]], na.rm = TRUE),
              n = n(), .groups = "drop")
  
  mean_DFO <- means$mean[means$Source == "DFO"]
  mean_HSC <- means$mean[means$Source == "HSC"]
  n_DFO    <- means$n[means$Source == "DFO"]
  n_HSC    <- means$n[means$Source == "HSC"]
  
  # Handle missing groups gracefully
  mean_DFO <- ifelse(length(mean_DFO) == 0, NA_real_, mean_DFO)
  mean_HSC <- ifelse(length(mean_HSC) == 0, NA_real_, mean_HSC)
  n_DFO    <- ifelse(length(n_DFO) == 0, 0L, n_DFO)
  n_HSC    <- ifelse(length(n_HSC) == 0, 0L, n_HSC)
  
  # Require at least 5 per group for inference (change if you like)
  if (any(n_table < 5)) {
    return(tibble(
      mean_DFO = mean_DFO,
      mean_HSC = mean_HSC,
      diff_HSC_minus_DFO = mean_HSC - mean_DFO,
      p_value = NA_real_,
      alpha = alpha,
      significant_99 = NA,
      sig_label = NA_character_,
      note = "Insufficient sample size"
    ))
  }
  
  # Permutation test
  test <- oneway_test(
    reformulate("Source", response),
    data = data,
    distribution = approximate(nresample = nperm)
  )
  
  # IMPORTANT: coerce pvalue() to plain numeric to avoid vctrs bind_rows error
  p_num <- as.numeric(pvalue(test))
  
  tibble(
    mean_DFO = mean_DFO,
    mean_HSC = mean_HSC,
    diff_HSC_minus_DFO = mean_HSC - mean_DFO,
    p_value = p_num,
    alpha = alpha,
    significant_99 = (p_num < alpha),
    sig_label = dplyr::case_when(
      is.na(p_num)      ~ NA_character_,
      p_num < 0.001     ~ "***",   # very strong
      p_num < 0.01      ~ "**",    # 99% threshold
      p_num < 0.05      ~ "*",     # 95% (still useful to display)
      TRUE              ~ "ns"
    ),
    note = paste0("Permutation test (coin), alpha=", alpha)
  )
}