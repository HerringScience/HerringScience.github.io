
library(ggplot2)
library(patchwork)
library(scales)
library(cli)
library(lubridate)
library(reprex)
library(tidyverse)
library(geosphere)
library(reshape2)
library(moderndive)
library(skimr)
library(ggridges)
library(GGally)
library(psych)
library(raster)
library(PBSmapping)
library(sf)
library(terra)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(DT)
library(dygraphs)
library(leaflet)
library(rmapshaper)
library(plotly)
library(mapproj)
library(oce) #new CTD Data package
library(pander)
library(geodata)
library(pacman)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(devtools)
library(maps)
library(dplyr)
library(coin)
library(readr)
library(stringr)
###logistic GAM bias test:
library(purrr)
library(tidyr)
library(mgcv)
library(readr)
### appenidx export:
library(writexl)
library(perm)



# CTD DATA

# Look at DFO and HSC data

source("build_Oceans_df.R")


Oceans  = build_Oceans_df(ctd_path = "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv", dfo_paths = c("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/herringLarvalSurveyClimateData.csv", "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/herringLarvalSurvey2009Data.csv"))

colnames(Oceans)


# Functions
source("se.R")
source("Layer_Oceans.R")
source("surface_Oceans.R")

surface <- Layer_Oceans(Oceans, mode = "range", depth_lower = 0, depth_upper = 5,
                     add_JD_bin = TRUE, min_n = 10, return = "summary")


write.table(surface, file= "surface.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


colnames(surface)


deep <- Layer_Oceans(Oceans, mode = "range", depth_lower = 29, depth_upper = 31,
                         add_JD_bin = TRUE, min_n = 10, return = "summary")

both <- dplyr::left_join(surf, deep, by = c("id", "ground", "Source", "Date", "Year", "JulianDay", "Lat", "Lon"))

write.table(deep, file= "deep.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


write.table(surface, file= "surface.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


#source("surface_Oceans.R")
#source("depth_Oceans.R")
source("strat_Oceans.R")

strat <- strat_index(
  data      = Oceans,
  upper_min = 0,
  upper_max = 5,
  lower_min = 20,
  lower_max = 30
)


write.table(atDep, file= "atDep.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

write.table(strat, file= "strat.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 



#################################################
# Binning

# surface


write.table(surface, file= "surface.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

# looks at ground, HSC v DFO for avgTemp1
colnames(surface)

surface %>%
  filter(Source %in% c("HSC", "DFO"),
         ground %in% c("Scots Bay", "German Bank"),
         !is.na(avgTemp)) %>%
  ggplot(aes(x = avgTemp, fill = Source)) +
  geom_histogram(
    position = "identity",   # overlay HSC + DFO
    alpha = 0.45,
    bins = 30,
    color = "grey20"
  ) +
  facet_wrap(~ ground, ncol = 1) +
  labs(
    title = "Histogram of SST (avgTemp): HSC vs DFO by Ground",
    x = "SST",
    y = "Count",
    fill = "Source"
  ) +
  theme_classic()


# looks at ground, HSC v DFO for avgTemp1, adds JD_bin

surface %>%
  filter(Source %in% c("HSC", "DFO"),
         ground %in% c("Scots Bay", "German Bank"),
         !is.na(avgTemp),
         !is.na(JD_bin)) %>%
  mutate(
    JD_bin = factor(
      JD_bin,
      levels = c("EarlySummer", "LateSummer", "Fall")
    )
  ) %>%
  ggplot(aes(x = avgTemp, fill = Source)) +
  geom_histogram(
    position = "identity",
    alpha = 0.45,
    bins = 30,
    color = "grey20"
  ) +
  facet_grid(ground ~ JD_bin) +
  labs(
    title = "Histogram of SST (avgTemp): HSC vs DFO by Ground and Seasonal Bin",
    x = "SST",
    y = "Count",
    fill = "Source"
  ) +
  theme_classic()


# At depth

deep %>%
  filter(Source %in% c("HSC", "DFO"),
         ground %in% c("Scots Bay", "German Bank"),
         !is.na(avgTemp),
         !is.na(JD_bin)) %>%
  mutate(
    JD_bin = factor(
      JD_bin,
      levels = c("EarlySummer", "LateSummer", "Fall")
    )
  ) %>%
  ggplot(aes(x = avgTemp, fill = Source)) +
  geom_histogram(
    position = "identity",
    alpha = 0.45,
    bins = 30,
    color = "grey20"
  ) +
  facet_grid(ground ~ JD_bin) +
  labs(
    title = "Histogram of Temp at ~30m (avgTemp): HSC vs DFO by Ground and Seasonal Bin",
    x = "30m Temperature",
    y = "Count",
    fill = "Source"
  ) +
  theme_classic()


# Look at julian day spread:

surface %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("Scots Bay", "German Bank"),
    !is.na(JulianDay),
    !is.na(JD_bin)
  ) %>%
  mutate(
    JD_bin = factor(
      JD_bin,
      levels = c("EarlySummer", "LateSummer", "Fall")
    )
  ) %>%
  ggplot(aes(x = JulianDay, fill = Source)) +
  geom_histogram(
    position = "identity",
    alpha = 0.45,
    bins = 20,
    color = "grey20"
  ) +
  facet_grid(ground ~ JD_bin) +
  labs(
    title = "Julian Day Sampling Distribution: HSC vs DFO by Ground and Seasonal Bin",
    x = "Julian Day",
    y = "Count",
    fill = "Source"
  ) +
  theme_classic()


# at depth
deep %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("Scots Bay", "German Bank"),
    !is.na(JulianDay),
    !is.na(JD_bin)
  ) %>%
  mutate(
    JD_bin = factor(
      JD_bin,
      levels = c("EarlySummer", "LateSummer", "Fall")
    )
  ) %>%
  ggplot(aes(x = JulianDay, fill = Source)) +
  geom_histogram(
    position = "identity",
    alpha = 0.45,
    bins = 20,
    color = "grey20"
  ) +
  facet_grid(ground ~ JD_bin) +
  labs(
    title = "Julian Day Sampling Distribution: HSC vs DFO by Ground and Seasonal Bin",
    x = "Julian Day",
    y = "Count",
    fill = "Source"
  ) +
  theme_classic()



# table for paper
final_bin_table <- surface %>%
  filter(!is.na(JD_bin)) %>%
  group_by(ground, JD_bin, Source) %>%
  summarise(
    n = n(),
    years = str_c(sort(unique(Year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Source,
    values_from = c(n, years),
    values_fill = list(n = 0, years = "")
  ) %>%
  # Ensure columns exist even if one Source is absent in some groups
  mutate(
    n_DFO = if (!"n_DFO" %in% names(.)) 0 else n_DFO,
    n_HSC = if (!"n_HSC" %in% names(.)) 0 else n_HSC,
    years_DFO = if (!"years_DFO" %in% names(.)) "" else years_DFO,
    years_HSC = if (!"years_HSC" %in% names(.)) "" else years_HSC
  ) %>%
  transmute(
    ground, JD_bin,
    DFO_n = n_DFO,
    HSC_n = n_HSC,
    DFO_years = years_DFO,
    HSC_years = years_HSC,
    usable_for_inference = DFO_n >= 10 & HSC_n >= 10
  ) %>%
  arrange(ground, JD_bin)

final_bin_table



write.table(final_bin_table, file= "surfinYears.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


# at depth:
final_bin_table <- deep %>%
  filter(!is.na(JD_bin)) %>%
  group_by(ground, JD_bin, Source) %>%
  summarise(
    n = n(),
    years = str_c(sort(unique(Year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Source,
    values_from = c(n, years),
    values_fill = list(n = 0, years = "")
  ) %>%
  # Ensure columns exist even if one Source is absent in some groups
  mutate(
    n_DFO = if (!"n_DFO" %in% names(.)) 0 else n_DFO,
    n_HSC = if (!"n_HSC" %in% names(.)) 0 else n_HSC,
    years_DFO = if (!"years_DFO" %in% names(.)) "" else years_DFO,
    years_HSC = if (!"years_HSC" %in% names(.)) "" else years_HSC
  ) %>%
  transmute(
    ground, JD_bin,
    DFO_n = n_DFO,
    HSC_n = n_HSC,
    DFO_years = years_DFO,
    HSC_years = years_HSC,
    usable_for_inference = DFO_n >= 10 & HSC_n >= 10
  ) %>%
  arrange(ground, JD_bin)

final_bin_table


write.table(final_bin_table, file= "deepinYears.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 



# Statistics:

#Bin Stats and permutation tests

# Valid bins used for inference

# surface
valid_bins <- tibble::tribble(
  ~ground,         ~JD_bin,        ~use,
  "Scots Bay",     "EarlySummer",  TRUE,
  "Scots Bay",     "LateSummer",   TRUE,
  "Scots Bay",     "Fall",         FALSE,
  "German Bank",   "EarlySummer",  FALSE,
  "German Bank",   "LateSummer",   TRUE,
  "German Bank",   "Fall",         TRUE
)

# deep
valid_bins <- tibble::tribble(
  ~ground,         ~JD_bin,        ~use,
  "Scots Bay",     "EarlySummer",  FALSE,
  "Scots Bay",     "LateSummer",   FALSE,
  "Scots Bay",     "Fall",         FALSE,
  "German Bank",   "EarlySummer",  FALSE,
  "German Bank",   "LateSummer",   FALSE,
  "German Bank",   "Fall",         TRUE
)


source("run_perm_test.R")

# Filter data once, reuse everywhere

      #surface
      analysis_data <- surface %>%
        filter(Source %in% c("DFO", "HSC")) %>%
        inner_join(filter(valid_bins, use),
                   by = c("ground", "JD_bin"))

      # deep
      analysis_data <- deep %>%
        filter(Source %in% c("DFO", "HSC")) %>%
        inner_join(filter(valid_bins, use),
                   by = c("ground", "JD_bin"))
      

        # Permutation test results
        results <- analysis_data %>%
          group_by(ground, JD_bin) %>%
          group_modify(~ run_perm_test(.x, response = "avgTemp")) %>%
          ungroup()

              # Sample sizes (matched to analysis bins)
              sample_sizes <- analysis_data %>%
                group_by(ground, JD_bin, Source) %>%
                summarise(n = n(), .groups = "drop") %>%
                pivot_wider(names_from = Source, values_from = n, values_fill = 0)
                      
                      
                      # Final results table
                      # surface
                      final_results <- results %>%
                        left_join(sample_sizes, by = c("ground", "JD_bin")) %>%
                        mutate(
                          p_adj = p.adjust(p_value, method = "BH")
                        )
                      
                          
                          final_results <- results %>%
                            left_join(sample_sizes, by = c("ground", "JD_bin")) %>%
                            mutate(
                              p_adj = p.adjust(p_value, method = "BH")
                            )


          write_csv(final_results, "T_results_HSC_vs_DFO.csv")

### up to here good and QCed








          
          
          
          
          
          
          #Is SST changing through the season within a ground?
                  



source("run_JDbin_bias_overlap_GAM.R")

res_all <- run_JDbin_bias_overlap_GAM(
  surface = surface,
  response = "avgTemp",
  min_n = 10,
  k_cap = 6,
  exclude = NULL,        # <— no exclusions
  export_csv = FALSE
)

res_all$bias_table
res_all$counts_table

# ---- prep ----
# assumes surface already has JD_bin
dat <- surface %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("Scots Bay", "German Bank"),
    !is.na(JulianDay),
    !is.na(JD_bin)
  ) %>%
  mutate(
    JD_bin = factor(JD_bin, levels = c("EarlySummer", "LateSummer", "Fall")),
    Source = factor(Source, levels = c("DFO", "HSC")),  # DFO baseline
    is_HSC = as.integer(Source == "HSC")
  )



### GAM-based bias test. Are my bins biased? models to test for seasonality bias within the bins

dat <- surface %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("Scots Bay", "German Bank"),
    !is.na(JulianDay),
    !is.na(JD_bin),
    # ---- REMOVE these two strata ----
    !(ground == "German Bank" & JD_bin == "EarlySummer"),
    !(ground == "Scots Bay"   & JD_bin == "Fall")
  ) %>%
  mutate(
    JD_bin = factor(JD_bin, levels = c("EarlySummer", "LateSummer", "Fall")),
    Source = factor(Source, levels = c("DFO", "HSC")),
    is_HSC = as.integer(Source == "HSC")
  )

bias_table <- dat %>%
  group_by(ground, JD_bin) %>%
  group_modify(~ fit_bias_gam(.x, k_cap = 6)) %>%
  ungroup() %>%
  mutate(
    p_adj_BH = p.adjust(p_value, method = "BH"),
    bias_flag_p05 = !is.na(p_value) & p_value < 0.05
  )

bias_table

dat %>%
  count(ground, JD_bin, Source) %>%
  arrange(ground, JD_bin, Source)


# -------------------------
# Prep + EXCLUSIONS
# Remove: German Bank EarlySummer and Scots Bay Fall
# -------------------------
dat <- surface %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("Scots Bay", "German Bank"),
    !is.na(JulianDay),
    !is.na(JD_bin),
    !(ground == "German Bank" & JD_bin == "EarlySummer"),
    !(ground == "Scots Bay"   & JD_bin == "Fall")
  ) %>%
  mutate(
    JD_bin = factor(JD_bin, levels = c("EarlySummer", "LateSummer", "Fall")),
    Source = factor(Source, levels = c("DFO", "HSC")),
    is_HSC = as.integer(Source == "HSC")
  )

# ============================================================
# A) OVERLAP + JulianDay summaries by Source within each ground × JD_bin
# ============================================================
summ_by_source <- dat %>%
  group_by(ground, JD_bin, Source) %>%
  summarise(
    n = n(),
    jd_min    = min(JulianDay),
    jd_q25    = quantile(JulianDay, 0.25, na.rm = TRUE),
    jd_median = median(JulianDay, na.rm = TRUE),
    jd_q75    = quantile(JulianDay, 0.75, na.rm = TRUE),
    jd_max    = max(JulianDay),
    .groups = "drop"
  )

summ_wide <- summ_by_source %>%
  pivot_wider(
    names_from = Source,
    values_from = c(n, jd_min, jd_q25, jd_median, jd_q75, jd_max),
    names_sep = "_"
  ) %>%
  mutate(
    # median sampling shift (HSC - DFO)
    median_diff_HSC_minus_DFO = jd_median_HSC - jd_median_DFO,
    
    # overlap of min/max ranges
    overlap_start = pmax(jd_min_HSC, jd_min_DFO, na.rm = TRUE),
    overlap_end   = pmin(jd_max_HSC, jd_max_DFO, na.rm = TRUE),
    overlap_width = pmax(0, overlap_end - overlap_start),
    
    # total combined span for scaling overlap
    combined_span = pmax(jd_max_HSC, jd_max_DFO, na.rm = TRUE) -
      pmin(jd_min_HSC, jd_min_DFO, na.rm = TRUE),
    
    overlap_fraction = ifelse(combined_span > 0, overlap_width / combined_span, NA_real_),
    
    # readable summary string for appendix
    window_summary = paste0(
      "DFO: ", jd_min_DFO, "–", jd_max_DFO, " (med ", jd_median_DFO, ") | ",
      "HSC: ", jd_min_HSC, "–", jd_max_HSC, " (med ", jd_median_HSC, ") | ",
      "Overlap: ", overlap_start, "–", overlap_end,
      " (w=", round(overlap_width, 1), ")"
    )
  )

# ============================================================
# B) Bias test per ground × JD_bin  (uses your robust fit_bias_gam)
# ============================================================
bias_table <- dat %>%
  group_by(ground, JD_bin) %>%
  group_modify(~ fit_bias_gam(.x, k_cap = 6)) %>%
  ungroup() %>%
  mutate(
    p_adj_BH = p.adjust(p_value, method = "BH"),
    bias_flag_p05 = !is.na(p_value) & p_value < 0.05
  )

# ============================================================
# C) MERGE into one appendix table
# ============================================================
appendix_table <- summ_wide %>%
  left_join(bias_table, by = c("ground", "JD_bin")) %>%
  arrange(ground, JD_bin) %>%
  mutate(
    # light rounding for readability (optional)
    median_diff_HSC_minus_DFO = round(median_diff_HSC_minus_DFO, 1),
    overlap_width = round(overlap_width, 1),
    overlap_fraction = round(overlap_fraction, 3),
    dev_expl = round(dev_expl, 3),
    p_value = signif(p_value, 3),
    p_adj_BH = signif(p_adj_BH, 3)
  )

appendix_table
colnames(appendix_table)
# ============================================================
# D) EXPORT
# ============================================================

appendix_table_reduced <- appendix_table %>%
  select(
    ground,
    JD_bin,
    n_DFO,
    n_HSC,
    overlap_width,
    overlap_fraction,
    median_diff_HSC_minus_DFO,
    p_adj_BH,
    bias_flag_p05
  ) %>%
  arrange(ground, JD_bin)


write_csv(appendix_table_reduced, "Appendix_JDbin_bias_overlap_GAM_filtered.csv")








#### repeat with deep:

# assumes surface already has JD_bin
dat <- deep %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("Scots Bay", "German Bank"),
    !is.na(JulianDay),
    !is.na(JD_bin)
  ) %>%
  mutate(
    JD_bin = factor(JD_bin, levels = c("EarlySummer", "LateSummer", "Fall")),
    Source = factor(Source, levels = c("DFO", "HSC")),  # DFO baseline
    is_HSC = as.integer(Source == "HSC")
  )



### GAM-based bias test. Are my bins biased? models to test for seasonality bias within the bins

dat <- deep %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("Scots Bay", "German Bank"),
    !is.na(JulianDay),
    !is.na(JD_bin),
    # ---- REMOVE these two strata ----
    !(ground == "German Bank" & JD_bin == "EarlySummer"),
    !(ground == "Scots Bay"   & JD_bin == "Fall")
  ) %>%
  mutate(
    JD_bin = factor(JD_bin, levels = c("EarlySummer", "LateSummer", "Fall")),
    Source = factor(Source, levels = c("DFO", "HSC")),
    is_HSC = as.integer(Source == "HSC")
  )

bias_table <- dat %>%
  group_by(ground, JD_bin) %>%
  group_modify(~ fit_bias_gam(.x, k_cap = 6)) %>%
  ungroup() %>%
  mutate(
    p_adj_BH = p.adjust(p_value, method = "BH"),
    bias_flag_p05 = !is.na(p_value) & p_value < 0.05
  )

bias_table

dat %>%
  count(ground, JD_bin, Source) %>%
  arrange(ground, JD_bin, Source)





### appenidx export:
library(writexl)

# -------------------------
# Prep + EXCLUSIONS
# Remove: German Bank EarlySummer and Scots Bay Fall
# -------------------------
dat <- surface %>%
  filter(
    Source %in% c("HSC", "DFO"),
    ground %in% c("Scots Bay", "German Bank"),
    !is.na(JulianDay),
    !is.na(JD_bin),
    !(ground == "German Bank" & JD_bin == "EarlySummer"),
    !(ground == "Scots Bay"   & JD_bin == "Fall")
  ) %>%
  mutate(
    JD_bin = factor(JD_bin, levels = c("EarlySummer", "LateSummer", "Fall")),
    Source = factor(Source, levels = c("DFO", "HSC")),
    is_HSC = as.integer(Source == "HSC")
  )

# ============================================================
# A) OVERLAP + JulianDay summaries by Source within each ground × JD_bin
# ============================================================
summ_by_source <- dat %>%
  group_by(ground, JD_bin, Source) %>%
  summarise(
    n = n(),
    jd_min    = min(JulianDay),
    jd_q25    = quantile(JulianDay, 0.25, na.rm = TRUE),
    jd_median = median(JulianDay, na.rm = TRUE),
    jd_q75    = quantile(JulianDay, 0.75, na.rm = TRUE),
    jd_max    = max(JulianDay),
    .groups = "drop"
  )

summ_wide <- summ_by_source %>%
  pivot_wider(
    names_from = Source,
    values_from = c(n, jd_min, jd_q25, jd_median, jd_q75, jd_max),
    names_sep = "_"
  ) %>%
  mutate(
    # median sampling shift (HSC - DFO)
    median_diff_HSC_minus_DFO = jd_median_HSC - jd_median_DFO,
    
    # overlap of min/max ranges
    overlap_start = pmax(jd_min_HSC, jd_min_DFO, na.rm = TRUE),
    overlap_end   = pmin(jd_max_HSC, jd_max_DFO, na.rm = TRUE),
    overlap_width = pmax(0, overlap_end - overlap_start),
    
    # total combined span for scaling overlap
    combined_span = pmax(jd_max_HSC, jd_max_DFO, na.rm = TRUE) -
      pmin(jd_min_HSC, jd_min_DFO, na.rm = TRUE),
    
    overlap_fraction = ifelse(combined_span > 0, overlap_width / combined_span, NA_real_),
    
    # readable summary string for appendix
    window_summary = paste0(
      "DFO: ", jd_min_DFO, "–", jd_max_DFO, " (med ", jd_median_DFO, ") | ",
      "HSC: ", jd_min_HSC, "–", jd_max_HSC, " (med ", jd_median_HSC, ") | ",
      "Overlap: ", overlap_start, "–", overlap_end,
      " (w=", round(overlap_width, 1), ")"
    )
  )

# ============================================================
# B) Bias test per ground × JD_bin  (uses your robust fit_bias_gam)
# ============================================================
bias_table <- dat %>%
  group_by(ground, JD_bin) %>%
  group_modify(~ fit_bias_gam(.x, k_cap = 6)) %>%
  ungroup() %>%
  mutate(
    p_adj_BH = p.adjust(p_value, method = "BH"),
    bias_flag_p05 = !is.na(p_value) & p_value < 0.05
  )

# ============================================================
# C) MERGE into one appendix table
# ============================================================
appendix_table <- summ_wide %>%
  left_join(bias_table, by = c("ground", "JD_bin")) %>%
  arrange(ground, JD_bin) %>%
  mutate(
    # light rounding for readability (optional)
    median_diff_HSC_minus_DFO = round(median_diff_HSC_minus_DFO, 1),
    overlap_width = round(overlap_width, 1),
    overlap_fraction = round(overlap_fraction, 3),
    dev_expl = round(dev_expl, 3),
    p_value = signif(p_value, 3),
    p_adj_BH = signif(p_adj_BH, 3)
  )

appendix_table
colnames(appendix_table)
# ============================================================
# D) EXPORT
# ============================================================

appendix_table_reduced <- appendix_table %>%
  select(
    ground,
    JD_bin,
    n_DFO,
    n_HSC,
    overlap_width,
    overlap_fraction,
    median_diff_HSC_minus_DFO,
    p_adj_BH,
    bias_flag_p05
  ) %>%
  arrange(ground, JD_bin)


write_csv(appendix_table_reduced, "Appendix_JDbin_bias_overlap_GAM_filtered.csv")

























# adjust for julian day in late summer Scots Bay:


scots_late <- surface %>%
  filter(
    ground == "Scots Bay",
    JD_bin == "LateSummer",
    Source %in% c("DFO", "HSC"),
    !is.na(avgTemp1),
    !is.na(JulianDay)
  ) %>%
  mutate(Source = factor(Source, levels = c("DFO", "HSC")))

# GAM-adjusted comparison
m_sst_scots_late <- gam(
  avgTemp1 ~ Source + s(JulianDay, k = 6),
  data = scots_late,
  method = "REML"
)

summary(m_sst_scots_late)


write_csv(t, "adjustedScotsLatSumme.csv")

















### QC

names(surface)
unique(surface$ground)
unique(surface$Source)

# If JD_bin exists:
if ("JD_bin" %in% names(surface)) unique(surface$JD_bin)

anti_join(
  valid_bins,
  surface %>% distinct(ground, JD_bin),
  by = c("ground", "JD_bin")
)





#SBsurface=surface[which(surface$ground == "Scots Bay"), ]
#hist(SBsurface$avgTemp1)

#GBsurface=surface[which(surface$ground == "German Bank"), ]
#hist(GBsurface$avgTemp1)

# Surface density: Differences in surface density between HSC and DFO were assessed using Welch two‑sample t‑tests and Mann–Whitney U tests within the overlapping sampling period (Julian days 210–238). No significant differences were detected (Welch t‑test, p = 0.63; Mann–Whitney U test, p = 0.42), and effect sizes were small (Cohen’s d = 0.12).”

# surface temp: Conclusion (Scots Bay): No statistically significant difference in surface temperature (avgTemp1) between HSC and DFO in the overlap season (210–238).
# Conclusion (Scots Bay): After controlling for Julian day, no significant HSC vs DFO temperature difference.

## surface density "For German Bank, surface density differed significantly between HSC and DFO within the overlapping sampling period (Julian days 223–308). Welch two‑sample t‑tests (p < 10⁻⁷) and Mann–Whitney U tests (p < 10⁻⁸) both indicated higher densities in DFO observations. The magnitude of the difference was large (Cohen’s d = −1.15)

# temperature:
#Conclusion (German Bank): HSC temperatures are significantly higher than DFO by ~2.22 °C within the overlap season (223–308), with a very large effect size. [herringsci...epoint.com]
#Conclusion (German Bank): Even after controlling for Julian day, HSC is significantly warmer than DFO by ~2°C.
#Conclusion (German Bank): The HSC‑warmer‑than‑DFO result is consistent in both early and late bins, and the difference is large.






# GB at depth:

GBdepth=atDep[which(atDep$ground == "German Bank"), ]

str(GBdepth$JulianDay)
colnames(GBdepth)

GBdepth2 <- GBdepth %>%
  dplyr::filter(JulianDay >= 200, JulianDay <= 350)

ggplot(GBdepth2, aes(x = JulianDay, y = avgTemp, colour = Source)) +
  geom_text(
    aes(label = Year),
    position = position_jitter(width = 6, height = 0.25),
    size = 3   # ← reduce this
  ) +
  ggtitle("Temporal Distribution of Samples in German Bank - 30m")



# GB Stratification
GBstrat=strat[which(strat$ground == "German Bank"), ]

GBstrat2 <- GBstrat %>%
  dplyr::filter(JulianDay >= 200, JulianDay <= 350)


ggplot(GBstrat2, aes(x = JulianDay, y = strat_density, colour = Source)) +
  geom_text(
    aes(label = Year),
    position = position_jitter(width = 6, height = 0.25),
    size = 3   # ← reduce this
  ) +
  ggtitle("Temporal Distribution of Samples in German Bank - Stratification Index")











#pre 2010
#post 2010

SBsurfaceH <- SBsurface[as.numeric(as.character(SBsurface$Year)) < 2010, ]

SBsurfaceP <- SBsurface %>% dplyr::filter(Year > 2010)

hist(SBsurfaceH$avgTemp1)
hist(SBsurfaceP$avgTemp1)


unique(Oceans$datatype)




######################################


CTD = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/Larval Data/CTD 30m.csv"))

CTD = read_csv(paste0("C:/Users/", Sys.info()[7],"/Documents/GitHub/HerringScience.github.io/Source Data/CTD_Raw.csv"))




##### cont here:


### Sounds speed function
sound_speed_chen_millero <- function(T, S, z = 0) {
  # Pure water sound speed
  c_w <- 1402.388 +
    5.03830 * T -
    5.81090e-2 * T^2 +
    3.3432e-4 * T^3 -
    1.47797e-6 * T^4 +
    3.1419e-9 * T^5
  
  # Salinity correction
  A <- 1.389 +
    1.262e-2 * T +
    7.166e-5 * T^2 +
    2.008e-6 * T^3 -
    3.21e-8 * T^4
  
  B <- 9.4742e-5 -
    1.2580e-5 * T -
    6.4885e-8 * T^2 +
    1.0507e-8 * T^3 -
    2.0122e-10 * T^4
  
  C <- -1.922e-2 - 4.42e-5 * T
  
  c_s <- A * S + B * S^(3/2) + C * S^2
  
  # Pressure/depth correction
  D <- 1.727e-3 - 7.9836e-6 * T
  c_p <- D * z
  
  c_w + c_s + c_p
}



CTD$SoundSpeed = sound_speed_chen_millero(CTD$Temperature, CTD$Salinity, z=30)




CTDSB=CTD[which(CTD$Ground == "Scots Bay"), ]
CTDGB=CTD[which(CTD$Ground == "German Bank"), ]

# shapiro-wilk normality test
# p>0.05 Normal distribution
# p<0.05 Doesn't have a normal distribution

shapiro.test(CTDSB$StratTemp)
shapiro.test(CTDGB$StratTemp)

hist(CTDSB$StratTemp)
mean(CTDSB$StratTemp)
hist(CTDGB$StratTemp)
mean(CTDGB$StratTemp)


# Non parametric alternative to t-test - ranked sum - independent samples
# p > 0.05 : No significant difference
# p < 0.05 : Signficant differences

wilcox.test(CTDSB$StratTemp, CTDGB$StratTemp)

#significantly different

dim(CTDGB)
dim(CTDSB)

## SST
shapiro.test(CTDSB$SST)
shapiro.test(CTDGB$SST)

hist(CTDSB$SST)
mean(CTDSB$SST)
hist(CTDGB$SST)
mean(CTDGB$SST)

wilcox.test(CTDSB$SST, CTDGB$SST)
# significantly different


## Temp at depth (Temperature)


shapiro.test(CTDSB$Temperature)
shapiro.test(CTDGB$Temperature)

hist(CTDSB$Temperature)
hist(CTDGB$Temperature)

wilcox.test(CTDSB$Temperature, CTDGB$Temperature)
# NOT significantly different


# Sounds Speed
head(CTD)

hist(CTDSB$SoundSpeed)
hist(CTDGB$SoundSpeed)

shapiro.test(CTDSB$SoundSpeed)
shapiro.test(CTDGB$SoundSpeed)

wilcox.test(CTDSB$SoundSpeed, CTDGB$SoundSpeed)
# no statistical difference!




### Salinity
shapiro.test(CTDSB$SurfaceSalinity)
shapiro.test(CTDGB$SurfaceSalinity)

#these are normally distributed
# classical F-test for equal variances
var.test(CTDSB$SurfaceSalinity, CTDGB$SurfaceSalinity)   
# variances are equal enough for parametric testing

t.test(CTDSB$SurfaceSalinity, CTDGB$SurfaceSalinity, var.equal = TRUE)
# NOT singificantly different
hist(CTDSB$SurfaceSalinity)
hist(CTDGB$SurfaceSalinity)


