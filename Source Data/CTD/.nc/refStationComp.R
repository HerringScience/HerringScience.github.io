

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


# scripts used to create prince5 is Prince5.R
# for HSC is CTD2026 and build_Oceans_df.R


## Take monthly_cast_averages (Prince 5) and compare with the same from HSC data, plot together to look at modern comparison

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/.nc")

# load Prince 5 data
prince5 <- readRDS("monthly_cast_averages.rds")
head(prince5)


# Load HSC data
HSC = readRDS("Oceans.rds")

head(prince5)
head(HSC)


prince5$min_depth
prince5$max_depth

# HSC is in a raw format, edit for monthly averages:
# create month in HSC

head(HSC)
HSC$month = month(HSC$Date)
min(HSC$Depth)
max(HSC$Depth)
unique(HSC$month)


# Only select HSC data (not DFO historical for now)
HSC=HSC[which(HSC$Source == "HSC"), ]
unique(HSC$Year)

head(HSC)

averagesHSC <- HSC %>%
  group_by(id, Date, ground, Lat, Lon) %>%
  summarise(
    mean_temperature = mean(Temperature, na.rm = TRUE),
    mean_salinity = mean(Salinity, na.rm = TRUE),
    min_temperature = min(Temperature, na.rm = TRUE),
    max_temperature = max(Temperature, na.rm = TRUE),
    stratification = max_temperature - min_temperature,
    mean_density = mean(Density, na.rm = TRUE),
    min_density = min(Density, na.rm=TRUE),
    max_density = max(Density, na.rm = TRUE),
    density_stratification = max_density - min_density,
    min_depth = min(Depth, na.rm = TRUE),
    max_depth = max(Depth, na.rm = TRUE),
    n_depths = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Year = as.numeric(format(Date, "%Y")),
    month = as.numeric(format(Date, "%m")),
    month_name = format(Date, "%B")
  ) %>%
  group_by(Year, month, month_name, ground) %>%
  summarise(
    monthly_mean_temperature = mean(mean_temperature, na.rm = TRUE),
    monthly_sd_temperature = sd(mean_temperature, na.rm = TRUE),
    monthly_se_temperature = monthly_sd_temperature / sqrt(sum(!is.na(mean_temperature))),
    
    monthly_mean_density = mean(mean_density, na.rm = TRUE),
    monthly_sd_density = sd(mean_density, na.rm = TRUE),
    monthly_se_density = monthly_sd_density / sqrt(sum(!is.na(mean_density))),
    
    monthly_mean_salinity = mean(mean_salinity, na.rm = TRUE),
    monthly_sd_salinity = sd(mean_salinity, na.rm = TRUE),
    monthly_se_salinity = monthly_sd_salinity / sqrt(sum(!is.na(mean_salinity))),
    
    monthly_mean_stratification = mean(stratification, na.rm = TRUE),
    monthly_sd_stratification = sd(stratification, na.rm = TRUE),
    monthly_se_stratification = monthly_sd_stratification / sqrt(sum(!is.na(stratification))),
    
    monthly_mean_density_stratification = mean(density_stratification, na.rm = TRUE),
    monthly_sd_density_stratification = sd(density_stratification, na.rm = TRUE),
    monthly_se_density_stratification =monthly_sd_density_stratification / sqrt(sum(!is.na(density_stratification))), 
    
    n_casts = n(),
    mean_latitude = mean(Lat, na.rm = TRUE),
    mean_longitude = mean(Lon, na.rm = TRUE),
    min_depth = min(min_depth, na.rm = TRUE),
    max_depth = max(max_depth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, month)

### Now export a copy to view:
write.csv(averagesHSC, "averagesHSC.csv", row.names = FALSE)




########## Now plot together the monthly averages:

# --------------------------------------------------
# 1. Standardize HSC monthly averages
# --------------------------------------------------

hsc_plot <- averagesHSC %>%
  mutate(
    Source = paste0("HSC - ", ground),
    Year = as.numeric(Year),
    month = as.numeric(month)
  ) %>%
  select(
    Year,
    month,
    month_name,
    Source,
    monthly_mean_temperature,
    monthly_mean_salinity,
    monthly_mean_stratification,
    monthly_mean_density_stratification,
    monthly_mean_density,
    n_casts,
    mean_latitude,
    mean_longitude
  )


# --------------------------------------------------
# 2. Standardize Prince-5 monthly averages
# --------------------------------------------------
# IMPORTANT:
# If your Prince-5 dataframe uses lowercase 'year',
# this renames it to 'Year'.
# If yours already has 'Year', this will still work.

prince5_plot <- monthly_cast_averages %>%
  rename(
    Year = any_of("year")
  ) %>%
  mutate(
    Source = "Prince-5",
    Year = as.numeric(Year),
    month = as.numeric(month),
    month_name = month.name[month]
  ) %>%
  select(
    Year,
    month,
    month_name,
    Source,
    monthly_mean_temperature,
    monthly_mean_salinity,
    monthly_mean_density,
    monthly_mean_stratification,
    monthly_mean_density_stratification,
    n_casts,
    mean_latitude,
    mean_longitude
  )


# --------------------------------------------------
# 3. Combine HSC and Prince-5
# --------------------------------------------------

combined_monthly <- bind_rows(hsc_plot, prince5_plot) %>%
  mutate(
    month_label = factor(
      month.abb[month],
      levels = month.abb
    ),
    Source = factor(Source)
  ) %>%
  arrange(Year, month, Source)


# --------------------------------------------------
# 4. Quick check
# --------------------------------------------------

print(combined_monthly)

table(combined_monthly$Year, combined_monthly$month, combined_monthly$Source)


### PLOTS
head(combined_monthly)

## Temperature:

          ggplot(combined_monthly,
                 aes(x = month_label,
                     y = monthly_mean_temperature,
                     color = Source,
                     group = Source)) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5) +
            facet_wrap(~ Year) +
            labs(
              title = "Monthly Mean Temperature: HSC Grounds vs Prince-5",
              x = "Month",
              y = "Monthly Mean Temperature",
              color = "Dataset / Ground"
            ) +
            theme_bw() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            )

## Salinity:
          
          ggplot(combined_monthly,
                 aes(x = month_label,
                     y = monthly_mean_salinity,
                     color = Source,
                     group = Source)) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5) +
            facet_wrap(~ Year) +
            labs(
              title = "Monthly Mean Salinity: HSC Grounds vs Prince-5",
              x = "Month",
              y = "Monthly Mean Salinity",
              color = "Dataset / Ground"
            ) +
            theme_bw() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            )

### Density
          ggplot(combined_monthly,
                 aes(x = month_label,
                     y = monthly_mean_density,
                     color = Source,
                     group = Source)) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5) +
            facet_wrap(~ Year) +
            labs(
              title = "Monthly Mean Density: HSC Grounds vs Prince-5",
              x = "Month",
              y = "Monthly Mean Density",
              color = "Dataset / Ground"
            ) +
            theme_bw() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            )

## Temperature Stratification:
          
          ggplot(combined_monthly,
                 aes(x = month_label,
                     y = monthly_mean_stratification,
                     color = Source,
                     group = Source)) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5) +
            facet_wrap(~ Year) +
            labs(
              title = "Monthly Mean Stratification: HSC Grounds vs Prince-5",
              x = "Month",
              y = "Monthly Mean Stratification",
              color = "Dataset / Ground"
            ) +
            theme_bw() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            )
          
          
### Density strat:
          ggplot(combined_monthly,
                 aes(x = month_label,
                     y = monthly_mean_density_stratification,
                     color = Source,
                     group = Source)) +
            geom_line(linewidth = 1) +
            geom_point(size = 2.5) +
            facet_wrap(~ Year) +
            labs(
              title = "Monthly Mean Density Stratification: HSC Grounds vs Prince-5",
              x = "Month",
              y = "Monthly Mean Density Stratification",
              color = "Dataset / Ground"
            ) +
            theme_bw() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            )
          


###### Compare average monthly between grounds

# --------------------------------------------------
# 1. Prepare HSC monthly temperature data
# --------------------------------------------------
          head(averagesHSC)
          
          
          hsc_vars <- averagesHSC %>%
            mutate(
              Year = as.numeric(Year),
              month = as.numeric(month)
            ) %>%
            select(
              Year,
              month,
              ground,
              
              HSC_temp = monthly_mean_temperature,
              HSC_temp_se = monthly_se_temperature,
              
              HSC_sal = monthly_mean_salinity,
              HSC_sal_se = monthly_se_salinity,
              
              HSC_den = monthly_mean_density,
              HSC_den_se = monthly_se_density,
              
              HSC_strat = monthly_mean_stratification,
              HSC_strat_se = monthly_se_stratification,
              
              HSC_Dstrat = monthly_mean_density_stratification,
              HSC_Dstrat_se = monthly_se_density_stratification
              
            )

# --------------------------------------------------
# 2. Prepare Prince-5 monthly temperature data
# --------------------------------------------------
# This handles either 'year' or 'Year' in monthly_cast_averages

          prince5_vars <- monthly_cast_averages %>%
            mutate(
              Year = as.numeric(year),
              month = as.numeric(month)
            ) %>%
            select(
              Year,
              month,
              
              Prince5_temp = monthly_mean_temperature,
              Prince5_temp_se = monthly_se_temperature,
              
              Prince5_sal = monthly_mean_salinity,
              Prince5_sal_se = monthly_se_salinity,
              
              Prince5_den = monthly_mean_density,
              Prince5_den_se = monthly_se_density,
              
              Prince5_strat = monthly_mean_stratification,
              Prince5_strat_se = monthly_se_stratification,
              
              Prince5_Dstrat = monthly_mean_density_stratification,
              Prince5_Dstrat_se = monthly_se_density_stratification
            )

# --------------------------------------------------
# 3. Join HSC grounds to Prince-5 by Year + month
# --------------------------------------------------
# This keeps only months where that HSC ground AND Prince-5 both exist.

          comparison <- hsc_vars %>%
            inner_join(prince5_vars, by = c("Year", "month")) %>%
            mutate(
              # Temperature difference
              temp_diff = HSC_temp - Prince5_temp,
              temp_abs_diff = abs(temp_diff),
              temp_diff_se = sqrt(HSC_temp_se^2 + Prince5_temp_se^2),
              
              # Salinity difference
              sal_diff = HSC_sal - Prince5_sal,
              sal_abs_diff = abs(sal_diff),
              sal_diff_se = sqrt(HSC_sal_se^2 + Prince5_sal_se^2),
              
              # Density difference
              den_diff = HSC_den - Prince5_den,
              den_abs_diff = abs(den_diff),
              den_diff_se = sqrt(HSC_den_se^2 + Prince5_den_se^2),
              
              # Stratification difference
              strat_diff = HSC_strat - Prince5_strat,
              strat_abs_diff = abs(strat_diff),
              strat_diff_se = sqrt(HSC_strat_se^2 + Prince5_strat_se^2),
              
              # Density Stratification difference
              Dstrat_diff = HSC_Dstrat - Prince5_Dstrat,
              Dstrat_abs_diff = abs(Dstrat_diff),
              Dstrat_diff_se = sqrt(HSC_Dstrat_se^2 + Prince5_Dstrat_se^2),
        
              month_name = month.name[month]
            ) %>%
            arrange(ground, Year, month)
          
# View matched monthly comparisons
print(comparison)

stats_summary <- comparison %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    
    # -----------------------------
    # Temperature stats
    # -----------------------------
    mean_HSC_temp = mean(HSC_temp, na.rm = TRUE),
    mean_Prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    
    mean_temp_difference_HSC_minus_Prince5 = mean(temp_diff, na.rm = TRUE),
    median_temp_difference = median(temp_diff, na.rm = TRUE),
    sd_temp_difference = sd(temp_diff, na.rm = TRUE),
    mean_absolute_temp_difference = mean(temp_abs_diff, na.rm = TRUE),
    temp_RMSE = sqrt(mean(temp_diff^2, na.rm = TRUE)),
    min_temp_difference = min(temp_diff, na.rm = TRUE),
    max_temp_difference = max(temp_diff, na.rm = TRUE),
    temp_correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs"),
    
    # -----------------------------
    # Salinity stats
    # -----------------------------
    mean_HSC_sal = mean(HSC_sal, na.rm = TRUE),
    mean_Prince5_sal = mean(Prince5_sal, na.rm = TRUE),
    
    mean_sal_difference_HSC_minus_Prince5 = mean(sal_diff, na.rm = TRUE),
    median_sal_difference = median(sal_diff, na.rm = TRUE),
    sd_sal_difference = sd(sal_diff, na.rm = TRUE),
    mean_absolute_sal_difference = mean(sal_abs_diff, na.rm = TRUE),
    sal_RMSE = sqrt(mean(sal_diff^2, na.rm = TRUE)),
    min_sal_difference = min(sal_diff, na.rm = TRUE),
    max_sal_difference = max(sal_diff, na.rm = TRUE),
    sal_correlation = cor(HSC_sal, Prince5_sal, use = "complete.obs"),
    
    # -----------------------------
    # Stratification stats
    # -----------------------------
    mean_HSC_strat = mean(HSC_strat, na.rm = TRUE),
    mean_Prince5_strat = mean(Prince5_strat, na.rm = TRUE),
    
    mean_strat_difference_HSC_minus_Prince5 = mean(strat_diff, na.rm = TRUE),
    median_strat_difference = median(strat_diff, na.rm = TRUE),
    sd_strat_difference = sd(strat_diff, na.rm = TRUE),
    mean_absolute_strat_difference = mean(strat_abs_diff, na.rm = TRUE),
    strat_RMSE = sqrt(mean(strat_diff^2, na.rm = TRUE)),
    min_strat_difference = min(strat_diff, na.rm = TRUE),
    max_strat_difference = max(strat_diff, na.rm = TRUE),
    strat_correlation = cor(HSC_strat, Prince5_strat, use = "complete.obs"),
    
    .groups = "drop"
  )

print(stats_summary)
View(stats_summary)


### STATS

## Q: are the monthly values at this ground statistically different from Prince 5 when matched by year and month? (t-test and wilcoxon)


          paired_tests <- comparison %>%
            group_by(ground) %>%
            summarise(
              n_matched_months = n(),
              
              # -----------------------------
              # Temperature tests
              # -----------------------------
              temp_paired_t_test_p_value = ifelse(
                n() >= 2,
                t.test(HSC_temp, Prince5_temp, paired = TRUE)$p.value,
                NA
              ),
              
              temp_wilcoxon_p_value = ifelse(
                n() >= 2,
                wilcox.test(HSC_temp, Prince5_temp, paired = TRUE, exact = FALSE)$p.value,
                NA
              ),
              
              ### Neither ground is statistically significant different from Prince 5 in terms of temperature. Temperature is well represented by Prince 5.
              
              
              # -----------------------------
              # Salinity tests
              # -----------------------------
              sal_paired_t_test_p_value = ifelse(
                n() >= 2,
                t.test(HSC_sal, Prince5_sal, paired = TRUE)$p.value,
                NA
              ),
              
              sal_wilcoxon_p_value = ifelse(
                n() >= 2,
                wilcox.test(HSC_sal, Prince5_sal, paired = TRUE, exact = FALSE)$p.value,
                NA
              ),
              
              ##### Salinity is also well represented by Prince 5 (not statistically significant)
              
              ## density TESTS
              
              den_paired_t_test_p_value = ifelse(
                n() >= 2,
                t.test(HSC_den, Prince5_den, paired = TRUE)$p.value,
                NA
              ),
              
              den_wilcoxon_p_value = ifelse(
                n() >= 2,
                wilcox.test(HSC_den, Prince5_den, paired = TRUE, exact = FALSE)$p.value,
                NA
              ),
              
              
              
              # -----------------------------
              # Stratification tests
              # -----------------------------
              strat_paired_t_test_p_value = ifelse(
                n() >= 2,
                t.test(HSC_strat, Prince5_strat, paired = TRUE)$p.value,
                NA
              ),
              
              strat_wilcoxon_p_value = ifelse(
                n() >= 2,
                wilcox.test(HSC_strat, Prince5_strat, paired = TRUE, exact = FALSE)$p.value,
                NA
              ),
              
              
      ### Density stratification:
      Dstrat_paired_t_test_p_value = ifelse(
        n() >= 2,
        t.test(HSC_Dstrat, Prince5_Dstrat, paired = TRUE)$p.value,
        NA
      ),
      
      Dstrat_wilcoxon_p_value = ifelse(
        n() >= 2,
        wilcox.test(HSC_Dstrat, Prince5_Dstrat, paired = TRUE, exact = FALSE)$p.value,
        NA
      ),
      
      
              
              .groups = "drop"
            )
          
            #### German Bank IS statistically different than Prince 5 for stratification
                #### Scots Bay is not different
          
          print(paired_tests)
          View(paired_tests)
          
## Key Result
          
          #The sources/datasets seem broadly comparable for average temperature, salinity, and density, but they may differ in how well they capture vertical structure/stratification, especially on German Bank.
          
          #In Scots Bay, mean environmental conditions appear comparable between paired observations. Evidence for differences in stratification is weaker than on German Bank, although the Wilcoxon test suggests that density stratification may differ between sources.
          
       ###Paired comparisons were conducted using matched year-month observations for German Bank and Scots Bay. Across both grounds, monthly mean temperature, salinity, and density did not differ significantly between paired observations, with p-values consistently above 0.05 for both paired t-tests and Wilcoxon signed-rank tests. This suggests that average hydrographic conditions were broadly comparable between sources for matched months.
          #In contrast, stratification metrics showed stronger evidence of source-related differences, particularly on German Bank. At German Bank, both temperature stratification and density stratification differed significantly between paired observations, with highly significant results from both paired t-tests and Wilcoxon tests. This indicates that although average conditions were similar, the vertical structure of the water column differed between sources.
          #For Scots Bay, there was no significant difference in temperature stratification, and evidence for density stratification differences was weaker. The paired t-test for density stratification was not significant, but the Wilcoxon test was significant, suggesting a possible non-normal or skewed pattern in paired differences. Overall, Scots Bay showed less evidence of source-related differences than German Bank.
          #These results suggest that source comparisons are more robust for monthly mean temperature, salinity, and density than for stratification metrics. Stratification should be interpreted cautiously, particularly for German Bank, because it is likely more sensitive to differences in cast depth, vertical resolution, and sampling coverage.
          
          
          
          
          scots_vs_prince5 <- comparison %>%
            filter(ground == "Scots Bay")
          
          german_vs_prince5 <- comparison %>%
            filter(ground == "German Bank")
          
          print(scots_vs_prince5)
          print(german_vs_prince5)
          




#### test seasonality versus ground effect:
          ## is there a true ground effect or season effect?

comparison_summer <- comparison %>%
  filter(month %in% c(7, 8, 9))  # July–September

# same as above, just using overlapping months
paired_tests <- comparison_summer %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    
    # -----------------------------
    # Temperature tests
    # -----------------------------
    temp_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_temp, Prince5_temp, paired = TRUE)$p.value,
      NA
    ),
    
    temp_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_temp, Prince5_temp, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    
    # -----------------------------
    # Salinity tests
    # -----------------------------
    sal_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_sal, Prince5_sal, paired = TRUE)$p.value,
      NA
    ),
    
    sal_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_sal, Prince5_sal, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    
    # -----------------------------
    # Stratification tests
    # -----------------------------
    strat_paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_strat, Prince5_strat, paired = TRUE)$p.value,
      NA
    ),
    
    strat_wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_strat, Prince5_strat, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    .groups = "drop"
  )


view(paired_tests)



#### There is a real ground effect in stratification at German Bank relative to Prince‑5


##Differences in stratification between Prince‑5 and German Bank persist even when controlling for seasonal effects, indicating that these differences reflect genuine spatial variability rather than sampling timing alone.










#### Descriptive performance metrics - not hypotheses tests
### They answer - How close is Prince 5 to this ground on average? Or how big is the difference?
# mean difference - average bias, directional difference
  # median difference - less biased version of above, average bias (less influence of outliers)
    # Error Size: 1. mean_absolute_difference and 2. RMSE
      # 1. average magnitude of difference (ignores sign)
        # 2. like mean absolute error but penalizes large error more
            # RMSE is the best single 'how different are they metric'
      # sd_difference - how consistent the differences are
    # Relationship: correlation, do the time series move together? Do they track each other?
        # high correlation and low RMSE = strong representativeness

stats_summary <- comparison %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    
    # -----------------------------
    # Temperature
    # -----------------------------
    mean_HSC_temp = mean(HSC_temp, na.rm = TRUE),
    mean_Prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    mean_temp_difference = mean(temp_diff, na.rm = TRUE),
    median_temp_difference = median(temp_diff, na.rm = TRUE),
    sd_temp_difference = sd(temp_diff, na.rm = TRUE),
    mean_abs_temp_difference = mean(temp_abs_diff, na.rm = TRUE),
    temp_RMSE = sqrt(mean(temp_diff^2, na.rm = TRUE)),
    temp_correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs"),
    
    # -----------------------------
    # Salinity
    # -----------------------------
    mean_HSC_sal = mean(HSC_sal, na.rm = TRUE),
    mean_Prince5_sal = mean(Prince5_sal, na.rm = TRUE),
    mean_sal_difference = mean(sal_diff, na.rm = TRUE),
    median_sal_difference = median(sal_diff, na.rm = TRUE),
    sd_sal_difference = sd(sal_diff, na.rm = TRUE),
    mean_abs_sal_difference = mean(sal_abs_diff, na.rm = TRUE),
    sal_RMSE = sqrt(mean(sal_diff^2, na.rm = TRUE)),
    sal_correlation = cor(HSC_sal, Prince5_sal, use = "complete.obs"),
    
    # -----------------------------
    # Stratification
    # -----------------------------
    mean_HSC_strat = mean(HSC_strat, na.rm = TRUE),
    mean_Prince5_strat = mean(Prince5_strat, na.rm = TRUE),
    mean_strat_difference = mean(strat_diff, na.rm = TRUE),
    median_strat_difference = median(strat_diff, na.rm = TRUE),
    sd_strat_difference = sd(strat_diff, na.rm = TRUE),
    mean_abs_strat_difference = mean(strat_abs_diff, na.rm = TRUE),
    strat_RMSE = sqrt(mean(strat_diff^2, na.rm = TRUE)),
    strat_correlation = cor(HSC_strat, Prince5_strat, use = "complete.obs"),
    
    .groups = "drop"
  )

view(stats_summary)

#Across both German Bank and Scots Bay, Prince‑5 reproduces temperature and salinity patterns well, but shows weaker agreement for stratification at German Bank, indicating that vertical structure at that site differs from Prince‑5 despite similar broader hydrographic conditions.


getwd()
### PLOTS monthly differences

library(grid)

comparison_long <- comparison %>%
  select(
    Year,
    month,
    ground,
    temp_diff,
    sal_diff,
    strat_diff
  ) %>%
  pivot_longer(
    cols = c(temp_diff, sal_diff, strat_diff),
    names_to = "variable",
    values_to = "difference"
  ) %>%
  mutate(
    variable = recode(
      variable,
      temp_diff = "Temperature",
      sal_diff = "Salinity",
      strat_diff = "Stratification"
    )
  )

ggplot(comparison_long,
       aes(x = month,
           y = difference,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.65) +
  facet_grid(variable ~ Year, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Differences: HSC Grounds minus Prince-5",
    subtitle = "Positive values mean HSC ground values are greater than Prince-5",
    x = "Month",
    y = "Difference",
    color = "Ground"
  ) + theme_bw() +
  theme(
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10),
    panel.spacing = unit(1, "lines")
  )

### temp plots with error bars:
ggplot(comparison,
       aes(x = month,
           y = temp_diff,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(
    aes(
      ymin = temp_diff - temp_diff_se,
      ymax = temp_diff + temp_diff_se
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 4) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Temperature Difference: HSC Grounds minus Prince-5",
    subtitle = "Error bars show ±1 SE of the difference; positive values mean HSC ground is warmer than Prince-5",
    x = "Month",
    y = "Temperature Difference (°C)",
    color = "Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10)
  )

#### error bars for stratification:
ggplot(comparison,
       aes(x = month,
           y = strat_diff,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(
    aes(
      ymin = strat_diff - strat_diff_se,
      ymax = strat_diff + strat_diff_se
    ),
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 4) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Stratification Difference: HSC Grounds minus Prince-5",
    subtitle = "Error bars show ±1 SE of the difference; positive values mean HSC ground is more stratified than Prince-5",
    x = "Month",
    y = "Stratification Difference",
    color = "Ground"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10)
  )

















# save the stats:
dir.create("Casts/HSC_Prince5_Comparison", showWarnings = FALSE, recursive = TRUE)

write.csv(
  temp_compare,
  "Casts/HSC_Prince5_Comparison/monthly_temperature_matched_comparisons.csv",
  row.names = FALSE
)

write.csv(
  temp_stats,
  "Casts/HSC_Prince5_Comparison/monthly_temperature_summary_stats.csv",
  row.names = FALSE
)

write.csv(
  paired_tests,
  "Casts/HSC_Prince5_Comparison/monthly_temperature_paired_tests.csv",
  row.names = FALSE
)




############# text for document:
# Scots Bay monthly temperatures were compared with Prince-5 using matched year-month observations available for Scots Bay, primarily June–September. German Bank monthly temperatures were compared with Prince-5 using matched year-month observations available for German Bank, primarily August–November.


# Ground specific comparison to Prince 5 using all available matched months
temp_stats_all_available <- temp_compare %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    months_sampled = paste(sort(unique(month_name)), collapse = ", "),
    
    mean_HSC_temp = mean(HSC_temp, na.rm = TRUE),
    mean_Prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    
    mean_difference_HSC_minus_Prince5 = mean(temp_difference, na.rm = TRUE),
    median_difference = median(temp_difference, na.rm = TRUE),
    sd_difference = sd(temp_difference, na.rm = TRUE),
    mean_absolute_difference = mean(abs_difference, na.rm = TRUE),
    RMSE = sqrt(mean(temp_difference^2, na.rm = TRUE)),
    
    .groups = "drop"
  )

print(temp_stats_all_available)




############### Clean minimla summary code STATS:
representativeness_stats <- temp_compare %>%
  group_by(ground) %>%
  summarise(
    n_months = n(),
    
    mean_bias = mean(temp_difference, na.rm = TRUE),
    mean_absolute_error = mean(abs_difference, na.rm = TRUE),
    RMSE = sqrt(mean(temp_difference^2, na.rm = TRUE)),
    
    correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs"),
    
    min_diff = min(temp_difference, na.rm = TRUE),
    max_diff = max(temp_difference, na.rm = TRUE),
    
    .groups = "drop"
  )

print(representativeness_stats)

### Conclusions:

## Scots Bay, 27 month matches, Scots Bay is on average only 0.2 degrees warmer than Prince 5. Prince 5 is typically off by 0.6 degrees. Overall error is relatively low (0.744). Very strong seasonal/monthly tracking (0.935). Sometimes Scots Bay is cooler, sometimes it is warmer.
# Prince 5 is a strong temperature proxy for Scots Bay. They are not systematically different, they move very closely over time and typical monthly differnce is less than a degree.
#Prince‑5 appears to represent average monthly temperature conditions in Scots Bay reasonably well, with very small average bias and strong temporal agreement.
# Is Prince 5 representative? Yes, fairly strong.




## German Bank, 19 month matches. German Bank is 0.35 degrees cooler than Prince 5 on average. Prince 5 is typically off by 0.9 degrees. Overall error is higher (1.14). Weak tracking (0.102). German Bank can be about 2 degrees warmer or cooler. Prince‑5 does not appear to represent German Bank as reliably but the mean bias is not huge, -0.353. So on average, Prince‑5 is only about 0.35°C warmer than German Bank. The bigger issue is the correlation (0.102). That means Prince‑5 and German Bank are not tracking each other well month-to-month/year-to-year, and the error is larger (0.929, 1.14). 
#Prince‑5 shows only weak representativeness for German Bank monthly average temperatures. Although the average bias is modest, the low correlation and higher error indicate that Prince‑5 does not reliably track German Bank temperature variability.
# Is Prince 5 representative? Only weakly/cautiously. It may approximate the average temperature sometimes but it does not track German Bank well. Mean bias is ok - but it can hide the wide flucatations, 1.74 to -2.22



#Overall:
##Prince‑5 was more representative of Scots Bay than German Bank for monthly average temperature. Scots Bay showed a small average temperature difference from Prince‑5 (+0.21°C), low mean absolute error (0.58°C), and strong correlation (r = 0.94), indicating that Prince‑5 closely tracked Scots Bay monthly temperatures. German Bank had a modest average bias relative to Prince‑5 (-0.35°C), but higher error (MAE = 0.93°C; RMSE = 1.14°C) and very weak correlation (r = 0.10), suggesting Prince‑5 does not reliably capture German Bank monthly temperature variability.


### take away:My recommendation
#For your conclusion, I would separate average bias from representativeness:
  
#Scots Bay: low bias and strong representativeness
#German Bank: modest average bias but poor representativeness/tracking

#That distinction is really important.


ggplot(temp_compare,
       aes(x = Prince5_temp,
           y = HSC_temp,
           color = ground)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "grey40") +
  facet_wrap(~ ground) +
  labs(
    title = "Monthly Average Temperature: HSC Grounds vs Prince-5",
    subtitle = "Dashed line shows perfect 1:1 agreement",
    x = "Prince-5 monthly mean temperature (°C)",
    y = "HSC ground monthly mean temperature (°C)",
    color = "Ground"
  ) +
  theme_bw()



#### Look monthly

ggplot(temp_compare,
       aes(x = month,
           y = temp_difference,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "grey40") +
  geom_point(size = 3, alpha = 0.8)  +
  facet_wrap(~ ground) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Temperature Bias: HSC Grounds minus Prince-5",
    subtitle = "Positive values mean the HSC ground was warmer than Prince-5",
    x = "Month",
    y = "Temperature difference (°C)",
    color = "Ground"
  ) +
  theme_bw()


# by year and month:

ggplot(temp_compare,
       aes(x = month,
           y = temp_difference,
           color = ground,
           group = interaction(ground, Year))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "grey40") +
  geom_line(alpha = 0.6) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Temperature Difference by Year",
    subtitle = "HSC ground monthly average minus Prince-5 monthly average",
    x = "Month",
    y = "Temperature difference (°C)",
    color = "Ground"
  ) +
  theme_bw()


### Look at error:

library(tidyr)

rep_stats_long <- representativeness_stats %>%
  select(ground, mean_absolute_error, RMSE) %>%
  pivot_longer(
    cols = c(mean_absolute_error, RMSE),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      mean_absolute_error = "Mean absolute error",
      RMSE = "RMSE"
    )
  )

ggplot(rep_stats_long,
       aes(x = ground,
           y = value,
           fill = metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "Prince-5 Representativeness Error by Ground",
    subtitle = "Lower values indicate better agreement with HSC ground temperatures",
    x = "Ground",
    y = "Temperature error (°C)",
    fill = "Metric"
  ) +
  theme_bw()

## correlation bar plot:

ggplot(representativeness_stats,
       aes(x = ground,
           y = correlation,
           fill = ground)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0,
             color = "grey40") +
  ylim(-1, 1) +
  labs(
    title = "Correlation Between Prince-5 and HSC Ground Temperatures",
    subtitle = "Higher values mean Prince-5 tracks monthly temperature patterns better",
    x = "Ground",
    y = "Correlation"
  ) +
  theme_bw() +
  theme(legend.position = "none")


### last one:
library(patchwork)
library(patchwork)

p1 <- ggplot(temp_compare,
             aes(x = Prince5_temp,
                 y = HSC_temp,
                 color = ground)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "grey40") +
  facet_wrap(~ ground) +
  labs(
    title = "A. Matched monthly temperatures",
    x = "Prince-5 temperature (°C)",
    y = "HSC ground temperature (°C)",
    color = "Ground"
  ) +
  theme_bw()

p2 <- ggplot(temp_compare,
             aes(x = month,
                 y = temp_difference,
                 color = ground,
                 group = ground)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "grey40") +
  geom_point(size = 3, alpha = 0.8)  +
  facet_wrap(~ ground) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "B. Monthly bias",
    x = "Month",
    y = "HSC minus Prince-5 (°C)",
    color = "Ground"
  ) +
  theme_bw()

p3 <- ggplot(rep_stats_long,
             aes(x = ground,
                 y = value,
                 fill = metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "C. Error metrics",
    x = "Ground",
    y = "Temperature error (°C)",
    fill = "Metric"
  ) +
  theme_bw()

combined_plot <- (p1 / p2 / p3)

combined_plot







## Which months matter most?

largest_temp_differences <- temp_compare %>%
  arrange(desc(abs_difference)) %>%
  select(
    ground,
    Year,
    month,
    month_name,
    HSC_temp,
    Prince5_temp,
    temp_difference,
    abs_difference
  )

print(largest_temp_differences)