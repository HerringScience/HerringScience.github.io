

## Take monthly_cast_averages (Prince 5) and compare with the same from HSC data, plot together

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/.nc")

# load Prince 5 data
prince5 <- readRDS("monthly_cast_averages.rds")

# Load HSC data
HSC = readRDS("Oceans.rds")

head(prince5)
head(HSC)


# HSC is in a raw format, edit for monthly averages:
# create month in HSC

head(HSC)
HSC$month = month(HSC$Date)
min(HSC$Depth)
max(HSC$Depth)
unique(HSC$month)

# why is there no June data from HSC?


# Only select HSC data (not DFO historical for now)
HSC=HSC[which(HSC$Source == "HSC"), ]
unique(HSC$Year)

averagesHSC <- HSC %>%
  group_by(id, Date, ground, Lat, Lon) %>%
  summarise(
    mean_temperature = mean(Temperature, na.rm = TRUE),
    mean_salinity = mean(Salinity, na.rm = TRUE),
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
    monthly_mean_salinity = mean(mean_salinity, na.rm = TRUE),
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

library(dplyr)
library(ggplot2)

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







###### Compare average monthly temperatures between grounds
library(dplyr)
library(tidyr)

# --------------------------------------------------
# 1. Prepare HSC monthly temperature data
# --------------------------------------------------

hsc_temp <- averagesHSC %>%
  mutate(
    Year = as.numeric(Year),
    month = as.numeric(month)
  ) %>%
  select(
    Year,
    month,
    ground,
    HSC_temp = monthly_mean_temperature
  )

# --------------------------------------------------
# 2. Prepare Prince-5 monthly temperature data
# --------------------------------------------------
# This handles either 'year' or 'Year' in monthly_cast_averages

prince5_temp <- monthly_cast_averages %>%
  rename(
    Year = any_of("year")
  ) %>%
  mutate(
    Year = as.numeric(Year),
    month = as.numeric(month)
  ) %>%
  select(
    Year,
    month,
    Prince5_temp = monthly_mean_temperature
  )

# --------------------------------------------------
# 3. Join HSC grounds to Prince-5 by Year + month
# --------------------------------------------------
# This keeps only months where that HSC ground AND Prince-5 both exist.

temp_compare <- hsc_temp %>%
  inner_join(prince5_temp, by = c("Year", "month")) %>%
  mutate(
    temp_difference = HSC_temp - Prince5_temp,
    abs_difference = abs(temp_difference),
    month_name = month.name[month]
  ) %>%
  arrange(ground, Year, month)


# View matched monthly comparisons
print(temp_compare)

temp_stats <- temp_compare %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    
    mean_HSC_temp = mean(HSC_temp, na.rm = TRUE),
    mean_Prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    
    mean_difference_HSC_minus_Prince5 = mean(temp_difference, na.rm = TRUE),
    median_difference = median(temp_difference, na.rm = TRUE),
    sd_difference = sd(temp_difference, na.rm = TRUE),
    
    mean_absolute_difference = mean(abs_difference, na.rm = TRUE),
    
    RMSE = sqrt(mean(temp_difference^2, na.rm = TRUE)),
    
    min_difference = min(temp_difference, na.rm = TRUE),
    max_difference = max(temp_difference, na.rm = TRUE),
    
    correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs"),
    
    .groups = "drop"
  )

print(temp_stats)

### STATS

paired_tests <- temp_compare %>%
  group_by(ground) %>%
  summarise(
    n_matched_months = n(),
    
    paired_t_test_p_value = ifelse(
      n() >= 2,
      t.test(HSC_temp, Prince5_temp, paired = TRUE)$p.value,
      NA
    ),
    
    wilcoxon_p_value = ifelse(
      n() >= 2,
      wilcox.test(HSC_temp, Prince5_temp, paired = TRUE, exact = FALSE)$p.value,
      NA
    ),
    
    .groups = "drop"
  )

print(paired_tests)


scots_vs_prince5 <- temp_compare %>%
  filter(ground == "Scots Bay")

german_vs_prince5 <- temp_compare %>%
  filter(ground == "German Bank")

print(scots_vs_prince5)
print(german_vs_prince5)


#### Explicit Stats
scots_stats <- scots_vs_prince5 %>%
  summarise(
    comparison = "Scots Bay vs Prince-5",
    n_matched_months = n(),
    mean_scots_temp = mean(HSC_temp, na.rm = TRUE),
    mean_prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    mean_difference_scots_minus_prince5 = mean(temp_difference, na.rm = TRUE),
    median_difference = median(temp_difference, na.rm = TRUE),
    sd_difference = sd(temp_difference, na.rm = TRUE),
    mean_absolute_difference = mean(abs_difference, na.rm = TRUE),
    RMSE = sqrt(mean(temp_difference^2, na.rm = TRUE)),
    correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs")
  )

german_stats <- german_vs_prince5 %>%
  summarise(
    comparison = "German Bank vs Prince-5",
    n_matched_months = n(),
    mean_german_temp = mean(HSC_temp, na.rm = TRUE),
    mean_prince5_temp = mean(Prince5_temp, na.rm = TRUE),
    mean_difference_german_minus_prince5 = mean(temp_difference, na.rm = TRUE),
    median_difference = median(temp_difference, na.rm = TRUE),
    sd_difference = sd(temp_difference, na.rm = TRUE),
    mean_absolute_difference = mean(abs_difference, na.rm = TRUE),
    RMSE = sqrt(mean(temp_difference^2, na.rm = TRUE)),
    correlation = cor(HSC_temp, Prince5_temp, use = "complete.obs")
  )

print(scots_stats)
print(german_stats)


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



### PLOTS monthly differences
library(ggplot2)

ggplot(temp_compare,
       aes(x = month,
           y = temp_difference,
           color = ground,
           group = ground)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Temperature Difference: HSC Grounds minus Prince-5",
    subtitle = "Positive values mean HSC ground is warmer than Prince-5",
    x = "Month",
    y = "Temperature Difference (°C)",
    color = "Ground"
  ) +
  theme_bw()


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
