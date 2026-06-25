


### CTD Pipeline

# load Oceans from CTD2026
# TEst with GB, fall


### Oceans_filtered and BinSummary from GBhistComp.R

colnames(Oceans_filtered)


unique(Oceans$depth_bin)

# create averages:
Oceans_bin_summary <- Oceans_filtered %>%
  group_by(Source, id, ground, JD_bin, Year, depth_bin) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    Salinity = mean(Salinity, na.rm = TRUE),
    Density = mean(Density, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  # Keep only the two depth bins you want
  filter(depth_bin %in% c("[5,10)", "[25,30]")) %>%
  # Turn depth bins into columns
  tidyr::pivot_wider(
    names_from = depth_bin,
    values_from = c(Temperature, Salinity, Density)
  ) %>%
  # Create stratification variables
  mutate(
    Temp_strat = `Temperature_[5,10)`-`Temperature_[25,30]`,
    Sal_strat  = `Salinity_[5,10)`-`Salinity_[25,30]`,
    Den_strat  = `Density_[5,10)` - `Density_[25,30]`
    
  )

unique(Oceans_filtered$depth_bin)

                ## Stratifcation over time:
                ggplot(Oceans_bin_summary, aes(x = Year, y = Temp_strat)) +
                  geom_point(alpha = 0.4) +
                  geom_smooth(method = "lm", se = TRUE, colour = "blue") +
                  annotate(
                    "text", 
                    x = Inf, y = Inf, 
                    label = "DFO n = 8\nHSC n = 33",
                    hjust = 4, vjust = 2,
                    size = 4
                  ) +
                  labs(
                    title = "Temperature Stratification over Time on German Bank",
                    x = "Year",
                    y = "Temperature Difference (5–10m minus 25–30m)"
                  ) +
                  theme_minimal()


      ### Historical data looks too sparse. Only 8 DFO plots, 33 HSC
                          Oceans_bin_summary %>%
                            filter(Source == "DFO", !is.na(Temp_strat)) %>%
                            summarise(n_casts = n_distinct(id))
                          
                          Oceans_bin_summary %>%
                            filter(Source == "HSC", !is.na(Temp_strat)) %>%
                            summarise(n_casts = n_distinct(id))
                         

# number of obs per depth bin overall
table(Oceans_filtered$depth_bin, useNA = "ifany")

Oceans_annual <- Oceans_filtered %>%
  group_by(Source, Year, ground, JD_bin, depth_bin) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    Salinity = mean(Salinity, na.rm = TRUE),
    Density = mean(Density, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

ggplot(Oceans_annual, aes(x = Year, y = Temperature, color = Source)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_grid(JD_bin ~ depth_bin) +
  theme_bw()+theme(axis.text.x = element_text(size=4))

## cleaner plot option:

# Example: assumes your plotting df already contains:
# Year, depth_bin, Temperature, Source

p1 <- ggplot(Oceans_annual, aes(x = Year, y = Temperature, color = Source)) +
  geom_point(size = 2.8, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~ depth_bin, nrow = 1) +
  scale_color_manual(values = c("DFO" = "#D55E00", "HSC" = "#0072B2")) +
  labs(
    x = "Year",
    y = "Temperature (°C)",
    color = "Source",
    title = "Temperature by year and depth bin",
    subtitle = "German Bank, LateSummer_Fall"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = "grey40"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

p1

##################create time period visual
PlotDF2 <- Oceans_annual %>%
  mutate(
    Period = case_when(
      Source == "DFO" ~ "Historic",
      Source == "HSC" ~ "Modern"
    )
  )

p2 <- ggplot(PlotDF2, aes(x = Period, y = Temperature, fill = Period)) +
  geom_boxplot(width = 0.65, alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.8, size = 2) +
  facet_wrap(~ depth_bin, nrow = 1) +
  scale_fill_manual(values = c("Historic" = "#E69F00", "Modern" = "#56B4E9")) +
  labs(
    x = NULL,
    y = "Temperature (°C)",
    title = "Historic vs modern temperature by depth bin",
    subtitle = "German Bank, LateSummer_Fall"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = "grey40"),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

p2

###############another option
SummaryPeriod <- PlotDF2 %>%
  group_by(depth_bin, Period) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),
    sd_temp = sd(Temperature, na.rm = TRUE),
    n = n(),
    se_temp = sd_temp / sqrt(n),
    .groups = "drop"
  )


p3 <- ggplot(SummaryPeriod, aes(x = depth_bin, y = mean_temp, color = Period, group = Period)) +
  geom_point(size = 3, position = position_dodge(width = 0.25)) +
  geom_line(linewidth = 1, position = position_dodge(width = 0.25)) +
  geom_errorbar(
    aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp),
    width = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.25)
  ) +
  scale_color_manual(values = c("Historic" = "#D55E00", "Modern" = "#0072B2")) +
  labs(
    x = "Depth bin (m)",
    y = "Mean temperature (°C)",
    color = "Period",
    title = "Historic vs modern temperature profiles",
    subtitle = "German Bank, LateSummer_Fall"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p3



## or explictly states sample size:
p3 <- ggplot(SummaryPeriod, aes(x = depth_bin, y = mean_temp, color = Period, group = Period)) +
  geom_point(size = 3, position = position_dodge(width = 0.25)) +
  geom_line(linewidth = 1, position = position_dodge(width = 0.25)) +
  geom_errorbar(
    aes(ymin = mean_temp - se_temp, ymax = mean_temp + se_temp),
    width = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.25)
  ) +
  geom_text(
    aes(label = paste0("n=", n)),
    position = position_dodge(width = 0.25),
    vjust = -1,
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Historic" = "#D55E00", "Modern" = "#0072B2")) +
  labs(
    x = "Depth bin (m)",
    y = "Mean temperature (°C)",
    color = "Period",
    title = "Historic vs modern temperature profiles",
    subtitle = "German Bank, LateSummer_Fall"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p3



#### Analysis:
model <- lm(Temperature ~ Period + depth_bin, data = PlotDF2)
summary(model)
#Result:Modern (HSC) appears warmer by about 2.48°C But this ignores Julian day.



# key result is period effect: +2.07 degrees celcius, p<0.0001
### with the sensitivity analysis, only strong years (5 or more casts)
## same end result  - PeriodModern was highly significant (p<0.0001) with a difference of 2.06 degrees
## With the 5 cast/year, overall sampling size is reduced and the depth bins aren't significantly different.

plot(model)

# is warming different by depth:
lm(Temperature ~ Period * depth_bin, data = PlotDF2)














# data for salinity is sparse
ggplot(Oceans_annual, aes(x = Year, y = Salinity, color = Source)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_grid(JD_bin ~ depth_bin) +
  theme_bw()+theme(axis.text.x = element_text(size=4))

ggplot(Oceans_annual, aes(x = Year, y = Density, color = Source)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_grid(JD_bin ~ depth_bin) +
  theme_bw()+theme(axis.text.x = element_text(size=4))





##Temperatures in the modern period (HSC) are consistently higher than historical observations (DFO) across all depth bins, suggesting a warming signal of approximately 2°C.

###Although historical (DFO) estimates have relatively large uncertainty due to limited sampling, modern (HSC) temperatures remain consistently higher, with minimal overlap between the two periods. This provides evidence of a substantial warming signal.


head(Oceans_annual)
unique(Oceans_annual$ground)





#################### Combine last summer and fall:

OceansFall <- Oceans %>%
  filter(ground %in% c("German Bank"),
         JD_bin %in% c("LateSummer", "Fall")) %>%
  mutate(
    Season_combo = "LateSummer_Fall"
  )

Oceans_bin_summary <- OceansFall %>%
  group_by(Source, id, ground, Season_combo, Year, depth_bin) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    Salinity = mean(Salinity, na.rm = TRUE),
    Density = mean(Density, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )
Oceans_annual <- OceansFall %>%
  group_by(Source, Year, ground, Season_combo, depth_bin) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )
ggplot(Oceans_annual, aes(x = Year, y = Temperature, color = Source)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ depth_bin) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 4))+
labs(
  title = "German Bank: Late Summer + Fall (5–30 m)",
  subtitle = "Combined seasonal analysis",
  x = "Year",
  y = "Temperature"
)


## “German Bank appears warmer in the modern HSC period than in the earlier DFO period, especially in the 5–20 m layer.”
## last two bins are sparse for data (DFO) weaker comparison


Oceans_annual %>%
  group_by(Source, depth_bin) %>%
  summarise(
    min_obs = min(n_obs),
    max_obs = max(n_obs),
    mean_obs = mean(n_obs)
  )


# There appears to be a higher modern temperature level in HSC than in historical DFO, but the certainty of that comparison is limited by sparse historical support.

#The modern HSC dataset provides substantially more observations than the historical DFO dataset across all depth bins, so modern estimates are much more precise. Any apparent difference between historical and modern temperatures should therefore be interpreted cautiously, with particular attention to the limited support in the historical record.

ggplot(Oceans_annual, aes(x = Year, y = Temperature, color = Source, size = n_obs)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ depth_bin) +
  theme_bw()

valid_bins <- Oceans_annual %>%
  group_by(Source, depth_bin) %>%
  summarise(total_obs = sum(n_obs), .groups = "drop")


### look at seasonality
# Do not combine all bins - earlysummer is different
#EarlySummer temperatures are consistently higher than LateSummer and Fall across all depth bins at German Bank, indicating a distinct seasonal regime. LateSummer and Fall exhibit overlapping temperature distributions and can be reasonably combined for analysis.

plot_data <- Oceans %>%
  filter(
    ground == "German Bank",
    JD_bin %in% c("EarlySummer", "LateSummer", "Fall")
  ) %>%
  group_by(Source, id, ground, JD_bin, Year, depth_bin) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_data, aes(x = JD_bin, y = Temperature, fill = JD_bin)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(color = Source),
              width = 0.15, height = 0,
              alpha = 0.7, size = 2) +
  facet_grid(Source ~ depth_bin) +
  theme_bw() +
  labs(
    title = "German Bank seasonal temperatures by depth bin",
    subtitle = "Each point is a cast mean within a depth bin",
    x = "Season",
    y = "Temperature"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )


