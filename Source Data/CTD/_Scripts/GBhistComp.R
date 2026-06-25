
# QC visualizations for historic and modern German Bank latesummer_fall

# comparing HSC and DFO
# vertical resolution mismatch
# maximum depth problems 

# trim top 5m to control for surface noise caused by strong tides
  # likely Scots Bay comparisons not possible between historic and modern, will       use Scots Bay modern and compare to Prince 5 modern
# Leaves German Bank - need to determine what depth? Cut 30+ as our data set often doesn't go that deep. Need to explore what we have for each season in terms of DFO data, number of data points

# QC Rules
# need at least 3 casts per depth bin/per year
# 5 or more is robust, 3 or less, increased uncertainty.

# In terms of observations per bin/per year, removing those bins without adequate number of casts takes away the low observations, use 3 observations as a minimum per year and bin.


# There is still the question of seasonality as well...


# table for data describing casts:
Summary <- Oceans %>%
  filter(
    ground == "German Bank",
    JD_bin == "LateSummer_Fall"
  ) %>%
  group_by(Year, ground, JD_bin) %>%
  summarise(
    n_unique_casts = n_distinct(id),
    .groups = "drop"
  ) %>%
  mutate(
    Useability = case_when(
      n_unique_casts < 3 ~ "Weak",
      n_unique_casts >= 3 & n_unique_casts <= 4 ~ "Moderate",
      n_unique_casts >= 5 ~ "Strong"
    )
  ) %>%
  arrange(Year)

Summary

#setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/QC")

#write.table(Summary, file= "GBnumberCastperGroundYear.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


## Next we remove years where there are less than 3 casts:

Oceans_filtered <- Oceans %>%
  # Step 1: keep only German Bank + LateSummer_Fall
  filter(
    ground == "German Bank",
    JD_bin == "LateSummer_Fall"
  ) %>%
  
  # Step 2: compute Useability per Year
  group_by(Year) %>%
  mutate(
    n_unique_casts = n_distinct(id),
    Useability = case_when(
      n_unique_casts >= 5 ~ "Strong",
      n_unique_casts >= 3 ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  ungroup() %>%
  
  # Step 3: remove Weak years
  filter(Useability != "Weak")


unique(Oceans_filtered$Useability)

###########################################################

#for the data left (two years removed 1985 and 1978), now calculate number of observations per year and depth bin

#plot1 number of casts per year and depth bin
BinSummary <- Oceans_filtered %>%
  group_by(Year, ground, depth_bin) %>%
  summarise(
    n_observations = n(),
    n_unique_casts = n_distinct(id),
    .groups = "drop"
  ) %>%
  arrange(ground, Year, depth_bin)

ggplot(BinSummary, aes(x = factor(Year), y = depth_bin, fill = n_unique_casts)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "mistyrose", high = "darkred") +
  labs(
    x = "Year",
    y = "Depth bin (m)",
    fill = "Unique casts",
    title = "Cast support by year and depth bin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## plot2 number of observations per year and depth bin
ggplot(BinSummary, aes(x = factor(Year), y = depth_bin, fill = n_observations)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    x = "Year",
    y = "Depth bin (m)",
    fill = "Number of observations",
    title = "Observation density by year and depth bin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#######################################################

## Use all available bins / year  - can run a sensitivity with only Strong cast coverage and bins from 5-20m.
### Sensitivity run - same as above but with greater thresholds for sample size:
# table for data describing casts:
## Next we remove years where there are less than 5 casts:

Oceans_filtered <- Oceans %>%
  # Step 1: keep only German Bank + LateSummer_Fall
  filter(
    ground == "German Bank",
    JD_bin == "LateSummer_Fall"
  ) %>%
  
  # Step 2: compute Useability per Year
  group_by(Year) %>%
  mutate(
    n_unique_casts = n_distinct(id),
    Useability = case_when(
      n_unique_casts >= 5 ~ "Strong",
      n_unique_casts >= 3 ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  ungroup() %>%
  
  # Step 3: remove Weak years
  filter(!Useability %in% c("Moderate", "Weak"))
  
unique(Oceans_filtered$Useability)

#for the data left (two years removed 1985 and 1978), now calculate number of observations per year and depth bin

#plot1 number of casts per year and depth bin
BinSummary <- Oceans_filtered %>%
  group_by(Year, ground, depth_bin) %>%
  summarise(
    n_observations = n(),
    n_unique_casts = n_distinct(id),
    .groups = "drop"
  ) %>%
  arrange(ground, Year, depth_bin)

ggplot(BinSummary, aes(x = factor(Year), y = depth_bin, fill = n_unique_casts)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "mistyrose", high = "darkred") +
  labs(
    x = "Year",
    y = "Depth bin (m)",
    fill = "Unique casts",
    title = "Cast support by year and depth bin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## plot2 number of observations per year and depth bin
ggplot(BinSummary, aes(x = factor(Year), y = depth_bin, fill = n_observations)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    x = "Year",
    y = "Depth bin (m)",
    fill = "Number of observations",
    title = "Observation density by year and depth bin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

########################################################################
## Test seasonality:



ggplot(Oceans_filtered, aes(x = JulianDay, fill = Source)) +
  geom_histogram(bins = 20, alpha = 0.3, position = "identity")

Oceans_filtered %>%
  group_by(Source) %>%
  summarise(
    n = n(),
    min_JD = min(JulianDay, na.rm = TRUE),
    q25_JD = quantile(JulianDay, 0.25, na.rm = TRUE),
    median_JD = median(JulianDay, na.rm = TRUE),
    mean_JD = mean(JulianDay, na.rm = TRUE),
    q75_JD = quantile(JulianDay, 0.75, na.rm = TRUE),
    max_JD = max(JulianDay, na.rm = TRUE),
    sd_JD = sd(JulianDay, na.rm = TRUE)
  )


wilcox.test(JulianDay ~ Source, data = Oceans_filtered)
ks.test(
  Oceans_filtered$JulianDay[Oceans_filtered$Source == "DFO"],
  Oceans_filtered$JulianDay[Oceans_filtered$Source == "HSC"]
)

Oceans_cast <- Oceans_filtered %>%
  group_by(Source, id, Year, ground, JD_bin, depth_bin, JulianDay) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

# this model ignores the effect of different julian days
m_cast0 <- gam(Temperature ~ Source + depth_bin, data = Oceans_cast, method = "REML")

#modern still warmer, but assuming that both sources have the same seasonal shifts
m_cast1 <- gam(Temperature ~ Source + s(JulianDay) + depth_bin, data = Oceans_cast, method = "REML")


summary(m_cast0)
summary(m_cast1)
AIC(m_cast0, m_cast1)

Oceans_cast <- Oceans_cast %>%
  mutate(Source = factor(Source))

# allows source specific seasonal structure 
# it explains more variation than forcing DFO and HSC to share the same seasonal curve 
#The relationship between temperature and Julian day is different for DFO and HSC
m_cast2 <- gam(Temperature ~ Source + depth_bin + s(JulianDay, by = Source),
  data = Oceans_cast,
  method = "ML"
)

summary(m_cast2)

m_cast1_ml <- gam(
  Temperature ~ Source + s(JulianDay) + depth_bin,
  data = Oceans_cast,
  method = "ML"
)

AIC(m_cast1_ml, m_cast2)
anova(m_cast1_ml, m_cast2, test = "Chisq")
# DFO and HSC differ mainly in their temperature-vs-Julian-day relationship, not simply by one constant average temperature difference across the seasonal bin.

# Temperature on German Bank during the LateSummer_Fall bin varies substantially within the bin as a function of Julian day. When Julian day is ignored, modern observations appear substantially warmer than historical observations. After accounting for a shared nonlinear Julian-day effect, the modern–historical contrast is reduced but remains positive. However, when separate Julian-day smooths are fit for each source, model fit improves further and the overall source offset is no longer significant, indicating that differences between the historical and modern datasets are better characterized as differences in within-bin seasonal pattern rather than as one uniform temperature shift.

#ne important statistical note: You did the right thing by comparing m_cast1_ml and m_cast2 with ML rather than REML.
#That’s the more appropriate way to compare models that differ in their fixed/smooth structure. In mgcv, REML and ML are both likelihood-based smoothness-selection approaches, and guidance commonly recommends using ML when formally comparing models that differ in fixed effects or non-fully penalized smooth structure. [web.mit.edu], [stats.stac...change.com]
#So your AIC and deviance comparison here are on solid footing. mgcv documentation also explicitly describes REML/ML as alternatives for likelihood-based smoothness selection in GAMs.



plot(m_cast2, pages = 1, shade = TRUE)

newdat <- expand.grid(
  JulianDay = seq(min(Oceans_cast$JulianDay), max(Oceans_cast$JulianDay), length.out = 200),
  Source = levels(factor(Oceans_cast$Source)),
  depth_bin = "[5,10)"
)

newdat$fit <- predict(m_cast2, newdata = newdat, type = "response")

ggplot(newdat, aes(x = JulianDay, y = fit, color = Source)) +
  geom_line(linewidth = 1.2) +
  theme_bw()
#You have:
#s(JulianDay):SourceDFO → edf = 2.87, p = 5.33e-06
#s(JulianDay):SourceHSC → edf = 5.51, p < 2e-16
#So both sources have meaningful within-bin timing effects.Interpretation of the edf values

#DFO smooth (edf ~ 2.9) = relatively simpler curve
#HSC smooth (edf ~ 5.5) = more flexible / more structured curve
#That makes sense with your sampling:
#DFO had much narrower and more uneven Julian-day coverage HSC had broader coverage across the seasonal window
#So HSC has enough data to support a more complex within-bin seasonal pattern.
#The difference between historical and modern temperature is not well described as one constant overall shift across the whole seasonal bin.
#Instead, the difference seems to depend on where you are within Julian day


#You now have evidence that:
  
#Julian day strongly affects temperature within the seasonal bin
#That effect differs between DFO and HSC
#Therefore, a simple additive historical-vs-modern comparison is too simplistic
#So your instinct was right.

Oceans_cast <- Oceans_cast %>%
  mutate(
    Source = factor(Source),
    depth_bin = factor(depth_bin)
  )

newdat <- expand.grid(
  JulianDay = seq(min(Oceans_cast$JulianDay, na.rm = TRUE),
                  max(Oceans_cast$JulianDay, na.rm = TRUE),
                  length.out = 200),
  Source = levels(Oceans_cast$Source),
  depth_bin = levels(Oceans_cast$depth_bin)
)

pred <- predict(m_cast2, newdata = newdat, se.fit = TRUE)

newdat <- newdat %>%
  mutate(
    fit = pred$fit,
    se = pred$se.fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

ggplot(newdat, aes(x = JulianDay, y = fit, color = Source, fill = Source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ depth_bin, nrow = 1) +
  theme_bw() +
  labs(
    x = "Julian day",
    y = "Predicted temperature (°C)",
    title = "Source-specific Julian-day smooths by depth bin"
  )



m_cast3 <- gam(
  Temperature ~ Source * depth_bin + s(JulianDay, by = Source),
  data = Oceans_cast,
  method = "ML"
)

summary(m_cast3)


AIC(m_cast2, m_cast3)
anova(m_cast2, m_cast3, test = "Chisq")

#the HSC-vs-DFO contrast is significantly smaller at 20–25 m and 25–30 m than it is at 5–10 m.
#The strongest pattern is that historical and modern temperature differ in their Julian-day seasonal structure; depth-dependent differences are secondary and only weakly supported overall.

#keep m_cast2

#At the cast level, adding source-specific Julian-day smooths substantially improved model fit, indicating that historical (DFO) and modern (HSC) temperature observations differed in their within-bin seasonal pattern across Julian day. Adding a Source × depth_bin interaction provided only a small additional improvement (AIC 944.7 vs. 943.8; deviance test p = 0.074), suggesting limited overall support for depth-varying historical-modern differences. However, the interaction terms for the deeper bins (20–25 m and 25–30 m) were negative and statistically significant, indicating that the HSC–DFO contrast was smaller at those depths than in the reference 5–10 m bin. Overall, the main source of structure in the data appears to be source-specific seasonal timing, with only modest evidence that the magnitude of the historical-modern contrast also changes with depth.

































############################################


Oceans_filtered1 <- Oceans %>%
  # Step 1: create grouped JD_bin if needed
  mutate(
    JD_bin_grouped = case_when(
      JD_bin %in% c("LateSummer", "Fall") ~ "LateSummer_Fall",
      TRUE ~ as.character(JD_bin)
    )
  ) %>%
 
   # Step 2: compute n per Year × ground × season
  group_by(Year, ground, JD_bin_grouped) %>%
  mutate(
    n_unique_casts = n_distinct(id),
    Useability = case_when(
      n_unique_casts >= 5 ~ "Strong",
      n_unique_casts >= 3 ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  ungroup() %>%
  
  # Step 3: filter what you want
  filter(
    ground == "German Bank",
    JD_bin_grouped == "LateSummer_Fall",
    Useability %in% c("Moderate", "Strong")
  )


write.table(Oceans_filtered, file= "numberCastperGroundYear.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


# how many data points/cast
Cast_table <- Oceans_filtered %>%
  group_by(id) %>%
  mutate(
    n_per_id = n(),
    Useability = case_when(
      n_per_id >= 5 ~ "Strong",
      n_per_id >= 3 ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  ungroup() %>%
  distinct(id, Year, n_per_id, Useability, depth_bin) %>%
  arrange(Year, id)

Cast_table
table(Cast_table$Useability)


Cast_table <- Oceans_filtered %>%
  group_by(id) %>%
  mutate(
    n_per_id = n(),
    Useability = case_when(
      n_per_id >= 5 ~ "Strong",
      n_per_id >= 3 ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  ungroup() %>%
  distinct(id, Year, n_per_id, Useability)


Cast_table <- Oceans %>%
  group_by(id) %>%
  mutate(
    n_per_id = n(),
    Useability = case_when(
      n_per_id >= 5 ~ "Strong",
      n_per_id >= 3 ~ "Moderate",
      TRUE ~ "Weak"
    )
  ) %>%
  ungroup() %>%
  distinct(id, Year, n_per_id, Useability)

ggplot(Cast_table, aes(x = Year, y = n_per_id, color = Useability)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("Weak" = "red", "Moderate" = "orange", "Strong" = "darkgreen")
  ) +
  labs(
    x = "Year",
    y = "Number of observations per cast (n_per_id)",
    color = "Useability",
    title = "Number of Cast Data Point Strength Through Time"
  ) +
  theme_minimal()



OceansNbins <- Oceans_filtered %>%
  group_by(id) %>%
  mutate(
    n_bins = n_distinct(depth_bin)
  ) %>%
  ungroup()

#### this is where I filtered out casts with less than 3 bins...
OceansNBins <- OceansNbins %>%
  filter(n_bins >= 3)

### Lots of data issues....what years are usable, i.e. have more than 2 casts, and then within those cast how many data points? I think the binning takes care of the second issues though...

ggplot(OceansNbins, aes(x = Year, y = n_bins)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7, size = 2, color = "steelblue") +
  geom_hline(yintercept = 3, linetype = "dashed") +
  geom_hline(yintercept = 4, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Number of depth bins per cast",
    title = "Vertical Coverage of Casts (n_bins per ID)"
  ) +
  theme_minimal()



write.table(OceansNBins, file= "QC_GBlatesummerFall.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


##############################
casts_per_year <- OceansNBins %>%
  distinct(id, Year) %>%   # one row per cast
  group_by(Year) %>%
  summarise(
    n_casts = n(),
    .groups = "drop"
  ) %>%
  arrange(Year)



### Look at how many observations within each depth bin and year:
 BinSummary <- OceansNBins %>%
  group_by(Year, ground, depth_bin) %>%
  summarise(
    n_observations = n(),
    n_unique_casts = n_distinct(id),
    .groups = "drop"
  ) %>%
  arrange(ground, Year, depth_bin)
 
 ###############################try oceans filtered:
 BinSummary <- Oceans_filtered %>%
   group_by(Year, ground, depth_bin) %>%
   summarise(
     n_observations = n(),
     n_unique_casts = n_distinct(id),
     .groups = "drop"
   ) %>%
   arrange(ground, Year, depth_bin)
 

 write.table(BinSummary, file= "BinSummary.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
 

ggplot(BinSummary, aes(x = factor(Year), y = depth_bin, fill = n_unique_casts)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "mistyrose", high = "darkred") +
  labs(
    x = "Year",
    y = "Depth bin (m)",
    fill = "Unique casts",
    title = "Cast support by year and depth bin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



ggplot(BinSummary, aes(x = factor(Year), y = depth_bin, fill = n_observations)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    x = "Year",
    y = "Depth bin (m)",
    fill = "Number of observations",
    title = "Observation density by year and depth bin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


### I think I filtered out the 2 observation casts....kind of want to add back in.

getwd()








# choose your source (example: "HSC" or "DFO)
plot_data <- Oceans_filtered %>%
  filter(
    Source == "DFO",
    ground %in% c("German Bank"),
    JD_bin %in% c("LateSummer_Fall")
  ) %>%
  distinct(id, n_per_id)

ggplot(plot_data, aes(x = n_per_id)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  scale_x_continuous(breaks = seq(
    min(plot_data$n_per_id, na.rm = TRUE),
    max(plot_data$n_per_id, na.rm = TRUE),
    by = 1
  )) +
  labs(
    title = "Number of Observations per Cast",
    subtitle = "Source = DFO",
    x = "Number of rows per cast (n_per_id)",
    y = "Count of casts"
  ) +
  theme_bw()

#looks like I might still be able to do a comparison for late summer and fall in German Bank

subset_oceans <- Oceans %>%
  filter(id %in% plot_data$id)

# create output folder
output_dir <- "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/QC"
dir.create(output_dir, showWarnings = FALSE)

# get unique ids
ids <- unique(subset_oceans$id)

subset_oceans <- subset_oceans %>%
  group_by(id) %>%
  arrange(Depth, .by_group = TRUE) %>%
  ungroup()

# loop and plot
for (i in ids) {
  
  df_i <- subset_oceans %>% filter(id == i)
  
  p <- ggplot(df_i, aes(x = Temperature, y = Depth)) +
    geom_path() +
    scale_y_reverse() +
    labs(
      title = paste("Cast:", i),
      x = "Temperature",
      y = "Depth"
    ) +
    theme_bw()
  
  ggsave(
    filename = paste0(output_dir, "/cast_", i, ".png"),
    plot = p,
    width = 5,
    height = 6,
    dpi = 300
  )
}






# choose your source (example: "HSC" or "DFO)
plot_data2 <- Oceans %>%
  filter(
    Source == "HSC",
    ground %in% c("German Bank"),
    JD_bin %in% c("Fall")
  ) %>%
  distinct(id, n_per_id)

ggplot(plot_data2, aes(x = n_per_id)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black") +
  scale_x_continuous(breaks = seq(
    min(plot_data2$n_per_id, na.rm = TRUE),
    max(plot_data2$n_per_id, na.rm = TRUE),
    by = 5
  )) +
  labs(
    title = "Number of Observations per Cast",
    subtitle = "Source = HSC",
    x = "Number of rows per cast (n_per_id)",
    y = "Count of casts"
  ) +
  theme_bw()














# filtering, separate Oceans into Source
HSC=Oceans[which(Oceans$Source == "HSC"), ]
DFO=Oceans[which(Oceans$Source == "DFO"), ]


# For HSC
