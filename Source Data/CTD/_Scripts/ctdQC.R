
# QC visualizations

# comparing HSC and DFO
# vertical resolution mismatch
# maximum depth problems 

# trim top 5m to control for surface noise caused by strong tides
# likely Scots Bay comparisons not possible between historic and modern
# Leaves German Bank - need to determine what depth? Cut 30+ as our data set often doesn't go that deep. Need to explore what we have for each season in terms of DFO data, number of datapoints

# If I am to generate an average per year and depth bin (combining the casts for that season) I think it matters less about the number of measurements per cast, but more the number of observations per depth bin

head(Oceans)

## Looking at differences in data density,
### 5-35m cut off

unique(Oceans$JD_bin)
#"EarlySummer" "LateSummer"  "Fall"  
unique(Oceans$ground)

head(Oceans)

# table for data describing casts:
Summary <- Oceans %>%
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
  arrange(JD_bin, ground, Year)
Summary

setwd("C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/CTD/QC")

write.table(Summary, file= "numberCastperGroundYear.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 


Oceans_filtered <- Oceans %>%
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

OceansNBins <- OceansNbins %>%
  filter(n_bins >= 3)

### Lots of data issues....what years are usable, i.e. have more than 2 casts, and then within those cast how many data points? I think the binning takes care of the second issues though...

ggplot(OceansNBins, aes(x = Year, y = n_bins)) +
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



casts_per_year <- OceansNBins %>%
  distinct(id, Year) %>%   # one row per cast
  group_by(Year) %>%
  summarise(
    n_casts = n(),
    .groups = "drop"
  ) %>%
  arrange(Year)

casts_per_year



### Look at how many observations within each depth bin and year:
 BinSummary <- OceansNBins %>%
  group_by(Year, ground, depth_bin) %>%
  summarise(
    n_observations = n(),
    n_unique_casts = n_distinct(id),
    .groups = "drop"
  ) %>%
  arrange(ground, Year, depth_bin)

BinSummary

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
