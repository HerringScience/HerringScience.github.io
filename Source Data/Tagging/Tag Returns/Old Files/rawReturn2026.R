library(dplyr)
library(tidyr)

# Load files
complete <- read.csv(
  "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/complete.returns.csv",
  stringsAsFactors = FALSE
)

returns <- read.csv(
  "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/Tag Returns spreadsheets/2021_returns.csv",
  stringsAsFactors = FALSE
)

# Convert complete tag field
complete$TAG_NUMBER <- as.character(complete$TAG_NUMBER)

# Convert HSC and DFO tags into a single column
returns_long <- returns %>%
  pivot_longer(
    cols = c(HSC, DFO),
    names_to = "TagType",
    values_to = "TAG_NUMBER"
  ) %>%
  filter(!is.na(TAG_NUMBER))

returns_long$TAG_NUMBER <- as.character(returns_long$TAG_NUMBER)

# Find tags not already in complete.returns
missing_tags <- anti_join(
  returns_long,
  complete %>% select(TAG_NUMBER),
  by = "TAG_NUMBER"
)

# Create rows matching complete.returns structure
new_rows <- data.frame(
  matrix(
    NA,
    nrow = nrow(missing_tags),
    ncol = ncol(complete)
  )
)

colnames(new_rows) <- colnames(complete)

# Populate available fields
new_rows$TAG_NUMBER <- missing_tags$TAG_NUMBER
new_rows$dataorigin <- "2021_returns.csv"

# Append
complete_updated <- bind_rows(
  complete,
  new_rows
)

# Save
write.csv(
  complete_updated,
  "C:/Users/herri/Documents/GitHub/HerringScience.github.io/Source Data/Tagging/Tag Returns/complete.returns.updated.csv",
  row.names = FALSE
)

cat(nrow(new_rows), "tags added\n")