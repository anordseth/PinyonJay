# Load necessary libraries
library(arrow)
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# File paths
data_path <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/PJ_Data/eBird_PJ/"
ebird_file <- paste0(data_path, "ebird_checklists_pinjay_2023.parquet")
features_file <- paste0(data_path, "ebirdst_features_2023.xlsx") 
output_csv <- paste0(data_path, "ebird_pj_clean.csv")
output_geojson <- paste0(data_path, "ebird_pinyonjay_GEE.geojson")

# Load eBird dataset
pj <- read_parquet(ebird_file)

# Load variable descriptions (for reference, not merging)
features <- read_excel(features_file)

# Convert missing obs_count values to presence-absence format
pj <- pj %>%
  mutate(presence = 1,  # bc every row is a detection
         is_stationary = as.logical(is_stationary))

# Filter effort hours > 5 when traveling
pj <- pj %>%
  filter(!(is_stationary == FALSE & effort_hrs > 5))

# Convert year + day_of_year into a proper date and extract month names
pj <- pj %>%
  mutate(observation_date = as.Date(paste(year, day_of_year), format = "%Y %j"),
         month = month(observation_date, label = TRUE))

# Summarize number of checklists per year and month
monthly_counts <- pj %>%
  group_by(year, month) %>%
  summarise(n_checklists = n(), .groups = "drop")

# Plot bar chart: Number of checklists per month, colored by year
ggplot(monthly_counts, aes(x = month, y = n_checklists, fill = factor(year))) +
  geom_bar(stat = "identity", position = "stack") +  # Dodge bars by year
  scale_fill_viridis_d(name = "Year") +  # Colorblind-friendly palette
  xlab("Month") +
  ylab("No. Checklists") +
  theme_minimal()

# # Select relevant columns for GEE upload
# pj_export <- pj %>%
#   select(latitude, longitude, presence, obs_count, is_stationary, effort_hrs, cci)

# Save cleaned dataset as CSV for Google Earth Engine
write.csv(pj, paste0(data_path, "ebird_pj_clean.csv"), row.names = FALSE)




# No. eBird pts / ecoregion 
counts <- c(6,184,154,22,119,19,222,216,8,1071,7,77,26,230,152,185,
            1239,1552,822,12,287,20,63,33,2,42,130,12,12,47,8,26,766)

# Convert to data frame
df <- data.frame(counts = counts)

# Plot
ggplot(df, aes(x = counts)) +
  geom_histogram(binwidth = 10, color = "black", fill = "steelblue") +
  theme_minimal()


############

library(tidyverse)
library(here)

pts <- read_csv(here("1_Data", "Thinned_eBird_PJ_Pts.csv"))

pts.yr.er <- pts %>%
  group_by(year, L2_CODE_NUM) %>%
  summarise(no_pts = n(), .groups = "drop")

total_pts <- pts %>%
  filter(L2_CODE_NUM == 1) %>%       # Replace 4 with your target ecoregion
  dplyr::summarise(total_points = n())

print(total_pts)


# just points per ecoregion
pts.er <- pts %>%
  group_by(L2_CODE_NUM) %>%
  summarise(no_pts = n(), .groups = "drop")

