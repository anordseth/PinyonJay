# ===================================================== #
# Process eBird Pinyon Jay data for Living Maps project #
# - Load and clean raw eBird detection data             #
# - Filter and spatially-temporally subsample points    #
# - Output cleaned datasets for SDM input               #
# ===================================================== #


# Load necessary libraries
library(arrow)
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ebirdst)


# File paths
input_path <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/PJ_Data/eBird_PJ/"
ebird_file <- paste0(input_path, "ebird_checklists_pinjay_2023.parquet")
features_file <- paste0(input_path, "ebirdst_features_2023.xlsx") 

out_path <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R/1_Data/"
pj_path <- paste0(out_path, "PJ_Obs/")

# output_csv <- paste0(data_path, "ebird_pj_clean.csv")
# output_geojson <- paste0(data_path, "ebird_pinyonjay_GEE.geojson")


# Load eBird dataset
pj <- read_parquet(ebird_file)

ggplot(pj, aes(x = effort_distance_km, y = effort_hrs, color = is_stationary)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "forestgreen")) +
  labs(
    title = "eBird Effort by Detection Type",
    x = "Effort Distance (km)",
    y = "Effort Hours",
    color = "Stationary"
  ) +
  theme_minimal()

# Load metadata
features <- read_excel(features_file)

# Clean presence category, fix dates, filter out mobile checklists longer than 5hrs
pj2 <- pj %>%
  mutate(presence = 1,  # every row is a detection
         date = as.Date(paste(year, day_of_year), format = "%Y %j"),
         month = month(date, label = TRUE)) %>%
  filter(effort_distance_km <= 1.5,
         # is_stationary | (!is_stationary & effort_hrs <= 5), # 27141
         effort_hrs <= 5) # 26384


ggplot(pj2, aes(x = effort_distance_km, y = effort_hrs, color = is_stationary)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "forestgreen")) +
  labs(
    title = "eBird Effort by Detection Type",
    x = "Effort Distance (km)",
    y = "Effort Hours",
    color = "Stationary"
  ) +
  theme_minimal()


# # Summarize number of checklists per year and month
# monthly_counts <- pj %>%
#   group_by(year, month) %>%
#   summarise(n_checklists = n(), .groups = "drop")
# 
# # Plot bar chart: Number of checklists per month, colored by year
# ggplot(monthly_counts, aes(x = month, y = n_checklists, fill = factor(year))) +
#   geom_bar(stat = "identity", position = "stack") +  # Dodge bars by year
#   scale_fill_viridis_d(name = "Year") +  # Colorblind-friendly palette
#   xlab("Month") +
#   ylab("No. Checklists") +
#   theme_minimal()


# Apply spatial-temporal subsampling using eBirdST best practices for presence-only data
# 3km x 3km spatial grid, weekly bins 
pj_subsampled <- grid_sample(
  x = pj2,
  coords = c("longitude", "latitude", "day_of_year"),
  is_lonlat = TRUE,
  res = c(3000, 3000, 7),
  sample_size_per_cell = 1,
  jitter_grid = TRUE,
  keep_cell_id = FALSE
)

pj_subsampled_febjul <- pj_subsampled %>%
  filter(month %in% c("Feb", "Mar", "Apr", "May", "Jun", "Jul"))


write.csv(pj_subsampled_febjul, paste0(pj_path, "ebird_pj_5hr_1500m_subsamp_febjul.csv"), row.names = FALSE)


###


# Creating a new version that limits the data to only stationary observations
pj_stat <- pj %>%
  filter(is_stationary == "TRUE")

pj_stat_subsamp <- grid_sample(
  x = pj_stat,
  coords = c("longitude", "latitude", "day_of_year"),
  is_lonlat = TRUE,
  res = c(3000, 3000, 7),
  sample_size_per_cell = 1,
  jitter_grid = TRUE,
  keep_cell_id = FALSE
)

write.csv(pj_stat_subsamp, paste0(pj_path, "ebird_pj_stat_subsamp.csv"), row.names = FALSE)






