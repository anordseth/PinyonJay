# ===================================================== #
# Process eBird Pinyon Jay data for Living Maps project #
# - Load and clean raw eBird detection data             #
# - Filter and spatially-temporally subsample points    #
# - Output cleaned datasets for SDM input               #
# ===================================================== #

pacman::p_load(arrow, readxl, sf, dplyr, ggplot2, lubridate, tidyr, ebirdst)


# File paths
input_path <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/PJ_Data/eBird_PJ/"
ebird_file <- paste0(input_path, "ebird_checklists_pinjay_2023.parquet")
features_file <- paste0(input_path, "ebirdst_features_2023.xlsx") 

out_path <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R/1_Data/"
pj_path <- paste0(out_path, "PJ_Obs/")


# Load eBird data
pj <- read_parquet(ebird_file)

# Look at effort for stationary and nonstationary checklists
ggplot(pj, aes(x = effort_distance_km, y = effort_hrs, color = is_stationary)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "forestgreen")) +
  labs(title = "eBird Effort by Detection Type",
       x = "Effort Distance (km)",
       y = "Effort Hours",
       color = "Stationary") +
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
  labs(title = "eBird Effort by Detection Type",
       x = "Effort Distance (km)",
       y = "Effort Hours",
       color = "Stationary") +
  theme_minimal()


# Apply spatial-temporal subsampling using eBirdST best practices
# 3km x 3km spatial grid, weekly bins 
# https://ebird.github.io/ebirdst/reference/grid_sample.html
pj_subsampled_old <- grid_sample(x = pj2,
                             coords = c("longitude", "latitude", "day_of_year"),
                             is_lonlat = TRUE,
                             res = c(3000, 3000, 7),
                             sample_size_per_cell = 1,
                             jitter_grid = TRUE,
                             keep_cell_id = FALSE)

pj_subsampled <- grid_sample(x = pj2,
                             coords = c("longitude", "latitude", "year"),
                             is_lonlat = TRUE,
                             res = c(3000, 3000, 1),
                             sample_size_per_cell = 1,
                             jitter_grid = TRUE,
                             keep_cell_id = FALSE)


pj_subsampled_febjul_old <- pj_subsampled_old %>%
  filter(month %in% c("Feb", "Mar", "Apr", "May", "Jun", "Jul")) 

pj_subsampled_febjul <- pj_subsampled %>%
  filter(month %in% c("Feb", "Mar", "Apr", "May", "Jun", "Jul")) 
# not sure why I used this month format... but here we are

write.csv(pj_subsampled_febjul, paste0(pj_path, "ebird_pj_5hr_1500m_subsamp_3km3km1yr_febjul.csv"), row.names = FALSE)


###


# # Creating a new version that limits the data to only stationary observations
# pj_stat <- pj %>%
#   filter(is_stationary == "TRUE")
# 
# pj_stat_subsamp <- grid_sample(x = pj_stat,
#                                coords = c("longitude", "latitude", "day_of_year"),
#                                is_lonlat = TRUE,
#                                res = c(3000, 3000, 7),
#                                sample_size_per_cell = 1,
#                                jitter_grid = TRUE,
#                                keep_cell_id = FALSE)
# 
# write.csv(pj_stat_subsamp, paste0(pj_path, "ebird_pj_stat_subsamp.csv"), row.names = FALSE)
# 





