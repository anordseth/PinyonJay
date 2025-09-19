# ======================================================
# PROCESS HABITAT TRENDS DATA
# FOR PINYON JAY LIVING MAPS ANALYSIS
# Part of the workflow that uses quantiles to classify
# habitat as non-habitat, low, moderate, and high quality
# ======================================================

library(tidyverse)

# Set working directory to where the Drive export is downloaded
base_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R"
spp <- "PinyonJay"
spp_model <- "FebJul_1500m_FSM"

(ht_in_dir <- file.path(base_dir, "3_ModelOutputs", spp_model, "habitatAreaClass"))
ht_out_dir <- file.path(base_dir, "4_Analysis", spp_model, "Habitat_Trends")
if (!dir.exists(ht_out_dir)) dir.create(ht_out_dir, recursive = TRUE)

(hab_trend_fn <- file.path(ht_in_dir, "Habitat_area_Trends_classified_091225.csv"))
# (hab_trend_fn <- file.path(ht_in_dir, "Habitat_area_Trends_classified_ALLBCRS.csv"))
out_fn <- file.path(ht_out_dir, "Habitat_Trends_classified_prepped.rds")

# List of BCRs used as regions
rgn_names <- paste0("BCR", c(9, 10, 15, 16, 17, 18, 32, 33, 34, 35))
hab_class_names <- c("Low", "Moderate", "High")

# Load and process data
hab_trend <- read.csv(hab_trend_fn) %>%
  dplyr::select(-c(system.index, .geo)) %>%
  filter(region %in% rgn_names) %>%
  mutate(region = factor(region, levels = rgn_names),
         species = spp) %>%
  dplyr::select(c(species, region, year, 
           area_m2_low, area_m2_mod, area_m2_high)) %>%
  arrange(region, year)

hab_trend_l <- hab_trend %>%
  pivot_longer(cols = c(area_m2_low, area_m2_mod, area_m2_high),
               names_to = "hab_qual_class", values_to = "area_m2") %>%
  mutate(hab_qual_class = str_remove(hab_qual_class, "area_m2_"),
         hab_qual_class = recode(hab_qual_class,
                                 low = "Low",
                                 mod = "Moderate",
                                 high = "High"),
         hab_qual_class = factor(hab_qual_class, levels = hab_class_names),
         area_km2 = area_m2 / 1e6,
         area_ha = area_km2 * 100,
         area_1k_ha = area_ha / 1000) %>%
  arrange(species, region, year, hab_qual_class)

summary(hab_trend_l)


# Save output
saveRDS(hab_trend_l, out_fn)


