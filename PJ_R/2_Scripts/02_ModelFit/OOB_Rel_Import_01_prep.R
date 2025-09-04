# ========================================================== X
# ========================================================== X
# ======================= MODEL FIT ======================== X
# ============ OOB Error & Relative Importance ============= X
# ========================== PREP ========================== X
# ========================================================== X
# ========================================================== X

# This script calculates model evaluation statistics (OOB Error) using 
# Living Maps output (modelFitStats.csv). It reads in 
# `modelFitStats.csv` files from each region's folder,
# summarizes mean and confidence intervals,
# and writes a combined CSV file for later use.

library(tidyverse)
library(purrr)
library(glue)


# ------------------------
# Set up
# ------------------------

spp <- "PinyonJay"
spp_model <- "FebJul_1500m_FSM"

output <- "modelFitStats"

base_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R"
(mfs_in_dir <- file.path(base_dir, "3_ModelOutputs", spp_model, output))
(oob_out_dir <- file.path(base_dir, "4_Analysis", spp_model))
if (!dir.exists(oob_out_dir)) dir.create(oob_out_dir, recursive = TRUE)

# Get region names from input directory
# rgn_names <- list.dirs(in_dir, full.names = FALSE, recursive = FALSE)

# Output file
out_oob_fn <- file.path(oob_out_dir, "OOB_error.csv")
out_rel_import_fn <- file.path(oob_out_dir, "Rel_Import.csv")


# ------------------------
# Set Variable Names
# ------------------------

clim_vars <- c("aet_spring", "cwd_spring", "pdsi_spring", "pr_spring", "pr_summer",
               "soil_spring", "swe_winter", "tmax_summer", "tmin_winter",
               "vpd_latesum")

topo_vars <- c("elevation", "HLI", "slope", "TPI_3", "TPI_6", "TPI_12", "TPI_24", 
               "TPI_48", "ruggedness")

bands <- c("blue", "green", "red", "nir", "swir1", "swir2", "NDVI")
coefs <- c("COS1", "COS2", "COS3", "SIN1", "SIN2", "SIN3", "SLP")
dates <- c("02-15", "08-15")

# ccdc_vars <- c(as.vector(outer(bands, dates, function(b, d) glue("{b}_CCDC_fitted_{d}"))),
#                as.vector(sapply(coefs, function(c) outer(bands, dates, function(b, d) 
#                  glue("{b}_coefs_{c}_{d}")) %>% 
#                    as.vector())),
#                as.vector(outer(c("NDWI_predicted", "NBR_predicted", "NBR2_predicted",
#                                  "NDSI_predicted"), dates, paste, sep = "_")))

ccdc_vars <- c(
  as.vector(outer(bands, dates, function(b, d) glue("{b}_CCDC_fitted_{d}"))),
  as.vector(sapply(coefs, function(c) outer(bands, dates, function(b, d) 
    glue("{b}_coefs_{c}_{d}")) %>% as.vector())),
  as.vector(outer(c("NDWI", "NBR", "NBR2", "NDSI"), dates, function(v, d) glue("{v}_{d}")))
)


# ------------------------
# Load and filter data
# ------------------------

fit_stats_all <- list.files(mfs_in_dir, pattern = "Export_modelFitStats_BCR.*\\.csv$", 
                            full.names = TRUE) %>%
  map_dfr(function(fn) {
    bcr <- stringr::str_extract(fn, "BCR\\d+")
    read_csv(fn) %>% mutate(train_region = bcr)
  })

if (nrow(fit_stats_all) == 0) stop("No modelFitStats.csv files found.")

fit_stats <- fit_stats_all %>%
  dplyr::select(-any_of(c("system:index", ".geo"))) %>%
  mutate(species = spp, model_type = spp_model) %>%
  relocate(c(species, model_type, train_region, fold, oob_error = OOBerror)) 


# ------------------------
# Calculate OOB Error
# ------------------------

oob <- fit_stats %>%
  dplyr::select(c(species, model_type, train_region, fold, oob_error))

oob_error <- oob %>%
  group_by(train_region) %>%
  dplyr::summarize(mean_oob = mean(oob_error),
            sd_oob = sd(oob_error),
            n = n(), .groups = "drop") %>%
  mutate(ci = (1.96 * (sd_oob/sqrt(n))),
         minus = mean_oob - ci,
         plus = mean_oob + ci) %>%
  left_join(oob, ., by = c("train_region"))

head(oob_error)

# Save
saveRDS(oob_error, out_oob_fn)
write.csv(oob_error, out_oob_fn, row.names = F)

# OOB error for manuscript table
oob_summary <- fit_stats %>%
  group_by(train_region) %>%
  dplyr::summarise(mean_oob = round(mean(oob_error, na.rm = TRUE), 4),
                   sd_oob = round(sd(oob_error, na.rm = TRUE), 4),
                   .groups = "drop")

# Write to file
write_csv(oob_summary, file.path(oob_out_dir, "OOB_error_summary_mean_sd.csv"))

# Preview
print(oob_summary)



# ----------------------------X
# ---- RELATIVE IMPORTANCE ----
# ----------------------------X

# pred_vars <- c(ccdc_vars, clim_vars, topo_vars)

# Pull predictor variable names from the dataset, dropping only metadata columns
pred_vars <- setdiff(names(fit_stats),
                     c("species", "model_type", "pts_type", "train_region", 
                       "trainRegion", "fold", "oob_error"))

# Reshape and calculate relative importance per region
# rel_import <- fit_stats %>%
#   dplyr::select(train_region, all_of(pred_vars)) %>%
#   pivot_longer(-train_region, names_to = "variable", values_to = "importance") %>%
#   group_by(train_region, variable) %>%
#   summarize(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop")

rel_import <- fit_stats %>%
  dplyr::select(train_region, fold, all_of(pred_vars)) %>%
  pivot_longer(-c(train_region, fold), names_to = "variable", values_to = "rel_import")

# Save full relative importance table
write.csv(rel_import, out_rel_import_fn, row.names = FALSE)

# Option to save per-region CSVs
# for (rgn in unique(rel_import$train_region)) {
#   out_rgn <- filter(rel_import, train_region == rgn)
#   write.csv(out_rgn, file.path(out_dir, paste0("Rel_Import_", rgn, ".csv")), row.names = FALSE)
# }



# ---------------------------------------------------------- X
# ---- JOIN VARIABLE CLASSES & CALCULATE RELATIVE IMPORT ----
# ---------------------------------------------------------- X

# 1. Create a dataframe linking each variable to its class and name
env_class <- data.frame(env_class = "Climate", variable = clim_vars, variable_name = clim_vars) %>%
  bind_rows(data.frame(env_class = "Reflectance", variable = ccdc_vars, variable_name = ccdc_vars)) %>%
  bind_rows(data.frame(env_class = "Topography", variable = topo_vars, variable_name = topo_vars)) %>%
  mutate(n = n())

# 2. Calculate proportion of each env class
env_class <- env_class %>%
  group_by(env_class, n) %>%
  reframe(prop_env_class = n() / n) %>%
  distinct() %>%
  left_join(env_class, ., by = c("env_class", "n")) %>%
  select(-n) %>%
  mutate(env_class = as.factor(env_class))

sapply(env_class, class)
sapply(rel_import, class)

unmatched_vars <- setdiff(unique(rel_import$variable), env_class$variable)

# 3. Join variable class metadata to rel_import
rel_import <- rel_import %>%
  left_join(env_class, by = "variable") %>%
  relocate(prop_env_class, variable, variable_name, .after = env_class)

rel_imp_check <- rel_import %>%
  select(env_class, variable, variable_name) %>%
  distinct()
  

# 4. Summarize importance by category per model-region
rel_import_sum <- rel_import %>%
  group_by(train_region, env_class, prop_env_class) %>%
  dplyr::summarize(sum_rel_import = sum(rel_import), .groups = "drop")

# 5. Calculate proportion of total relative importance for each category
rel_import_prop <- rel_import_sum %>%
  group_by(train_region) %>%
  summarize(sum = sum(sum_rel_import), .groups = "drop") %>%
  left_join(rel_import_sum, by = c("train_region")) %>%
  mutate(prop_rel_import = sum_rel_import / sum) %>%
  select(-sum)

# 6. Sanity check: category proportions should sum to 1 per region
rel_import_prop %>%
  group_by(train_region) %>%
  summarize(sum = sum(prop_rel_import)) %>%
  print()

# 7. Calculate mean and CI of category proportions per region
rel_import_prop_mean_sd <- rel_import_prop %>%
  group_by(train_region, env_class, prop_env_class) %>%
  summarize(mean_prop_rel_import = mean(prop_rel_import),
            sd_prop_rel_import = sd(prop_rel_import),
            n = n(), .groups = "drop") %>%
  mutate(ci_prop = 1.96 * (sd_prop_rel_import / sqrt(n)),
         minus_prop = mean_prop_rel_import - ci_prop,
         plus_prop = mean_prop_rel_import + ci_prop) %>%
  left_join(rel_import_prop, 
            by = c("train_region", "env_class", "prop_env_class"))

# 8. Scale variable importance from 0–1 within each model-run
rel_import_scale <- rel_import %>%
  nest(data = -c(train_region, fold)) %>%
  mutate(data = map(data, function(x) {
    x$rel_import_scale <- (x$rel_import - min(x$rel_import)) /
      (max(x$rel_import) - min(x$rel_import))
    return(x)
  })) %>%
  unnest(cols = data)

# 9. Get mean and CI of scaled variable importance
rel_import_scale_mean_sd <- rel_import_scale %>%
  group_by(train_region, env_class, prop_env_class, variable, variable_name) %>%
  summarize(mean_rel_import_scale = mean(rel_import_scale),
            sd_rel_import_scale = sd(rel_import_scale),
            n = n(), .groups = "drop") %>%
  mutate(ci_rel_import = 1.96 * (sd_rel_import_scale / sqrt(n)),
         minus_rel_import = mean_rel_import_scale - ci_rel_import,
         plus_rel_import = mean_rel_import_scale + ci_rel_import) %>%
  left_join(rel_import_scale, 
            by = c("train_region", "env_class", "prop_env_class", 
                   "variable", "variable_name"))

# 10. Join scaled variable importance with category proportions
rel_import_full <- left_join(rel_import_scale_mean_sd, rel_import_prop_mean_sd,
                             by = c("train_region", "env_class", "prop_env_class"))

# 11. Split into category-level and variable-level outputs
rel_import_sum <- rel_import_full %>%
  select(train_region, env_class, prop_env_class, 
         mean_prop_rel_import, sd_prop_rel_import, 
         ci_prop, minus_prop, plus_prop) %>%
  distinct()

rel_import_vars <- rel_import_full %>%
  select(train_region, env_class, variable, variable_name,
         mean_rel_import_scale, sd_rel_import_scale, 
         minus_rel_import, plus_rel_import) %>%
  distinct()


# 12. Save 
saveRDS(rel_import_full, file.path(oob_out_dir, "Rel_Import_full.rds"))
saveRDS(rel_import_sum, file.path(oob_out_dir, "Rel_Import_sum.rds"))
saveRDS(rel_import_vars, file.path(oob_out_dir, "Rel_Import_vars.rds"))

write.csv(rel_import_full, file.path(oob_out_dir, "Rel_Import_full.csv"),
          row.names = FALSE)
write.csv(rel_import_sum, file.path(oob_out_dir, "Rel_Import_sum.csv"),
          row.names = FALSE)
write.csv(rel_import_vars, file.path(oob_out_dir, "Rel_Import_vars.csv"),
          row.names = FALSE)






