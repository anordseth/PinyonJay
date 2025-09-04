# ========================================================== X
# ======================= MODEL FIT ======================== X
# ========================== AUC =========================== X
# ========================== PREP ========================== X
# ========================================================== X

# This script calculates model evaluation statistics (AUC) using 
# Living Maps output (modelProjStats). It reads in 
# `modelProjStats.csv` files from each region's folder,
# computes AUC per fold, summarizes mean and confidence intervals,
# and writes a combined CSV file for later use.

library(tidyverse)
library(pROC)

# ------------------------
# Set up metadata
# ------------------------

spp <- "PinyonJay"
ptsDates <- "FebJul_1500m_FSM" 
inputFile <- "modelProjStats"

base_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R"

(auc_within_dir <- file.path(base_dir, "3_ModelOutputs", ptsDates, inputFile))
(auc_cross_dir <- file.path(base_dir, "3_ModelOutputs", ptsDates, inputFile, "crossRegion"))
(auc_out_dir <- file.path(base_dir, "4_Analysis", ptsDates))
if (!dir.exists(auc_out_dir)) dir.create(auc_out_dir, recursive = TRUE)

# Output files
out_all_fn <- file.path(auc_out_dir, "AUC_all.csv")
out_within_fn <- file.path(auc_out_dir, "AUC_within.csv")
out_cross_fn <- file.path(auc_out_dir, "AUC_cross.csv")

# ------------------------
# Load and combine data
# ------------------------

proj_stats_within <- list.files(auc_within_dir, pattern = "modelProjStats.*\\.csv$", 
                                full.names = TRUE) %>%
  map_dfr(read_csv)

# --- BEGIN OPTIONAL: Cross-region evaluation --- #
include_cross_region <- TRUE

if (include_cross_region) {
  proj_stats_cross <- list.files(auc_cross_dir, pattern = "crossProjStats.*\\.csv$",
                                 full.names = TRUE) %>%
    map_dfr(read_csv)
}

# --- END OPTIONAL --- #

# ------------------------
# Organize and filter
# ------------------------

prep_proj_stats <- function(df) {
  df %>%
    dplyr::select(-`system:index`, -.geo) %>%
    dplyr::relocate(fold, modelFold, trainRegion, projRegion, useAvail, .before = 1) %>%
    dplyr::select(fold, modelFold, trainRegion, projRegion, useAvail, RFprob) %>%
    mutate(trainRegion = factor(trainRegion),
           projRegion = factor(projRegion))
}

proj_stats_within <- prep_proj_stats(proj_stats_within)
if (include_cross_region) {
  proj_stats_cross <- prep_proj_stats(proj_stats_cross)
}

# ------------------------
# Calculate AUC separately
# ------------------------

calc_auc <- function(df) {
  df %>%
    filter(fold == modelFold) %>%
    group_by(trainRegion, projRegion, fold) %>%
    nest(data = c(useAvail, RFprob)) %>%
    mutate(auc = map_dbl(data, ~ auc(.x$useAvail, .x$RFprob, quiet = TRUE))) %>%
    ungroup()
}

auc_within <- calc_auc(proj_stats_within)
if (include_cross_region) {
  auc_cross <- calc_auc(proj_stats_cross)
}

# Summarize with mean, sd, CI for each
summarize_auc <- function(df) {
  df %>%
    group_by(trainRegion, projRegion) %>%
    dplyr::summarise(n = n(),
                     mean_auc = mean(auc),
                     sd_auc = sd(auc), .groups = "drop") %>%
    mutate(ci = 1.96 * (sd_auc / sqrt(n)),
           minus = mean_auc - ci,
           plus = mean_auc + ci)
}

auc_summary_within <- summarize_auc(auc_within)
if (include_cross_region) {
  auc_summary_cross <- summarize_auc(auc_cross)
}

# ------------------------
# Optionally combine summaries (e.g., for plotting)
# ------------------------

auc_summary_all <- bind_rows(
  mutate(auc_summary_within, comparison = "within"),
  if (include_cross_region) mutate(auc_summary_cross, comparison = "cross") else NULL
) %>%
  mutate(model = ptsDates)

# ------------------------
# Save outputs separately
# ------------------------

write_csv(auc_summary_within, out_within_fn)
if (include_cross_region) {
  write_csv(auc_summary_cross, out_cross_fn)
}
write_csv(auc_summary_all, file.path(auc_out_dir, paste0("AUC_All_Summary_", ptsDates, ".csv")))





