# threshold_summary_analysis.R
# Analyze model evaluation metrics (sensitivity, specificity, Youden's Index) across BCRs

# Load required libraries
library(tidyverse)
library(janitor)
library(readr)


# -----------------------------
# Set up + load data
# -----------------------------

base_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R"
(th_in_dir <- file.path(base_dir, "3_ModelOutputs", "FebJulPts", "R10_rand", "threshold"))
(th_out_dir <- file.path(base_dir, "4_Analysis", "FebJulPts", "R10_rand"))

# Automatically load the only CSV in the threshold directory
input_file <- list.files(th_in_dir, pattern = "*.csv", full.names = TRUE)
threshold_stats <- read_csv(input_file) %>%
  select(-c("system:index", ".geo"))
glimpse(threshold_stats)


# ------------------------
# Calculate Youden's Index
# ------------------------

threshold_stats <- threshold_stats %>%
  mutate(youden_index = sensitivity + specificity - 1)

# ------------------------
# Plot Sensitivity vs Specificity
# ------------------------

ggplot(threshold_stats, aes(x = sensitivity, y = specificity, label = region)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -1, size = 3) +
  labs(x = "Sensitivity (True Positive Rate)",
       y = "Specificity (True Negative Rate)") +
  theme_minimal()


# ------------------------
# Plot Youden's Index by BCR
# ------------------------

ggplot(threshold_stats, aes(x = reorder(region, youden_index), y = youden_index)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Youden's Index by BCR",
    subtitle = "Youden's Index = Sensitivity + Specificity - 1",
    x = "Bird Conservation Region",
    y = "Youden's Index"
  ) +
  theme_minimal() +
  coord_flip()


# ------------------------
# Flag low-performing regions
# ------------------------

low_perf_threshold <- 0.5

low_perf <- threshold_stats %>%
  filter(youden_index < low_perf_threshold)

if (nrow(low_perf) > 0) {
  message("BCRs with low Youden's Index (< ", low_perf_threshold, "):")
  print(low_perf %>% select(region, sensitivity, specificity, youden_index))
} else {
  message("All regions have acceptable Youden's Index (>= ", low_perf_threshold, ").")
}

# ------------------------
# Save cleaned summary
# ------------------------

output_file <- file.path(th_out_dir, "thresholdStats_clean.csv")
write_csv(threshold_stats, output_file)
