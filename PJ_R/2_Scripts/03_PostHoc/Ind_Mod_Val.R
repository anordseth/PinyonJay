# =======================================
# Independent Validation: AUC Calculation
# For Pinyon Jay SDMs (adapted from Crego et al. 2024)
# =======================================

# Load libraries
library(tidyverse)
library(pROC)
library(data.table)

# Set validation data directory
val_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R/3_ModelOutputs/FebJulPts/R10_rand/validationData"

# List files
val_files <- list.files(val_dir, pattern = ".*\\.csv$", full.names = TRUE)

# Load and combine all validation files
val_data <- rbindlist(lapply(val_files, fread), fill = TRUE)

# Deduplicate validation points only; keep all random points
val_data <- val_data %>%
  select(meanProbability, presence, region, year, month, day, point, type)

# Separate validation and random points
val_validation <- val_data %>%
  filter(type == "validation") %>%
  distinct(region, year, month, day, point, .keep_all = TRUE)

val_random <- val_data %>%
  filter(type == "random")

# Combine and clean
val_data <- bind_rows(val_validation, val_random) %>%
  filter(!is.na(meanProbability)) %>%
  mutate(presence = ifelse(type == "validation", 1, 0))

# Summarize count by year and region
val_data %>% 
  count(region, year, presence) %>% 
  pivot_wider(names_from = presence, values_from = n, 
              names_prefix = "presence_") %>% 
  print()

# Grouped stats by region and year
grouped_stats <- val_data %>%
  group_by(region, year, presence) %>%
  summarise(
    mean = mean(meanProbability, na.rm = TRUE),
    sd = sd(meanProbability, na.rm = TRUE),
    n = n(), .groups = 'drop')
print(grouped_stats)

# Adaptive t-test or Wilcoxon test depending on normality and variance
adaptive_test_results <- val_data %>%
  group_by(region, year) %>%
  filter(n_distinct(presence) == 2) %>%
  group_modify(~ {
    pres <- .x %>% filter(presence == 1) %>% pull(meanProbability)
    rand <- .x %>% filter(presence == 0) %>% pull(meanProbability)
    
    if (length(pres) > 2 & length(rand) > 2) {
      shapiro_pres <- shapiro.test(pres)$p.value
      shapiro_rand <- shapiro.test(rand)$p.value
      
      if (shapiro_pres > 0.05 & shapiro_rand > 0.05) {
        var_res <- var.test(pres, rand)
        t_res <- t.test(pres, rand, var.equal = FALSE)
        tibble(
          test = "Welch t-test",
          stat = t_res$statistic,
          df = t_res$parameter,
          p_value = t_res$p.value,
          mean_val = t_res$estimate[1],
          mean_rand = t_res$estimate[2],
          shapiro_p_pres = shapiro_pres,
          shapiro_p_rand = shapiro_rand
        )
      } else {
        w_res <- wilcox.test(pres, rand)
        tibble(
          test = "Wilcoxon rank-sum",
          stat = w_res$statistic,
          df = NA,
          p_value = w_res$p.value,
          mean_val = mean(pres),
          mean_rand = mean(rand),
          shapiro_p_pres = shapiro_pres,
          shapiro_p_rand = shapiro_rand
        )
      }
    } else {
      tibble(
        test = NA, stat = NA, df = NA, p_value = NA,
        mean_val = NA, mean_rand = NA,
        shapiro_p_pres = NA, shapiro_p_rand = NA
      )
    }
  })
print(adaptive_test_results)

# Compute AUC by region and year
auc_results <- val_data %>%
  group_by(region, year) %>%
  filter(n_distinct(presence) == 2) %>%
  group_modify(~ {
    roc_obj <- tryCatch(
      roc(.x$presence, .x$meanProbability, quiet = TRUE),
      error = function(e) NULL
    )
    if (!is.null(roc_obj)) {
      tibble(
        auc = as.numeric(auc(roc_obj)),
        ci_lower = as.numeric(ci(roc_obj)[1]),
        ci_upper = as.numeric(ci(roc_obj)[3])
      )
    } else {
      tibble(auc = NA, ci_lower = NA, ci_upper = NA)
    }
  })
print(auc_results)

# Optional: Plot distributions by region-year
val_data %>%
  ggplot(aes(x = factor(presence), y = meanProbability, fill = factor(presence))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  facet_wrap(~region + year) +
  scale_fill_manual(values = c("gray70", "steelblue")) +
  labs(
    title = "Habitat Suitability Scores: Validation vs. Random",
    x = "Point Type (0 = Random, 1 = Validation)",
    y = "Mean Habitat Suitability"
  ) +
  theme_minimal()


