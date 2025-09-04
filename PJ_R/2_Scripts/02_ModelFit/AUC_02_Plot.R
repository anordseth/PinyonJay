# ========================================================== X
# ======================= MODEL FIT ======================== X
# ========================== AUC =========================== X
# ========================== PLOT ========================== X
# ========================================================== X

# This script generates a bar plot of AUC values with 95% CI 
# error bars using output from the Living Maps AUC evaluation script.

library(tidyverse)
library(cowplot)

# ------------------------
# Set up file paths
# ------------------------

spp <- "PinyonJay"
ptsDates <- "FebJul_1500m_FSM"

base_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R"
(auc_in_dir2 <- file.path(base_dir, "4_Analysis", ptsDates))
auc_out_dir2 <- file.path(base_dir, "5_Figures", ptsDates)
if (!dir.exists(auc_out_dir2)) dir.create(auc_out_dir2, recursive = TRUE)

# In files
auc_within_fn <- file.path(auc_in_dir2, "AUC_within.csv")
auc_cross_fn <- file.path(auc_in_dir2, "AUC_cross.csv")
auc_all_fn <- file.path(auc_in_dir2, "AUC_within.csv")

auc_within <- read_csv(auc_within_fn)
auc_cross <- read_csv(auc_cross_fn)
auc_within <- read_csv(auc_all_fn)
auc_all <- bind_rows(
  mutate(auc_within, comparison = "within"),
  mutate(auc_cross, comparison = "cross")
)

# ------------------------
# Bar plot (within-region only)
# ------------------------

# Identify region names and assign colors
rgn_names <- c("BCR9", "BCR10", "BCR15", "BCR16", "BCR17", "BCR18", "BCR32", "BCR33", "BCR34", "BCR35")
rgn_pal <- hcl.colors(length(rgn_names), "Viridis")
names(rgn_pal) <- rgn_names

# Factor levels to order BCR9 first
auc_within$projRegion <- factor(auc_within$projRegion, levels = rgn_names)
auc_within$trainRegion <- factor(auc_within$trainRegion, levels = rgn_names)

auc_p <- ggplot(auc_within, aes(x = projRegion, fill = trainRegion)) +
  geom_bar(aes(y = mean_auc), stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = minus, ymax = plus), col = "grey25",
                linewidth = 0.5, width = 0.3,
                position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = rgn_pal) +
  theme_bw() +
  theme(title = element_text(size = 9),
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.background = element_rect(color = "black", linewidth = 0.25),
        legend.key.size = unit(0.1, "in"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_blank()) +
  labs(y = "AUC", x = "Projected Region", fill = "Training Region")
auc_p

# Save bar plot
ggsave("AUC_within_region.png", plot = auc_p, path = auc_out_dir2,
       width = 6.5, height = 4, units = "in", dpi = 900)


# ------------------------
# AUC Heatmap 
# ------------------------

auc_all$trainRegion <- factor(auc_all$trainRegion, levels = rgn_names)
auc_all$projRegion <- factor(auc_all$projRegion, levels = rgn_names)

auc_heatmap <- ggplot(auc_all, aes(x = projRegion, y = trainRegion, fill = mean_auc)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_auc, 2)), size = 2.5) +
  scale_fill_gradient2(low = "#fef0d9", mid = "#fc8d59", high = "#b30000", midpoint = 0.75, na.value = "#cccccc") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7)) +
  labs(x = "Projected Region", y = "Training Region", fill = "Mean AUC")

# Plot with static midpoint (0.75)
auc_heatmap <- ggplot(auc_all, aes(x = projRegion, y = trainRegion, fill = mean_auc)) +
  geom_tile(color = "white") +
  geom_text(aes(label = format(round(mean_auc, 2), nsmall = 2)), size = 2.5) +
  scale_fill_gradient2(low = "#fff5eb", mid = "#fc8d59", high = "#990000", 
                       midpoint = 0.75) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7)) +
  labs(x = "Projected Region", y = "Training Region", fill = "Mean AUC")
auc_heatmap

ggsave("AUC_heatmap.png", plot = auc_heatmap, path = auc_out_dir2,
       width = 6, height = 5.5, units = "in", dpi = 900)

