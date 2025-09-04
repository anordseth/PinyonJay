# ========================================================== X
# ========================================================== X
# ======================= MODEL FIT ======================== X
# ===================== PLOT OOB ERROR ===================== X
# ========================================================== X
# ========================================================== X

library(tidyverse)


# ---------------X
# ---- SET UP ----
# ---------------X

spp <- "PinyonJay"
spp_model <- "FebJul_1500m_FSM"

rgn_names <- c("BCR9", "BCR10", "BCR15", "BCR16", "BCR17", "BCR18", "BCR32", 
               "BCR33", "BCR34", "BCR35")

# Viridis palette to match across scripts
rgn_pal <- hcl.colors(length(rgn_names), "Viridis")
names(rgn_pal) <- rgn_names

# Directory setup
base_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R"
in_dir <- file.path(base_dir, "4_Analysis", spp_model)
out_dir <- file.path(base_dir, "5_Figures", spp_model, "Model_Fit")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Input file
(oob_fn <- file.path(in_dir, "OOB_error.csv"))
if (!file.exists(oob_fn)) stop("OOB_error.csv not found")


# -----------------------X
# ---- PLOT OOB Error ----
# -----------------------X

# Load the species-model OOB error data
oob_dat <- read_csv(oob_fn) 
head(oob_dat)

# # pull just the means/sd
# oob_dat <- oob_dat %>%
#   select(c(species, model_type, train_region, mean_oob, sd_oob, 
#            ci, minus, plus)) %>%
#   distinct()
# oob_dat

# Collapse to region-level summary only
oob_summary <- oob_dat %>%
  select(species, model_type, train_region, mean_oob, sd_oob, ci, minus, plus) %>%
  distinct() %>%
  mutate(train_region = factor(train_region, levels = rgn_names))

# Plot
p_oob <- ggplot(oob_summary, aes(x = train_region, fill = train_region)) +
  geom_bar(aes(y = mean_oob), stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = minus, ymax = plus), color = "grey25", linewidth = 0.5, 
                width = 0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = rgn_pal) +
  # ylim(c(0, 0.1)) +
  coord_cartesian(ylim = c(0, 0.075)) +
  theme_minimal() +
  theme(title = element_text(size = 9),
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = "none",
        panel.grid.major.x = element_blank()) +
  labs(y = "OOB Error Rate", x = "Region", fill = "")
p_oob

