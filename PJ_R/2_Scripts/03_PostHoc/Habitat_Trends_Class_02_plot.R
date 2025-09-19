# =============================================================
# ================== PLOT HABITAT TRENDS ======================
# ======= BASED ON CLASSIFIED AREA FOR PINYON JAY ============
# =============================================================

library(tidyverse)
library(cowplot)


# === Setup === #

# Set base working directory and output paths
base_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R"
spp <- "PinyonJay"
spp_model <- "FebJul_1500m_FSM"

(in_dir <- file.path(base_dir, "4_Analysis", spp_model, "Habitat_Trends"))
(out_dir <- file.path(base_dir, "5_Figures", spp_model, "Habitat_Trends"))
(hab_trends_fn <- file.path(in_dir, "Habitat_Trends_classified_prepped.rds"))

# Load data
hab_trend <- readRDS(hab_trends_fn)

# Define color palette
hab_colors <- c("Low" = "#440154", "Moderate" = "#26828e", "High" = "#b5de2b")

# Y-axis helper
y_axis_prep <- function(df, area_col){
  vals <- df[[area_col]]
  vals <- vals[!is.na(vals) & is.finite(vals)]
  if (length(vals) == 0) return(list(range_log = numeric(), range = numeric()))
  max_log <- ceiling(max(vals))
  min_log <- floor(min(vals))
  if (!is.finite(min_log) || !is.finite(max_log)) return(list(range_log = numeric(), range = numeric()))
  range_log <- seq(min_log, max_log)
  range <- signif((exp(range_log)/1000), 1)
  list(range_log = range_log, range = range)
}

# Function to combine full + regional plots
# draw_combined <- function(full_plot, region_plot, width_ratio = c(0.4, 0.6)) {
#   cowplot::ggdraw() +
#     draw_plot(full_plot + theme(legend.position = "none"), width = width_ratio[1]) +
#     draw_plot(region_plot + theme(legend.position = "none"), x = width_ratio[1], width = width_ratio[2])
# }

draw_combined <- function(full_plot, region_plot, width_ratio = c(0.4, 0.6)) {
  cowplot::ggdraw() +
    draw_plot(full_plot, width = width_ratio[1]) +  # DO NOT remove the legend
    draw_plot(region_plot + theme(legend.position = "none"),
              x = width_ratio[1], width = width_ratio[2])
}


# === Habitat by Quality Class === #

sum_hab <- hab_trend %>%
  group_by(year, hab_qual_class) %>%
  dplyr::summarise(total_area_ha = sum(area_ha), .groups = "drop") %>%
  mutate(area_log = if_else(total_area_ha > 0, log(total_area_ha), NA_real_))

hab_trend_log <- hab_trend %>% mutate(log_area = if_else(area_ha > 0, log(area_ha), NA_real_))

sum_hab_range <- y_axis_prep(sum_hab, "area_log")
rgn_hab_range <- y_axis_prep(hab_trend_log, "log_area")

# Log scale
hab_by_quality_log <- ggplot(sum_hab, aes(x = year, y = area_log, group = hab_qual_class)) +
  geom_line(aes(color = hab_qual_class), linewidth = 0.75) +
  scale_color_manual(values = hab_colors) +
  scale_y_continuous(breaks = sum_hab_range$range_log, labels = sum_hab_range$range) +
  theme_bw() +
  theme(legend.position = c(0.97, 0.03),
        legend.justification = c("right", "bottom"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.75, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.box.background = element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Year", y = "Log Habitat Area (1000s ha)", color = "Habitat Quality")
hab_by_quality_log

rgn_by_quality_log <- ggplot(hab_trend_log, aes(x = year, y = log_area, group = hab_qual_class)) +
  facet_wrap(~region, nrow = 2) +
  geom_line(aes(color = hab_qual_class), linewidth = 0.75) +
  scale_color_manual(values = hab_colors) +
  scale_y_continuous(breaks = rgn_hab_range$range_log, labels = rgn_hab_range$range) +
  theme_bw() +
  guides(col = "none") +
  theme(title = element_text(size = 9), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.85, "cm"),
        legend.key.spacing.y = unit(0.01, "cm"),
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = "Log Habitat Area (1000s ha)", color = "Habitat Quality")

combined_by_quality_log <- draw_combined(hab_by_quality_log, rgn_by_quality_log)
combined_by_quality_log



ggsave(file.path(out_dir, "combined_by_quality_log.png"), plot = combined_by_quality_log,
       width = 8.5, height = 5, dpi = 300)


# # Raw scale
# hab_by_quality_raw <- ggplot(sum_hab, aes(x = year, y = total_area_ha / 1000, group = hab_qual_class)) +
#   geom_line(aes(color = hab_qual_class), linewidth = 0.75) +
#   scale_color_manual(values = hab_colors) +
#   theme_bw() +
#   labs(x = "Year", y = "Habitat Area (1000s ha)", color = "Habitat Quality")
# 
# rgn_by_quality_raw <- ggplot(hab_trend, aes(x = year, y = area_ha / 1000, group = hab_qual_class)) +
#   facet_wrap(~region, nrow = 2) +
#   geom_line(aes(color = hab_qual_class), linewidth = 0.75) +
#   scale_color_manual(values = hab_colors) +
#   theme_bw() +
#   guides(col = "none") +
#   theme(title = element_text(size = 9), 
#         # legend.position = "none",
#         legend.title = element_text(size = 8),
#         legend.text = element_text(size = 7),
#         legend.key.size = unit(0.85, "cm"),
#         legend.key.spacing.y = unit(0.01, "cm"),
#         strip.background = element_rect(fill = "white"),
#         axis.title = element_text(size = 8),
#         axis.text = element_text(size = 7),
#         strip.text = element_text(size = 8),
#         panel.grid.minor = element_blank()) +
#   labs(x = "Year", y = "Habitat Area (1000s ha)", color = "Habitat Quality")
# 
# combined_by_quality_raw <- draw_combined(hab_by_quality_raw, rgn_by_quality_raw)
# combined_by_quality_raw



# === All Habitat Combined === #

total_hab <- hab_trend %>%
  group_by(year) %>%
  summarise(total_area_ha = sum(area_ha), .groups = "drop") %>%
  mutate(area_log = log(total_area_ha))

total_by_region <- hab_trend %>%
  group_by(region, year) %>%
  summarise(total_area_ha = sum(area_ha), .groups = "drop") %>%
  mutate(area_log = log(total_area_ha))

total_range <- y_axis_prep(total_hab, "area_log")
rgn_total_range <- y_axis_prep(total_by_region, "area_log")

# Log scale
hab_total_log <- ggplot(total_hab, aes(x = year, y = area_log)) +
  geom_line(color = "black", linewidth = 0.75) +
  scale_y_continuous(breaks = total_range$range_log, labels = total_range$range) +
  theme_bw() +
  labs(x = "Year", y = "Habitat Area (1000s ha)")

rgn_total_log <- ggplot(total_by_region, aes(x = year, y = area_log)) +
  facet_wrap(~region, nrow = 2) +
  geom_line(color = "#1f9e89", linewidth = 0.75) +
  scale_y_continuous(breaks = rgn_total_range$range_log, labels = rgn_total_range$range) +
  theme_bw() +
  guides(col = "none") +
  theme(title = element_text(size = 9), 
        # legend.position = "none",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.85, "cm"),
        legend.key.spacing.y = unit(0.01, "cm"),
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        panel.grid.minor = element_blank()) +
  labs(x = "Year", y = "Habitat Area (1000s ha)")

combined_total_log <- draw_combined(hab_total_log, rgn_total_log)
combined_total_log

# Raw scale
hab_total_raw <- ggplot(total_hab, aes(x = year, y = total_area_ha / 1000)) +
  geom_line(color = "black", linewidth = 0.75) +
  theme_bw() +
  labs(x = "Year", y = "Habitat Area (1000s ha)")

rgn_total_raw <- ggplot(total_by_region, aes(x = year, y = total_area_ha / 1000)) +
  facet_wrap(~region, nrow = 2) +
  geom_line(color = "#1f9e89", linewidth = 0.75) +
  theme_bw() +
  guides(col = "none") +
  theme(title = element_text(size = 9), 
        # legend.position = "none",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.85, "cm"),
        legend.key.spacing.y = unit(0.01, "cm"),
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        panel.grid.minor = element_blank()) +
  labs(x = "Year", y = "Habitat Area (1000s ha)")

combined_total_raw <- draw_combined(hab_total_raw, rgn_total_raw)
combined_total_raw



# === Calculating % Change === #

# Total habitat change (full study area)
hab_change_total <- total_hab %>%
  arrange(year) %>%
  dplyr::summarise(start_year = first(year),
                   end_year = last(year),
                   area_start = first(total_area_ha),
                   area_end = last(total_area_ha),
                   change_ha = area_end - area_start,
                   percent_change = 100 * (area_end - area_start) / area_start)
print(hab_change_total)

# Total habitat change by quality
hab_change_total_qual <- sum_hab %>%
  arrange(year) %>%
  group_by(hab_qual_class) %>%
  dplyr::summarise(start_year = first(year),
                   end_year = last(year),
                   area_start = total_area_ha[year == first(year)],
                   area_end = total_area_ha[year == last(year)],
                   change_ha = area_end - area_start,
                   percent_change = 100 * (area_end - area_start) / area_start,
                   .groups = "drop")
print(hab_change_total_qual)

# Habitat change by BCR
hab_change_bcr <- total_by_region %>%
  arrange(region, year) %>%
  group_by(region) %>%
  dplyr::summarise(start_year = first(year),
                   end_year = last(year),
                   area_start = first(total_area_ha),
                   area_end = last(total_area_ha),
                   change_ha = area_end - area_start,
                   percent_change = 100 * (area_end - area_start) / area_start,
                   .groups = "drop")
print(hab_change_bcr)

# Habitat quality change by BCR
hab_change_by_quality <- hab_trend %>%
  group_by(region, hab_qual_class, year) %>%
  dplyr::summarise(area_ha = sum(area_ha), .groups = "drop") %>%
  group_by(region, hab_qual_class) %>%
  dplyr::summarise(area_start = area_ha[year == min(year)],
                   area_end = area_ha[year == max(year)],
                   change_ha = area_end - area_start,
                   percent_change = if_else(area_start == 0, NA_real_,
                                            100 * (area_end - area_start) / area_start),
                   .groups = "drop")
print(hab_change_by_quality, n = 30)



# Bar chart
ggplot(hab_change_by_quality, aes(x = region, y = change_ha / 1000, fill = hab_qual_class)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = hab_colors) +
  theme_bw() +
  labs(x = "Region", y = "Change in Habitat Area (1000s ha)", fill = "Habitat Quality")





# 1. Habitat change by BCR and quality class
hab_change_by_quality <- hab_trend %>%
  filter(year %in% c(1985, 2022)) %>%
  group_by(region, hab_qual_class, year) %>%
  dplyr::summarise(area_ha = sum(area_ha), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = area_ha, names_prefix = "year_") %>%
  mutate(change_ha = year_2022 - year_1985)

# 2. Habitat change across all BCRs by habitat quality
hab_change_all_bcrs <- hab_trend %>%
  filter(year %in% c(1985, 2022)) %>%
  group_by(hab_qual_class, year) %>%
  dplyr::summarise(area_ha = sum(area_ha), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = area_ha, names_prefix = "year_") %>%
  mutate(change_ha = year_2022 - year_1985,
         region = "All BCRs") %>%
  dplyr::select(region, hab_qual_class, year_1985, year_2022, change_ha)

# 3. Bind and preserve factor levels
# all_region_levels <- c("All BCRs", sort(unique(hab_change_by_quality$region)))

hab_change_combined <- bind_rows(hab_change_by_quality, hab_change_all_bcrs) 


ggplot(hab_change_combined, aes(x = region, y = change_ha / 1000, fill = hab_qual_class)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = hab_colors) +
  theme_bw() +
  labs(x = "Region", y = "Change in Habitat Area (1000s ha)", fill = "Habitat Quality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###


# Bind all regions, with 'All BCRs' treated as a separate facet
hab_change_combined <- bind_rows(hab_change_by_quality, hab_change_all_bcrs) %>%
  mutate(region_facet = if_else(region == "All BCRs", "All BCRs", "Individual BCRs"))

ggplot(hab_change_combined, aes(x = region, y = change_ha / 1000, fill = hab_qual_class)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = hab_colors) +
  theme_bw() +
  labs(
    x = "Region",
    y = "Change in Habitat Area (1000s ha)",
    fill = "Habitat Quality"
  ) +
  facet_wrap(~region_facet, scales = "free", ncol = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold")
  )



# =============================================================
# ======== ADDITIONAL VISUALIZATION OPTIONS ==================
# =============================================================

# 1. Percent Change by BCR (Bar Plot)
ggplot(hab_change_bcr, aes(x = region, y = percent_change)) +
  geom_col(fill = "#1f9e89") +
  theme_bw() +
  labs(x = "BCR", y = "Percent Change (1985–2022)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2. Bubble Plot of Percent Change vs Starting Habitat
ggplot(hab_change_bcr, aes(x = area_start/1000, y = percent_change, size = abs(change_ha))) +
  geom_point(color = "#26828e", alpha = 0.7) +
  theme_bw() +
  labs(x = "Habitat Area in 1985 (1000s ha)",
       y = "Percent Change (1985–2022)",
       size = "Absolute Change (ha)")

# 2b. Bubble Plot of Percent Change vs Percent of Starting Habitat (share of 1985 total)
hab_change_bcr_pct <- hab_change_bcr %>%
  mutate(pct_start_hab = 100 * area_start / sum(area_start, na.rm = TRUE))

ggplot(hab_change_bcr_pct, aes(x = pct_start_hab, y = percent_change, size = abs(change_ha))) +
  geom_point(color = "#26828e", alpha = 0.7) +
  theme_bw() +
  labs(x = "Percent of Total Habitat in 1985 (%)",
       y = "Percent Change (1985–2022)",
       size = "Absolute Change (ha)")


# 3. Scatterplot with Quality Breakdown (using hab_change_by_quality)
ggplot(hab_change_by_quality, aes(x = year_1985/1000, y = (change_ha/year_1985)*100, 
                                  size = abs(change_ha), color = hab_qual_class)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = hab_colors) +
  theme_bw() +
  labs(x = "Habitat Area in 1985 (1000s ha)",
       y = "Percent Change (1985–2022)",
       size = "Absolute Change (ha)",
       color = "Habitat Quality")


# 4. Slope Chart (Before vs After)
ggplot(hab_change_bcr, aes(x = start_year, y = area_start/1000, group = region)) +
  geom_line(aes(xend = end_year, yend = area_end/1000), stat = "identity") +
  geom_point(aes(y = area_start/1000), color = "#440154") +
  geom_point(aes(x = end_year, y = area_end/1000), color = "#b5de2b") +
  theme_bw() +
  labs(x = "Year", y = "Habitat Area (1000s ha)", title = "Slope Chart of Habitat Change by BCR")


# 5. Ranked Percent Change
ggplot(hab_change_bcr %>% 
         arrange(percent_change) %>% 
         mutate(region = factor(region, levels = region)),
       aes(x = region, y = percent_change)) +
  geom_col(fill = "#1f9e89") +
  coord_flip() +
  theme_bw() +
  labs(x = "BCR", y = "Percent Change (1985–2022)",
       title = "Ranked Percent Change by BCR")
