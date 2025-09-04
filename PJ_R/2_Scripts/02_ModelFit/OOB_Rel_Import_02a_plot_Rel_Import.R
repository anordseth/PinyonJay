# ========================================================== X
# ========================================================== X
# ================== VARIABLE IMPORTANCE =================== X
# ================ PLOT RELATIVE IMPORTANCE ================ X
# ========================================================== X
# ========================================================== X

library(tidyverse)
library(cowplot)

# ----------------------------X
# ---- SET SPECIES SETUP ----
# ----------------------------X

spp <- "PinyonJay"
spp_model <- "FebJul_1500m"
# pts_type <- "R10_rand"

# Full region names (10 BCRs)
rgn_names <- c("BCR9", "BCR10", "BCR15", "BCR16", "BCR17",
               "BCR18", "BCR32", "BCR33", "BCR34", "BCR35")

# Viridis palette to match across scripts
rgn_pal <- hcl.colors(length(rgn_names), "Viridis")
names(rgn_pal) <- rgn_names

# Environmental classes
env_class_pal <- c(
  "Topography" = "#f4a582",  
  "Climate" = "#3182bd",  
  "Reflectance"  = "#a6dba0"  
)


# Directory setup
base_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R"
(in_dir <- file.path(base_dir, "4_Analysis", spp_model))
(out_dir <- file.path(base_dir, "5_Figures", spp_model, "Model_Fit"))
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

rel_import_fn <- file.path(in_dir, "Rel_Import_full.rds") 
if (!file.exists(rel_import_fn)) stop("Rel_Import_full.rds not found")

rel_import <- readRDS(rel_import_fn) %>%
  mutate(train_region = factor(train_region, levels = rgn_names))


# ---- Summed Relative Importance Plot ----

rel_import_sum <- rel_import %>%
  dplyr::select(train_region, env_class, prop_env_class,
         sum_rel_import, prop_rel_import, mean_prop_rel_import,
         sd_prop_rel_import, ci_prop, minus_prop, plus_prop) %>%
  distinct() %>%
  mutate(x = as.numeric(env_class) - 0.5, xend = x + 1)

arrow <- rel_import_sum %>%
  filter(env_class == "Reflectance") %>%
  dplyr::select(env_class, x = xend, y = prop_env_class) %>%
  distinct() %>%
  mutate(x = x + 0.01, xend = x + 0.15, yend = y - 0.05, text = "Total Proportion")

rel_import_sum_p <- ggplot(rel_import_sum, aes(x = env_class)) +
  geom_bar(aes(y = mean_prop_rel_import, fill = train_region),
           stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = rgn_pal) +
  geom_segment(aes(y = prop_env_class, x = x, xend = xend, yend = prop_env_class),
               linewidth = 1) +
  geom_errorbar(aes(ymin = minus_prop, ymax = plus_prop, group = train_region),
                col = "grey55", linewidth = 0.5, width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_segment(data = arrow, aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(ends = "first", length = unit(0.05, "inches"))) +
  geom_text(data = arrow, aes(x = xend, y = yend, label = text),
            nudge_x = 0.1, nudge_y = -0.02, size = 7*(5/14)) +
  theme_minimal() +
  theme(title = element_text(size = 9), 
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "white"),
        legend.background = element_rect(color = "black", linewidth = 0.25),
        legend.key.size = unit(0.1, "in"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        panel.grid.major.x = element_blank()) +
  labs(y = "Relative Importance", x = "Environmental Class", fill = "Region")
rel_import_sum_p

# out_fig_sum <- file.path(out_dir, "Rel_Import_Sum.png")
# ggsave(out_fig_sum, plot = rel_import_sum_p, width = 6.5, height = 4, units = "in", dpi = 300)
# 
# print(paste("Saved summed relative importance plot to", out_fig_sum))


# ---- Full Relative Importance Plots ----

rel_import_vars <- rel_import %>%
  select(c(train_region, env_class, variable, variable_name,
           mean_rel_import_scale, sd_rel_import_scale, minus_rel_import, plus_rel_import)) %>%
  distinct() 

rel_import_split <- split(rel_import_vars, rel_import_vars$train_region)

# Non-coef plots
non_coef_plots <- lapply(rel_import_split, function(dat_region){
  if(nrow(dat_region) > 0){
    region <- unique(dat_region$train_region)
    dat_region %>%
      filter(!grepl("coef", variable_name)) %>%
      ggplot(aes(y = reorder(variable_name, mean_rel_import_scale))) +
      geom_bar(aes(x = mean_rel_import_scale, fill = env_class), stat = "identity") +
      scale_fill_manual(values = env_class_pal) +
      geom_linerange(aes(xmin = minus_rel_import, xmax = plus_rel_import),
                     linewidth = 0.5, position = position_dodge(width = 0.9)) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.25), n.breaks = 5,
                         labels = seq(0, 1, by = 0.25), limits = c(0, 1)) +
      theme_bw() +
      theme(strip.background = element_rect(fill = "white", color = NA),
            strip.text = element_text(hjust = -0.02, size = 10),
            axis.text = element_text(size = 6),
            legend.background = element_rect(color = "black", linewidth = 0.25),
            legend.key.size = unit(0.1, "in"),
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 6),
            panel.grid.major.y = element_blank()) +
      labs(title = region, y = "", x = "Relative Importance", fill = "Environmental Class") %>%
      return()
  }
})
non_coef_plots

non_coef_leg <- get_legend(non_coef_plots[[1]])
non_coef_plots <- lapply(non_coef_plots, function(plot){ 
  plot + theme(legend.position = "none") 
})
non_coef_align <- do.call(align_plots, c(non_coef_plots, list(align = "hv", 
                                                              axis = "tblr")))

non_coef_p <- do.call(plot_grid, non_coef_align)
non_coef_p <- ggdraw() + 
                draw_plot(non_coef_p) + 
                draw_grob(non_coef_leg, width = 0.1, height = 0.1, x = 0.665, y = 0.41)
non_coef_p

# ggsave("Rel_Import_Vars_Non_Coef.png", path = out_dir, plot = non_coef_p, 
#        width = 11, height = 8, units = "in", dpi = 300)


# Coef Plots
coef_plots <- lapply(rel_import_split, function(dat_region){
  if(nrow(dat_region) > 0){
    region <- unique(dat_region$train_region)
    dat_region %>%
      filter(grepl("coef", variable_name)) %>%
      ggplot(aes(y = reorder(variable_name, mean_rel_import_scale))) +
      geom_bar(aes(x = mean_rel_import_scale, fill = env_class), stat = "identity") +
      scale_fill_manual(values = env_class_pal) +
      geom_linerange(aes(xmin = minus_rel_import, xmax = plus_rel_import),
                     linewidth = 0.5, position = position_dodge(width = 0.9)) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.25), n.breaks = 5,
                         labels = seq(0, 1, by = 0.25), limits = c(0, 1)) +
      theme_bw() +
      theme(strip.background = element_rect(fill = "white", color = NA),
            strip.text = element_text(hjust = -0.02, size = 10),
            axis.text = element_text(size = 5),
            legend.background = element_rect(color = "black", linewidth = 0.25),
            legend.key.size = unit(0.1, "in"),
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 6),
            panel.grid.major.y = element_blank()) +
      labs(subtitle = region, y = "", x = "Relative Importance", fill = "Environmental Class") %>%
      return()
  }
})

coef_leg <- get_legend(coef_plots[[1]])
coef_plots <- lapply(coef_plots, function(plot){ plot + theme(legend.position = "none") })
coef_align <- do.call(align_plots, c(coef_plots, list(align = "hv", axis = "tblr")))
coef_p <- do.call(plot_grid, coef_align)

# ggsave("Rel_Import_Vars_Coef.png", path = out_dir, plot = coef_p, width = 11, height = 8, units = "in", dpi = 300)


# Top 10 Vars only
top10_plots <- lapply(rel_import_split, function(dat_region){
  if(nrow(dat_region) > 0){
    region <- unique(dat_region$train_region)
    dat_region %>%
      arrange(desc(mean_rel_import_scale)) %>%
      head(n = 10) %>%
      ggplot(aes(y = reorder(variable_name, mean_rel_import_scale))) +
      geom_bar(aes(x = mean_rel_import_scale, fill = env_class), stat = "identity") +
      scale_fill_manual(values = env_class_pal) +
      geom_linerange(aes(xmin = minus_rel_import, xmax = plus_rel_import),
                     linewidth = 0.5, position = position_dodge(width = 0.9)) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.25), n.breaks = 5,
                         labels = seq(0, 1, by = 0.25), limits = c(0, 1)) +
      theme_minimal() +
      theme(strip.background = element_rect(fill = "white", color = NA),
            strip.text = element_text(hjust = -0.02, size = 8),
            axis.title = element_text(size = 8),
            axis.text = element_text(size = 6),
            legend.background = element_rect(color = "black", linewidth = 0.25),
            legend.key.size = unit(0.1, "in"),
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 6),
            panel.grid.major.y = element_blank()) +
      labs(subtitle = region, y = "", x = "Relative Importance", fill = "Environmental Class") %>%
      return()
  }
})

top10_leg <- get_legend(top10_plots[[1]])
top10_plots <- lapply(top10_plots, function(plot){ plot + theme(legend.position = "none") })

# top10_align <- do.call(align_plots, c(top10_plots, list(align = "hv", axis = "tblr")))
# top10_p <- do.call(plot_grid, top10_align)

top10_align <- align_plots(plotlist = top10_plots, align = "hv", axis = "tblr")

top10_p <- plot_grid(plotlist = top10_align, ncol = 2, nrow = 5)
top10_p


# top10_p <- ggdraw() + draw_plot(top10_p) + 
#   draw_grob(non_coef_leg, width = 0.1, height = 0.1, x = 0.6825, y = 0.405)
top10_p <- plot_grid(
  top10_p,
  non_coef_leg,
  ncol = 1,
  rel_heights = c(1, 0.07)  # Adjust to make the legend smaller and nicely tucked at the bottom
)

top10_p

# ggsave("Rel_Import_Vars_Top10.png", path = out_dir, plot = top10_p, width = 8, 
#        height = 7, units = "in", dpi = 300)

