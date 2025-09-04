# ============================== #
# Pinyon Jay BCR Overview Figure #
# ============================== #

# Load packages
pacman::p_load(dplyr, ggplot2, sf, readr, viridis, cowplot, geojsonsf, rnaturalearth, rnaturalearthdata, ggspatial, terra)


# ============================== #
# Step 1: Load Range & Points #
# ============================== #

pj_range <- st_read("1_Data/Boundaries/pinjay_range_2022/pinjay_range_2022.gpkg") %>%
  st_transform(4326)

pj_points_df <- read_csv("/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/PJ_Data/eBird_PJ/ebird_pj_clean.csv")
pj_points <- pj_points_df %>%
  mutate(lon = longitude, lat = latitude) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Buffering in a projected CRS for accurate distance, then returning to WGS84
pj_range_buff <- st_transform(pj_range, 2163) %>%
  st_buffer(60000) %>%
  st_transform(4326)

pj_pts_clean <- st_filter(pj_points, pj_range_buff)


# ============================== #
# Step 2: Load and Prep BCRs #
# ============================== #

bcrs <- st_read("1_Data/Boundaries/BCRs.gdb") %>%
  st_transform(4326) %>%
  st_make_valid()

bcr_clip <- st_intersection(bcrs, pj_range_buff)
us_outline <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
bcr_clip <- st_intersection(bcr_clip, us_outline)
bcr_clip <- bcr_clip %>% mutate(name_en = ifelse(is.na(name_en), bcr_label_name, name_en))


# ============================== #
# Step 3: Filter BCRs by Points #
# ============================== #

pjpts_bcrs <- st_join(pj_pts_clean, bcr_clip)
pj_bcr_counts <- pjpts_bcrs %>% st_drop_geometry() %>% count(bcr_label, sort = TRUE)
bcr_clip <- bcr_clip %>% filter(bcr_label %in% pj_bcr_counts$bcr_label, bcr_label != '5')
pjpts_bcrs <- pjpts_bcrs %>% filter(bcr_label %in% bcr_clip$bcr_label)


# ============================== #
# Step 4: Load Forest Raster #
# ============================== #

# Option 1: Load raster
forest_raster_ds <- rast("Compressed_PinyonJay_ForestMask_1km.tif")

# Match CRS
map_crs_epsg <- sf::st_crs(bcr_clip)$epsg
raster_crs_epsg <- terra::crs(forest_raster_ds, describe = TRUE)$code
if (!is.na(map_crs_epsg) && !is.na(raster_crs_epsg) && raster_crs_epsg != map_crs_epsg) {
  forest_raster_ds <- terra::project(forest_raster_ds, paste0("EPSG:", map_crs_epsg))
}

# Crop and mask to BCRs
forest_cropped <- crop(forest_raster_ds, vect(bcr_clip))
forest_masked <- mask(forest_cropped, vect(bcr_clip))

# Convert to df for ggplot
forest_df <- as.data.frame(forest_masked, xy = TRUE, na.rm = TRUE)
colnames(forest_df)[3] <- "forest"
forest_df <- forest_df %>% filter(forest == 1)

# Option 2 (alternative): Use vector polygons instead of raster
forest_poly <- st_read("Compressed_PinyonJay_ForestMask_1km_Polygons.gpkg")
# forest_poly <- st_intersection(forest_poly, bcr_clip)  # optional: clip to BCRs


# --------------------------- #
# Step 5: Create Main + Inset #
# --------------------------- #

# Reproject map to projected CRS (EPSG:5070) for accurate scale bar?
reproject_for_scalebar <- FALSE
# ------------------------- #

bcr_union <- bcr_clip %>% 
  dplyr::summarise(.groups = "drop") %>% 
  st_union() %>% 
  st_as_sf()

# Temporarily project for accurate inset bounding box generation
bcr_union_proj <- st_transform(bcr_union, 5070)

bcr_bbox <- st_bbox(bcr_union_proj) %>% 
  st_as_sfc() %>% st_buffer(250000) %>% 
  st_transform(4326)

us_states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
inset_states <- st_intersection(us_states, bcr_bbox)

bcr_clip <- bcr_clip %>% 
  group_by(name_en) %>% 
  mutate(id = cur_group_id()) %>% 
  ungroup()

bcr_union_outline <- bcr_clip %>% 
  group_by() %>% 
  dplyr::summarise()

bcr_centroids <- st_centroid(bcr_clip) %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         bcr_label = bcr_clip$bcr_label,
         name_en = bcr_clip$name_en)

n_bcr <- nrow(bcr_centroids)
bcr_centroids$circle_fill <- viridis(n_bcr + 2, option = "D")[1:n_bcr]

if (reproject_for_scalebar) {
  target_crs <- 5070
  bcr_clip <- st_transform(bcr_clip, target_crs)
  bcr_union_outline <- st_transform(bcr_union_outline, target_crs)
  pjpts_bcrs <- st_transform(pjpts_bcrs, target_crs)
  bcr_centroids <- st_transform(bcr_centroids, target_crs)
  forest_df <- forest_df %>% mutate(x = x, y = y)  # skip reprojection of df
}

p_main <- ggplot() +
  # geom_tile(data = forest_df, aes(x = x, y = y), fill = "forestgreen", alpha = 0.3) +
  geom_sf(data = forest_poly, fill = "forestgreen", color = NA, alpha = 0.3) +
  geom_sf(data = bcr_clip, fill = NA, color = "black", linetype = "dashed", size = 0.1) +
  geom_sf(data = bcr_union_outline, fill = NA, color = "black", size = 1) +
  geom_sf(data = pjpts_bcrs, shape = 3, size = 0.4, color = "black", alpha = .3) +
  geom_label(data = bcr_centroids,
             aes(x = x, y = y, label = bcr_label, fill = circle_fill),
             color = "white", size = 3.5, fontface = "bold", label.size = 0) +
  scale_fill_identity() +
  annotation_scale(location = "bl", width_hint = 0.25, 
                   bar_cols = c("black", "white"), text_cex = 0.7, units = "km") +
  theme_void() +
  guides(fill = "none")
p_main

p_inset <- ggplot() +
  geom_sf(data = inset_states, fill = "white", color = "gray30", size = 0.2) +
  geom_sf(data = bcr_clip, fill = "gray70", color = "gray20", size = 0.4) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA))

legend_df <- bcr_centroids %>%
  st_drop_geometry() %>%
  select(bcr_label, name_en, circle_fill) %>%
  distinct() %>%
  mutate(label = paste(bcr_label, name_en, sep = ": "))

p_legend <- ggplot(legend_df, aes(x = 1, y = reorder(label, -as.numeric(bcr_label)), 
                                  label = label, fill = circle_fill)) +
  geom_label(aes(fill = circle_fill), color = "white", label.size = 0, size = 4.5, fontface = "bold", hjust = 0, label.padding = unit(0.3, "lines")) +
  scale_fill_identity() +
  xlim(1, 6) +
  theme_void()

final_map <- cowplot::plot_grid(p_main, p_inset, rel_widths = c(0.75, 0.25))

print(final_map)
print(p_legend)

