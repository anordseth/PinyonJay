
# Load packages
pacman::p_load(dplyr, tidyr, here, ggplot2, sf, rnaturalearth, rnaturalearthdata, 
               readr, ggrepel, patchwork, cowplot, geojsonsf, ggspatial, terra)


## STEP 1: Buffer range & filter points ##

# Load eBird PJ range
pj_range <- st_read("1_Data/Boundaries/pinjay_range_2022/pinjay_range_2022.gpkg")

# Reproject for buffering
pj_range_proj <- st_transform(pj_range, crs = 2163)

# Buffer in meters
pj_range_buff_proj_60km <- st_buffer(pj_range_proj, dist = 60000)  

# Reproject back to WGS84
pj_range_buff_60km <- st_transform(pj_range_buff_proj_60km, crs = 4326)

# Compare buffers 
ggplot() +
  geom_sf(data = pj_range, fill = NA, color = "black") +
  geom_sf(data = pj_range_buff_60km, fill = NA, color = "red", size = 1) +
  # geom_sf(data = pj_points, color = "purple", alpha = 0.25) +
  coord_sf(xlim = st_bbox(pj_range_buff_50km)[c("xmin", "xmax")],
           ylim = st_bbox(pj_range_buff_50km)[c("ymin", "ymax")]) +
  theme_minimal()


# Bring in PJ detections
pj_points_df <- read_csv("/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/PJ_Data/eBird_PJ/ebird_pj_clean.csv")
# I can't remember what is "clean" about this, 
# but they haven't been temporally restricted or thinned

# Make a copy before converting
pj_points <- pj_points_df %>%
  mutate(lon = longitude, lat = latitude) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Filter PJ points to buffered range
pj_pts_clean <- st_filter(pj_points, pj_range_buff_60km)

ggplot() +
  # geom_sf(data = na, fill = "gray90", color = "black", size = 0.2) +
  geom_sf(data = pj_range, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = pj_range_buff_60km, fill = NA, color = "red", size = 1) +
  geom_sf(data = pj_pts_clean, color = "purple", alpha = 0.2) +
  coord_sf(xlim = st_bbox(pj_range_buff_60km)[c("xmin", "xmax")],
           ylim = st_bbox(pj_range_buff_60km)[c("ymin", "ymax")]) +
  theme_minimal()



## STEP 2: Prep BCRs ##

# Load and prep BCRs
bcrs <- st_read("1_Data/Boundaries/BCRs.gdb") %>%
  st_transform(st_crs(pj_points)) %>%
  st_make_valid()

# First clip the BCRs to the buffered eBird PJ range
bcr_clip <- st_intersection(bcrs, pj_range_buff_60km)

# Load US boundaries & states
us_outline <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
us_states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

# Clip to US
bcr_clip2 <- st_intersection(bcr_clip, us_outline)

# Fill missing name_en values with bcr_label_name
bcr_clip2 <- bcr_clip2 %>%
  mutate(name_en = ifelse(is.na(name_en), bcr_label_name, name_en))



## STEP 3: Filter BCRs by PJ detections ##

# Join points to BCRs
pjpts_bcrs <- st_join(pj_pts_clean, bcr_clip2, join = st_intersects)

# Summarize counts by BCR
pj_bcr_counts <- pjpts_bcrs %>%
  st_drop_geometry() %>%
  count(bcr_label, sort = TRUE)

# Filter BCRs to those with PJ detections 
bcr_clip3 <- bcr_clip2 %>%
  filter(bcr_label %in% pj_bcr_counts$bcr_label,
         bcr_label != '5') # Removing 5 bc it only has 2 points

# Filter PJ dectections to those w/in fully clipped BCRs
pj_pts_clean2 <- st_filter(pjpts_bcrs, bcr_clip3)

# Repeat join and summarize
pjpts_bcrs2 <- st_join(pj_pts_clean, bcr_clip3, join = st_intersects)
pjpts_bcrs2 <- pjpts_bcrs2 %>% filter(!is.na(bcr_label))

pj_bcr_counts2 <- pjpts_bcrs2 %>%
  st_drop_geometry() %>%
  count(bcr_label, sort = TRUE)

ggplot() +
  geom_sf(data = bcr_clip3, fill = "lightblue", color = "blue", alpha = 0.5) +
  geom_sf(data = pjpts_bcrs2, color = "purple", alpha = 0.2) +
  coord_sf(xlim = st_bbox(pj_range_buff_50km)[c("xmin", "xmax")],
           ylim = st_bbox(pj_range_buff_50km)[c("ymin", "ymax")]) +
  theme_minimal()



## STEP 4: Visualize Filtered BCRs w/ Inset Map ##

# Dissolve for bounding box for inset map
bcr_union <- bcr_clip3 %>%
  summarise(.groups = "drop") %>%
  st_union() %>%
  st_as_sf()

# Project and buffer for bbox
bcr_union_proj <- st_transform(bcr_union, crs = 5070)
bcr_bbox <- st_bbox(bcr_union_proj) %>%
  st_as_sfc() %>%
  st_buffer(250000) %>%
  st_transform(crs = 4326)

# Clip US states to buffered bbox
inset_states <- st_intersection(us_states, bcr_bbox)

# Add region ID and label info
bcr_clip4 <- bcr_clip3 %>%
  group_by(name_en) %>%
  mutate(id = cur_group_id()) %>%
  ungroup()

# BCR outlines
bcr_union_outline <- bcr_clip4 %>%
  group_by() %>%
  summarise()

# BCR centroids for labeling
bcr_centroids <- st_centroid(bcr_clip4) %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  left_join(st_drop_geometry(bcr_clip3)[, c("bcr_label", "name_en")], by = "bcr_label")

# Use viridis palette for label fill
circle_fills <- viridis::viridis(n = nrow(bcr_centroids))
bcr_centroids <- bcr_centroids %>%
  mutate(circle_fill = circle_fills)

# pj_sample <- pj_pts_clean2 %>% slice_sample(n = 8000)

# replacing pj_sample w/ pts thinned w/ eBird S&T best practices 
ebird_thin <- read_csv("/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R/1_Data/PJ_Obs/ebird_pj_subsamp_1kmthin_febjul.csv")

# convert .geo to sf geometry
ebird_thin_sf <- geojson_sf(ebird_thin$.geo)

# adding in forest raster
# List the four tiles
tile_paths <- list.files("/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R/1_Data/Boundaries/ForestRaster",
  pattern = "PinyonJay_RawForestMask.*\\.tif$",
  full.names = TRUE)

# Load and mosaic
forest_tiles <- lapply(tile_paths, rast)

# Align all tiles to first tile's grid to avoid mosaic hanging
ref_tile <- forest_tiles[[1]]
forest_tiles_aligned <- lapply(forest_tiles, function(x) {
  if (!terra::compareGeom(x, ref_tile, stopOnError = FALSE)) {
    terra::resample(x, ref_tile, method = "near")
  } else {
    x
  }
})

forest_raster <- do.call(mosaic, forest_tiles_aligned)

# Reproject if needed
map_crs_epsg <- sf::st_crs(bcr_clip3)$epsg
raster_crs_epsg <- terra::crs(forest_raster, describe = TRUE)$code

if (!is.na(map_crs_epsg) && !is.na(raster_crs_epsg) && raster_crs_epsg != map_crs_epsg) {
  forest_raster <- terra::project(forest_raster, paste0("EPSG:", map_crs_epsg))
}

# Downsample to a coarser resolution (e.g., 1000m)
forest_raster_ds <- aggregate(forest_raster, fact = 33, fun = "max")  # ~30m × 33 ≈ 1km

bcr_vect <- vect(bcr_union_outline)
forest_cropped <- crop(forest_raster_ds, bcr_vect)
forest_masked <- mask(forest_cropped, bcr_vect)

# Then convert to dataframe
forest_df <- as.data.frame(forest_masked, xy = TRUE, na.rm = TRUE)
colnames(forest_df)[3] <- "forest"

forest_df <- forest_df %>% 
  filter(forest == 1)

# Main map
p_main <- ggplot() +
  geom_tile(data = forest_df, aes(x = x, y = y), fill = "forestgreen", alpha = 0.3) +
  geom_sf(data = bcr_clip3, fill = NA, color = "black", linetype = "dashed", size = 0.1) +
  geom_sf(data = bcr_union_outline, fill = NA, color = "black", size = 1) +
  # geom_sf(data = pj_sample, shape = 3, size = 0.4, color = "black", alpha = .3) +
  geom_sf(data = ebird_thin_sf, shape = 3, size = 0.4, color = "black", alpha = .3) +
  geom_label(data = bcr_centroids,
             aes(x = x, y = y, label = bcr_label, fill = circle_fill),
             color = "white", size = 3.5, fontface = "bold", label.size = 0) +
  scale_fill_identity() +
  theme_void() +
  theme(panel.grid = element_blank(), axis.title = element_blank()) +
  guides(fill = "none")
p_main


p_main <- p_main +
  geom_raster(data = forest_df, aes(x = x, y = y), fill = "forestgreen", alpha = 0.3)

# Inset map
# p_inset <- ggplot() +
#   geom_sf(data = inset_states, fill = "gray95", color = "gray80", size = 0.2) +
#   geom_sf(data = bcr_clip3, fill = "gray70", color = "gray20", size = 0.4) +
#   theme_void() +
#   theme(panel.background = element_rect(fill = "white", color = NA))
# p_inset

p_inset <- ggplot() +
  geom_sf(data = inset_states, fill = "white", color = "gray30", size = 0.2) +
  geom_sf(data = bcr_clip3, fill = "gray70", color = "gray20", size = 0.4) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA))
p_inset


# Create standalone legend with matching circle colors
legend_df <- bcr_centroids %>%
  st_drop_geometry() %>%
  select(bcr_label, bcr_label_name, circle_fill) %>%
  distinct() %>%
  mutate(label = paste(bcr_label, bcr_label_name, sep = ": "))

p_legend <- ggplot(legend_df, aes(x = 1, y = reorder(label, -as.numeric(bcr_label)), 
                                  label = label, fill = circle_fill)) +
  geom_label(aes(fill = circle_fill), color = "white", label.size = 0, size = 4.5, fontface = "bold", hjust = 0) +
  scale_fill_identity() +
  xlim(1, 6) +
  theme_void()

# this version is working, but I want a colored band w/ white text
# legend_df <- bcr_centroids %>%
#   st_drop_geometry() %>%
#   select(bcr_label, bcr_label_name, circle_fill) %>%
#   distinct() %>%
#   mutate(label = paste(bcr_label, bcr_label_name, sep = ": "))
# 
# p_legend <- ggplot(legend_df, aes(x = 1, y = reorder(label, as.numeric(bcr_label)), 
#                                   label = label)) +
#   geom_text(aes(color = circle_fill), hjust = 0, size = 4.5, fontface = "bold") +
#   scale_color_identity() +
#   xlim(1, 3.5) +
#   theme_void()

# Combine map and inset only
final_map <- plot_grid(p_main,
                       p_inset,
                       rel_widths = c(0.75, 0.25))

# Print separately
print(final_map)
print(p_legend)





# Trying the rasterVis approach 
# Convert to RasterLayer
forest_r <- raster(forest_masked)

# Plot
gplot(forest_r) +
  geom_tile(aes(fill = factor(value)), show.legend = FALSE) +
  scale_fill_manual(values = "forestgreen") +
  coord_equal()


###


# Convert masked raster to polygons (low-res for plotting)
forest_poly <- as.polygons(forest_masked, dissolve = TRUE)
forest_poly_sf <- st_as_sf(forest_poly)

# Replace forest layer in plot
p_main <- ggplot() +
  #geom_sf(data = forest_poly_sf, fill = "forestgreen", color = NA, alpha = 0.3) +  
  geom_sf(data = bcr_clip3, fill = NA, color = "black", linetype = "dashed", size = 0.1) +
  geom_sf(data = bcr_union_outline, fill = NA, color = "black", size = 1) +
  geom_sf(data = ebird_thin_sf, shape = 3, size = 0.4, color = "black", alpha = .3) +
  geom_label(data = bcr_centroids,
             aes(x = x, y = y, label = bcr_label, fill = circle_fill),
             color = "white", size = 3.5, fontface = "bold", label.size = 0) +
  scale_fill_identity() +
  annotation_scale(location = "bl", width_hint = 0.25, 
                   bar_cols = c("black", "white"), text_cex = 0.7, unit = "km") +
  theme_void() +
  theme(panel.grid = element_blank(), axis.title = element_blank()) +
  guides(fill = "none")
p_main



###
##
#



pres <- effort_final %>%
  filter(presence == 1)

pres_pts <- pres %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() + 
  geom_sf(data = bcr_clip3, fill = NA, color = "black", size = 0.1) + 
  geom_sf(data = pres_pts, shape = 3, size = 0.4, color = "black", alpha = .3)
  
  





###########
#######
###















# bcr_clip <- bcr_clip %>%
#   mutate(name_en = ifelse(is.na(name_en), "Sierra Madre Occidental", name_en))

# ggplot() +
#   geom_sf(data = bcr_clip, aes(fill = name_en), color = "white", size = 0.5) +
#   geom_sf(data = pj_pts_clean, color = "orange", alpha = 0.25) +
#   geom_sf_text(data = bcr_clip, aes(label = name_en), size = 3, color = "black") +
#   coord_sf(xlim = st_bbox(pj_range_buff)[c("xmin", "xmax")],
#            ylim = st_bbox(pj_range_buff)[c("ymin", "ymax")]) +
#   theme_minimal() +
#   scale_fill_viridis_d(name = "BCR Name")  

# Join filtered points and bcrs
pjpts_bcrs <- pj_pts_clean %>%
  st_join(bcr_clip, join = st_intersects) 
  
pjpts_bcrs_summ <- pjpts_bcrs %>%
  st_drop_geometry() %>%
  group_by(bcr_label, name_en) %>%
  summarise(no. = n(), .groups = "drop")

# Removing Northern Pacific Rainforest (bcr_label = 5) bc there are only 2 points
npr <- bcr_clip %>%
  filter(bcr_label == 5)

# bcr_clip2 <- bcr_clip %>%
#   filter(bcr_label != 5) %>%
#   mutate(id = row_number())
# 
# ggplot() +
#   geom_sf(data = bcr_clip2, aes(fill = name_en), color = "white", size = 0.5) +
#   geom_sf_label(data = bcr_clip2, aes(label = as.factor(id)), size = 3, color = "black") +
#   theme_minimal() +
#   scale_fill_viridis_d(name = "Bird Conservation Region")

bcr_clip2 <- bcr_clip %>%
  filter(name_en != "Northern Pacific Rainforest") %>%
  group_by(name_en) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(id_label = paste0(id, ": ", name_en))

# Create a unique, ordered lookup table for legend labels
legend_df <- bcr_clip2 %>%
  distinct(id, id_label) %>%
  arrange(id)


# Load US boundaries and states
us_outline <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
us_states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

ggplot() +
  geom_sf(data = us_states, fill = "grey") +
  geom_sf(data = bcrs, fill = NA, color = "purple") +
  coord_sf(xlim = st_bbox(pj_range_buff)[c("xmin", "xmax")],
           ylim = st_bbox(pj_range_buff)[c("ymin", "ymax")]) +
  theme_minimal()

# 1. Clip BCRs to the US outline
bcr_clip2_us <- st_intersection(bcr_clip2, us_outline)

# 2. Dissolve to one shape for bbox
bcr_union <- bcr_clip2_us %>%
  summarise(.groups = "drop") %>%
  st_union() %>%
  st_as_sf()

# 3. Project for clean buffering
bcr_union_proj <- st_transform(bcr_union, crs = 5070)  # NAD83 / Conus Albers

# 4. Create buffered bounding box and reproject back to lat/lon
bcr_bbox <- st_bbox(bcr_union_proj) %>%
  st_as_sfc() %>%
  st_buffer(250000) %>%            # 500 km buffer
  st_transform(crs = 4326)

# 5. Clip US states to buffered bbox
inset_states <- st_intersection(us_states, bcr_bbox)

# 6. Main map plot
p_main <- ggplot() +
  geom_sf(data = bcr_clip2_us, aes(fill = as.factor(id)), color = "white", size = 0.5) +
  geom_sf_label(data = bcr_clip2_us, aes(label = bcr_label), size = 3, color = "black") +
  theme_minimal() +
  scale_fill_viridis_d(name = "Bird Conservation Region",
                       labels = legend_df$id_label,
                       breaks = as.character(legend_df$id))

# 7. Inset map
p_inset <- ggplot() +
  geom_sf(data = inset_states, fill = "gray95", color = "black", size = 0.2) +
  geom_sf(data = bcr_clip2_us, fill = "gray40", color = NA, alpha = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA))

# 8. Combine main + inset
final_map <- ggdraw() +
  draw_plot(p_main) +
  draw_plot(p_inset, x = 0.65, y = 0.7, width = 0.3, height = 0.3)

# 9. Print the combined map
print(final_map)





# # Plot
# p_main <- ggplot() +
#   geom_sf(data = bcr_clip2, aes(fill = as.factor(id)), color = "white", 
#           size = 0.5) +
#   geom_sf_label(data = bcr_clip2, aes(label = id), size = 3, color = "black") +
#   theme_minimal() +
#   scale_fill_viridis_d(name = "Bird Conservation Region",
#                        labels = legend_df$id_label,
#                        breaks = as.character(legend_df$id))
# 
# # Dissolve BCRs into a single shape for bounding box & outline
# bcr_union <- bcr_clip2 %>%
#   summarise(.groups = "drop") %>%
#   st_union() %>%
#   st_as_sf()
# 
# # Create a bounding box to clip US states
# bcr_bbox <- st_bbox(bcr_union) %>% 
#   st_as_sfc() %>%
#   st_buffer(2)  # adjust units as needed
# 
# # Clip US states to that bounding box
# us_states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
# 
# inset_states <- st_intersection(us_states, bcr_bbox)
# 
# # Inset map
# p_inset <- ggplot() +
#   # clipped US states in light gray
#   geom_sf(data = inset_states, fill = "gray95", color = "black", size = 0.2) +
#   # dissolved BCRs in dark gray with no internal borders
#   geom_sf(data = bcr_clip2, fill = "gray40", color = NA, alpha = 0.75) +
#   theme_void() +
#   theme(panel.background = element_rect(fill = "white", color = NA))
# 
# p_inset
# 
# # Combine main + inset
# final_map <- ggdraw() +
#   draw_plot(p_main) +
#   draw_plot(p_inset, x = 0.65, y = 0.7, width = 0.3, height = 0.3)
# 
# # Print the combined map
# print(final_map)




