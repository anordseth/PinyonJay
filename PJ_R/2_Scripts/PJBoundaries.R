# Load packages
pacman::p_load(dplyr, tidyr, here, ggplot2, sf, rnaturalearth, rnaturalearthdata, 
               readr, ggrepel, patchwork, cowplot)

# Load eBird PJ range
pj_range <- st_read("1_Data/Boundaries/pinjay_range_2022/pinjay_range_2022.gpkg")

# Reproject for buffering
pj_range_proj <- st_transform(pj_range, crs = 2163)

# Buffer in meters
pj_range_buff_proj <- st_buffer(pj_range_proj, dist = 60000)  # 10 km

# Reproject back to WGS84
pj_range_buff <- st_transform(pj_range_buff_proj, crs = 4326)

# Bring in PJ detections
pj_points_df <- read_csv("/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/PJ_Data/eBird_PJ/ebird_pj_clean.csv")

# Make a copy before converting
pj_points <- pj_points_df %>%
  mutate(lon = longitude, lat = latitude) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  # geom_sf(data = na, fill = "gray90", color = "black", size = 0.2) +
  geom_sf(data = pj_range, fill = "lightblue", color = "blue", alpha = 0.5) +
  geom_sf(data = pj_range_buff, fill = NA, color = "red", size = 1) +
  geom_sf(data = pj_points, color = "purple", alpha = 0.25) +
  coord_sf(xlim = st_bbox(pj_range_buff)[c("xmin", "xmax")],
           ylim = st_bbox(pj_range_buff)[c("ymin", "ymax")]) +
  theme_minimal()

# Filter PJ points to buffered range
pj_pts_clean <- st_filter(pj_points, pj_range_buff)

# Load and prep BCRs
bcrs <- st_read("1_Data/Boundaries/BCRs.gdb") %>%
  st_transform(st_crs(pj_points)) %>%
  st_make_valid()

# Crop to buffered pj range 
er_clip <- st_intersection(ecoregions, pj_range_buff)

bcr_clip <- st_intersection(bcrs, pj_range_buff) # not sure why SMO is missing

bcr_clip <- bcr_clip %>%
  mutate(name_en = ifelse(is.na(name_en), "Sierra Madre Occidental", name_en))

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




