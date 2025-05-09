# Load required packages
pacman::p_load(dplyr, tidyr, here, ggplot2, sf, rnaturalearth, rnaturalearthdata, 
               readr, ggrepel, patchwork)

# Load boundaries
pj_range <- st_read("1_Data/Boundaries/pinjay_range_2022/pinjay_range_2022.gpkg")
na <- ne_countries(continent = "North America", scale = "medium", returnclass = "sf")


# Create eBird range bbox -------------------------------------------------

# Create and buffer bounding box
bbox <- st_bbox(pj_range)
bbox["ymin"] <- bbox["ymin"] - 1  # Extend southern boundary
bbox_sf <- st_as_sfc(bbox)
bbox_buffered <- st_buffer(bbox_sf, dist = 10000)  # 10 km buffer

# Viz
ggplot() +
  geom_sf(data = na, fill = "gray90", color = "black", size = 0.2) +
  geom_sf(data = pj_range, fill = "lightblue", color = "blue", alpha = 0.6) +
  geom_sf(data = bbox_buffered, fill = NA, color = "red", linetype = "dashed", size = 1) +
  coord_sf(xlim = st_bbox(bbox_buffered)[c("xmin", "xmax")],
           ylim = st_bbox(bbox_buffered)[c("ymin", "ymax")]) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

# # Save range bbox as geoJSON and shapefile
# st_write(pj_range, "1_Data/pinjay_range_2022/pinjay_range_2022.geojson", driver = "GeoJSON")

# shapefile_path <- "1_Data/pinjay_range_2022/shapefile/"
# if (!dir.exists(shapefile_path)) dir.create(shapefile_path, recursive = TRUE)
# st_write(pj_range, file.path(shapefile_path, "pinjay_range_2022.shp"), driver = "ESRI Shapefile")



# Compare L2 and BCR  -----------------------------------------------------

# PJ range
pj_range <- st_read("1_Data/Boundaries/pinjay_range_2022/pinjay_range_2022.gpkg")

# PJ detections
pj_points_df <- read_csv("/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/PJ_Data/eBird_PJ/ebird_pj_clean.csv")
pj_points <- st_as_sf(pj_points_df, coords = c("longitude", "latitude"), crs = 4326)

# Ensure geometries use the same CRS
pj_points <- st_transform(pj_points, st_crs(pj_range))

# Identify points outside the range
pj_points_outside <- pj_points[!st_within(pj_points, pj_range, sparse = FALSE), ]

# Count them
n_outside <- nrow(pj_points_outside)

# Add a column to flag if each point is within the PJ range
pj_points$in_range <- as.logical(st_within(pj_points, pj_range, sparse = FALSE))

# Example: Compare year distribution
pj_points %>%
  group_by(in_range) %>%
  summarise(mean_year = mean(year, na.rm = TRUE),
            n = n())

# Or for a binary/categorical variable, say protocol type
pj_points %>%
  count(protocol_type, in_range) %>%
  group_by(protocol_type) %>%
  mutate(prop = n / sum(n))


# Load and prep ecoregions
ecoregions <- st_read("1_Data/Boundaries/na_cec_eco_l2/NA_CEC_Eco_Level2.shp") %>%
  st_transform(st_crs(pj_points)) %>%
  st_make_valid()

# BCRs
bcr_path <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJayR/1_Data/Boundaries/BCRs.gdb"

bcrs <- st_read(bcr_path) %>%
  st_transform(st_crs(pj_points)) %>%
  st_make_valid() %>%
  mutate(name_en = case_when(
    bcr_label == "34" & is.na(name_en) ~ "Sierra Madre Occidental",
    bcr_label == "39" & is.na(name_en) ~ "Mexican Highlands",
    TRUE ~ name_en
  ))

# Trying to write to a GEE friendly format
st_write(bcrs, "bcrs_cleaned.shp", delete_dsn = TRUE)

# Join points to regions
pj_w_ecos <- pj_points %>%
  st_join(ecoregions, join = st_intersects) 

pj_w_bcrs <- pj_points %>%
  st_join(bcrs, join = st_intersects) 

# Filter regions to those w/ PJ pts
pj_ecos <- ecoregions %>%
  filter(NA_L2CODE %in% unique(pj_w_ecos$NA_L2CODE))

pj_bcrs <- bcrs %>%
  filter(bcr_label %in% unique(pj_w_bcrs$bcr_label))

# Create bounding box
pj_range_line <- st_boundary(pj_range)
point_bbox <- st_bbox(pj_points)

# Plot ecoregions and BCRs
eco_map <- ggplot() +
  geom_sf(data = pj_ecos, fill = "lightgreen", color = "darkgreen", size = 2, alpha = 0.5) +
  geom_sf(data = pj_points, color = "red", size = 0.5, alpha = 0.5) +
  geom_sf(data = pj_range_line, color = "goldenrod", size = 3) +
  coord_sf(xlim = c(point_bbox$xmin, point_bbox$xmax),
           ylim = c(point_bbox$ymin, point_bbox$ymax)) +
  theme_minimal() +
  labs(title = "Ecoregions with PJ Detections")

bcr_map <- ggplot() +
  geom_sf(data = pj_bcrs, fill = "lightblue", color = "navy", size = 2, alpha = 0.5) +
  geom_sf(data = pj_points, color = "red", size = 0.5, alpha = 0.5) +
  geom_sf(data = pj_range_line, color = "goldenrod", size = 3) +
  coord_sf(xlim = c(point_bbox$xmin, point_bbox$xmax),
           ylim = c(point_bbox$ymin, point_bbox$ymax)) +
  theme_minimal() +
  labs(title = "BCRs with PJ Detections")

# Combine and display
eco_map + bcr_map



# Count how many unique ecoregions and BCRs contain PJ points
n_ecoregions <- n_distinct(pj_ecos$NA_L2CODE)
n_bcrs <- n_distinct(pj_bcrs$bcr_label)

cat("Number of ecoregions with PJ detections:", n_ecoregions, "\n")
cat("Number of BCRs with PJ detections:", n_bcrs, "\n")





# -------- Option A: Filter ecoregions to PJ range before join -------- #

ecoregions_filtered <- ecoregions[st_intersects(ecoregions, pj_range, sparse = FALSE), ]

pj_with_ecos_filtered <- pj_points %>%
  st_join(ecoregions_filtered, join = st_intersects) %>%
  filter(!is.na(NA_L2CODE))  # remove unmatched

summary_filtered <- pj_with_ecos_filtered %>%
  st_drop_geometry() %>%
  group_by(NA_L2CODE, NA_L2NAME) %>%
  summarise(n_filtered = n(), .groups = "drop")


# -------- Option B: Join all ecoregions, no pre-filtering -------- #

pj_with_ecos_unfiltered <- pj_points %>%
  st_join(ecoregions, join = st_intersects) %>%
  filter(!is.na(NA_L2CODE))

summary_unfiltered <- pj_with_ecos_unfiltered %>%
  st_drop_geometry() %>%
  group_by(NA_L2CODE, NA_L2NAME) %>%
  summarise(n_unfiltered = n(), .groups = "drop")


# ================== COMPARISON ================== #

# Join the two summaries to compare
summary_comparison <- full_join(summary_filtered, summary_unfiltered, 
                                by = c("NA_L2CODE", "NA_L2NAME")) %>%
  mutate(n_filtered = replace_na(n_filtered, 0),
         n_unfiltered = replace_na(n_unfiltered, 0),
         difference = n_unfiltered - n_filtered)

print(summary_comparison)
# write_csv(summary_comparison, "output/ecoregion_point_counts_comparison.csv")

###

# Bring in BCRs
bcr_path <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJayR/1_Data/Boundaries/BCRs.gdb"

bcrs <- st_read(bcr_path) %>%
  st_transform(st_crs(pj_points)) %>%
  st_make_valid()

bcrs <- bcrs %>%
  mutate(name_en = case_when(
    bcr_label == "34" & is.na(name_en) ~ "Sierra Madre Occidental",
    bcr_label == "39" & is.na(name_en) ~ "Mexican Highlands",
    TRUE ~ name_en
  ))


# -------- Option A: Filter BCRs to PJ range before join -------- #

bcrs_filtered <- bcrs[st_intersects(bcrs, pj_range, sparse = FALSE), ]

pj_with_bcrs_filtered <- pj_points %>%
  st_join(bcrs_filtered, join = st_intersects) %>%
  filter(!is.na(bcr_label))

summary_filtered <- pj_with_bcrs_filtered %>%
  st_drop_geometry() %>%
  group_by(bcr_label, name_en) %>%
  summarise(n_filtered = n(), .groups = "drop")


# -------- Option B: Join all BCRs, no pre-filtering -------- #

pj_with_bcrs_unfiltered <- pj_points %>%
  st_join(bcrs, join = st_intersects) %>%
  filter(!is.na(bcr_label))

summary_unfiltered <- pj_with_bcrs_unfiltered %>%
  st_drop_geometry() %>%
  group_by(bcr_label, name_en) %>%
  summarise(n_unfiltered = n(), .groups = "drop")


# ================== COMPARISON ================== #

summary_comparison <- full_join(summary_filtered, summary_unfiltered,
                                by = c("bcr_label", "name_en")) %>%
  mutate(n_filtered = replace_na(n_filtered, 0),
         n_unfiltered = replace_na(n_unfiltered, 0),
         difference = n_unfiltered - n_filtered)

print(summary_comparison)
# write_csv(summary_comparison, "output/bcr_point_counts_comparison.csv")


# Plot L2 Ecoregions & BCRs vs PJ Range -----------------------------------

# PJ range boundary
pj_range_line <- st_boundary(pj_range)

# Bounding box based on PJ detections
point_bbox <- st_bbox(pj_points)

# ================== ECOREGION MAP ================== #
eco_map <- ggplot() +
  geom_sf(data = overlapping_ecos, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  geom_sf(data = pj_points, color = "red", size = 0.5) +
  geom_sf(data = pj_range_line, color = "goldenrod", size = 1) +
  coord_sf(xlim = c(point_bbox$xmin, point_bbox$xmax),
           ylim = c(point_bbox$ymin, point_bbox$ymax)) +
  theme_minimal() +
  labs(title = "Overlapping Ecoregions")

# ================== BCR MAP ================== #
bcr_map <- ggplot() +
  geom_sf(data = bcrs_filtered, fill = "lightblue", color = "navy", alpha = 0.5) +
  geom_sf(data = pj_points, color = "red", size = 0.5) +
  geom_sf(data = pj_range_line, color = "goldenrod", size = 1) +
  coord_sf(xlim = c(point_bbox$xmin, point_bbox$xmax),
           ylim = c(point_bbox$ymin, point_bbox$ymax)) +
  theme_minimal() +
  labs(title = "Overlapping BCRs")

# Combine maps
eco_map + bcr_map


###

# PJ detections by ecoregion
pj_ecoregion_counts <- pj_with_ecos %>%
  mutate(NA_L2CODE = as.numeric(NA_L2CODE)) %>%  # do it here, not in group_by
  st_drop_geometry() %>%
  group_by(NA_L2CODE, NA_L2NAME) %>%
  summarise(presence_count = n(), .groups = "drop") %>%
  arrange(desc(presence_count))

print(pj_ecoregion_counts)
# write_csv(pj_ecoregion_counts, "output/pj_counts_per_ecoregion.csv")


# PJ detections by BCR
pj_bcr_counts <- pj_with_bcrs %>%
  st_drop_geometry() %>%
  group_by(bcr_label, name_en) %>%
  summarise(presence_count = n(), .groups = "drop") %>%
  arrange(desc(presence_count))

print(pj_bcr_counts)
# write_csv(pj_bcr_counts, "output/pj_counts_per_bcr.csv")

