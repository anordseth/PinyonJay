# Load required packages
pacman::p_load(dplyr, tidyr, here, ggplot2, sf, rnaturalearth, rnaturalearthdata, readr)

# Step 1: Read the Pinyon Jay range Geopackage
pinyon_jay_range <- st_read("1_Data/pinjay_range_2022/pinjay_range_2022.gpkg")

# Step 2: Create and adjust the bounding box
bbox <- st_bbox(pinyon_jay_range)
bbox["ymin"] <- bbox["ymin"] - 1  # Extend southern boundary (adjust value as needed)
bbox_sf <- st_as_sfc(bbox)

# Step 3: Add a buffer to the bounding box
buffer_distance <- 10000  # 10 km buffer
bbox_buffered <- st_buffer(bbox_sf, dist = buffer_distance)

# Step 4: Load North America map
north_america <- ne_countries(continent = "North America", scale = "medium", returnclass = "sf")

# Step 5: Visualize the data
ggplot() +
  geom_sf(data = north_america, fill = "gray90", color = "black", size = 0.2) +
  geom_sf(data = pinyon_jay_range, fill = "lightblue", color = "blue", alpha = 0.6) +
  geom_sf(data = bbox_buffered, fill = NA, color = "red", linetype = "dashed", size = 1) +
  coord_sf(
    xlim = c(st_bbox(bbox_buffered)[1], st_bbox(bbox_buffered)[3]),
    ylim = c(st_bbox(bbox_buffered)[2], st_bbox(bbox_buffered)[4])
  ) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

# Step 6: Save the buffered bounding box to a GeoJSON file
st_write(bbox_buffered, "PJRangeBB.geojson")

# Step 7: Save the bounding box corners to a CSV file
# Extract only the bounding box corners (4 or 5 points)
bbox_coords <- as.data.frame(matrix(c(
  bbox["xmin"], bbox["ymin"],  # Bottom-left
  bbox["xmin"], bbox["ymax"],  # Top-left
  bbox["xmax"], bbox["ymax"],  # Top-right
  bbox["xmax"], bbox["ymin"],  # Bottom-right
  bbox["xmin"], bbox["ymin"]   # Close the box (optional)
), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Longitude", "Latitude"))))

# Save the bounding box corners to a CSV file
write_csv(bbox_coords, "PJRangeBB.csv")

# Step 8: Verify outputs by visualizing GeoJSON and CSV
bbox_geojson <- st_read("PJRangeBB.geojson")
bbox_csv <- st_as_sf(bbox_coords, coords = c("Longitude", "Latitude"), crs = st_crs(north_america))

# ggplot() +
#   geom_sf(data = north_america, fill = "gray90", color = "black", size = 0.2) +
#   geom_sf(data = bbox_geojson, fill = NA, color = "red", linetype = "dashed", size = 1, alpha = 0.8) +
#   geom_sf(data = bbox_csv, color = "blue", size = 2, alpha = 0.8) +
#   theme_minimal() +
#   labs(
#     title = "Bounding Box Verification",
#     subtitle = "GeoJSON (red dashed) and CSV Coordinates (blue points) Over North America",
#     x = "Longitude",
#     y = "Latitude"
#   )


# Convert GeoPackage to GeoJSON for GEE upload
output_geojson_path <- "1_Data/pinjay_range_2022/pinjay_range_2022.geojson"
st_write(pinyon_jay_range, output_geojson_path, driver = "GeoJSON")

# Convert GeoPackage to Shapefile for GEE upload
output_shapefile_path <- "1_Data/pinjay_range_2022/shapefile/"
if (!dir.exists(output_shapefile_path)) dir.create(output_shapefile_path, recursive = TRUE)
st_write(pinyon_jay_range, paste0(output_shapefile_path, "pinjay_range_2022.shp"), driver = "ESRI Shapefile")


