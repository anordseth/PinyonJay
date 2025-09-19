# ======================================================== #
# Compress and Save Forest Raster for Pinyon Jay Mapping  #
# ======================================================== #

library(terra)
library(sf)
library(ggplot2)

# Define directory where raw forest mask tiles are located
tile_dir <- "/Users/aen/Documents/ORISE_Postdoc/PinyonJayMacroecology/LivingMaps/PinyonJay/PJ_R/1_Data/Boundaries/ForestRaster"

tile_paths <- list.files(tile_dir,
                         pattern = "Combined_RefinedForestMask.*\\.tif$",
                         full.names = TRUE)

# Load tiles
forest_tiles <- lapply(tile_paths, rast)

# Use the largest tile as reference to preserve extent
tile_areas <- sapply(forest_tiles, function(x) prod(res(x)) * ncell(x))
sorted_indices <- order(tile_areas)
forest_tiles <- forest_tiles[sorted_indices]
ref_tile <- forest_tiles[[length(forest_tiles)]]

# Align tiles to reference grid
forest_tiles_aligned <- lapply(forest_tiles, function(x) {
  if (!compareGeom(x, ref_tile, stopOnError = FALSE)) {
    resample(x, ref_tile, method = "near")
  } else {
    x
  }
})

# Mosaic tiles using built-in max function (NA-safe)
forest_raster <- do.call(mosaic, c(forest_tiles_aligned, list(fun = "max")))

# Downsample to ~1km resolution (~33x33 of 30m pixels)
forest_raster_ds <- aggregate(forest_raster, fact = 33, fun = "max")

# Save compressed raster to disk for future reuse
writeRaster(forest_raster_ds, 
            filename = "Compressed_PinyonJay_ForestMask_1km.tif",
            overwrite = TRUE,
            wopt = list(gdal = c("COMPRESS=DEFLATE", "TILED=YES")))

# Also convert to vector polygon outline for visualization
forest_poly <- forest_raster_ds %>%
  as.polygons() %>%
  st_as_sf()

forest_col <- names(forest_poly)[which(names(forest_poly) != "geometry")]
forest_poly <- forest_poly %>% filter(.data[[forest_col]] == 1)

st_write(forest_poly, "Compressed_PinyonJay_ForestMask_1km_Polygons.gpkg",
         delete_dsn = TRUE)  # overwrite if exists

# Optional: Visualize the raster
plot(forest_raster_ds, main = "Compressed Forest Mask Raster (~1km)", col = c("white", "forestgreen"))

# Optional: Visualize the polygons
ggplot(forest_poly) +
  geom_sf(fill = "forestgreen", color = NA) +
  theme_void() +
  ggtitle("Compressed Forest Mask Polygons (~1km)")
