# Load necessary packages
install.packages("elevatr")
install.packages("raster")
install.packages("sf")
install.packages("progress")

getwd()

library(elevatr)
library(raster)
library(sf)
library(progress)

# Step 1: Load the Level III Ecoregions Shapefile
# Replace 'path_to_shapefile' with the actual path to your downloaded ecoregions shapefile
ecoregions <- st_read("./1_Data/ShapesTIFs/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp")

# Step 2: Filter for the Southern Rockies Ecoregion
# The Southern Rockies ecoregion typically has a specific LEVEL3_CODE or NAME depending on your data
southern_rockies <- ecoregions[ecoregions$US_L3NAME == "Southern Rockies", ]

# Step 3: Download DEM within the Southern Rockies Boundary
# Define a buffer around the ecoregion if needed for additional coverage
southern_rockies_buffer <- st_buffer(southern_rockies, dist = 0.1)  # Adjust distance as needed

# Download the DEM using the buffered boundary and clip to exact boundary
dem_southern_rockies <- get_elev_raster(locations = southern_rockies_buffer, z = 10, clip = "locations")

# Clip DEM to exact ecoregion boundary
dem_southern_rockies_clipped <- mask(dem_southern_rockies, southern_rockies)

# Step 4: Plot the Clipped DEMhttp://127.0.0.1:9161/graphics/b3096b8d-c00c-4b1f-83b7-3af1eafd2049.png
plot(dem_southern_rockies_clipped, main = "Digital Elevation Model of the Southern Rockies",
     xlab = "Longitude", ylab = "Latitude", col = terrain.colors(100))

writeRaster(dem_southern_rockies_clipped, "./1_Data/ShapesTIFs/SoRockiesDEM.tif", format = "GTiff")
