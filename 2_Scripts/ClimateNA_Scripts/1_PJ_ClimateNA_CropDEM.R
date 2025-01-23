# Load required packages
pacman::p_load(plyr, here, tidyr, ggplot2, terra, sf, dplyr)

################################################################################
#                            SET UP FILE PATHS
################################################################################

# Detect operating system and set base paths
if (.Platform$OS.type == "unix") {
  # Mac/Linux paths
  base_dir <- here("1_Data/StateDEMs")  # Directory where state DEMs are stored
} else {
  # Windows paths
  base_dir <- here("1_Data\\StateDEMs")  # Adjust for Windows path separator
}

# Define directories for DEM files and output
dem_dir <- file.path(base_dir)  # DEM directory
out_dir <- file.path(base_dir, "cropped_dems")  # Output directory for cropped DEMs
if (!dir.exists(out_dir)) dir.create(out_dir)

# Check paths to ensure they are correct
print(paste("DEM directory:", dem_dir))
print(paste("Output directory:", out_dir))

# List all .tif files in the DEM directory
dem_files <- list.files(dem_dir, pattern = "\\.tif$", full.names = TRUE)
if (length(dem_files) == 0) stop("No .tif files found in the specified directory.")

print(paste("Found", length(dem_files), "DEM files to process."))

################################################################################
#                            PROCESS EACH DEM
################################################################################

# Iterate through each DEM file
for (dem_path in dem_files) {
  print(paste("Processing DEM:", dem_path))
  
  # Load the current DEM
  dem <- rast(dem_path)
  
  # Plot the DEM to check if it loaded correctly (optional)
  plot(dem, main = paste("Digital Elevation Model:", basename(dem_path)),
       xlab = "Longitude", ylab = "Latitude", col = terrain.colors(100))
  
  # Print DEM and number of cells
  print(dem)
  ncell <- ncell(dem)
  print(paste("Number of cells in DEM:", ncell))
  
  # Step 1: Create the grid (as an sf object)
  grids <- st_make_grid(st_as_sfc(st_bbox(ext(dem))), n = c(20, 20))  # Create a 20x20 grid
  
  # Step 2: Convert to sf and mutate to add the ID column
  grids_sf <- st_as_sf(grids) %>%
    dplyr::mutate(id = 1:length(grids),
                  id = formatC(id, width = nchar(max(1:length(grids))), flag = "0"))
  
  # Plot DEM and grid
  plot(dem)
  plot(st_geometry(grids_sf), add = TRUE)
  
  # Split the sf object by the 'id' column
  grids_split <- split(grids_sf, grids_sf$id)
  
  ################################################################################
  #                            CROP DEM BASED ON GRID
  ################################################################################
  
  # Define the cropping function
  crop_dem <- function(grid, dem) {
    # Crop the DEM with the given grid
    dem_crop <- crop(dem, grid)
    
    # Turn 0 values into NA
    dem_crop[dem_crop == 0] <- NA
    
    # Check if all values are NA; skip if they are
    if (!all(is.na(values(dem_crop)))) {
      # Convert to dataframe to crop and remove empty space, then convert back to raster
      dem_crop_2 <- as.data.frame(dem_crop, xy = TRUE) %>%
        rast(., type = "xyz")
      crs(dem_crop_2) <- crs(dem_crop)  # Ensure CRS consistency
      
      return(dem_crop_2)
    }
  }
  
  # Crop the DEM based on the grid cells
  system.time({
    cropped_dems <- lapply(grids_split, crop_dem, dem = dem)
  })
  
  # Remove any NULL elements (i.e., empty crops)
  cropped_dems <- cropped_dems[!sapply(cropped_dems, is.null)]
  
  # Check the number of cells in each cropped DEM
  print("Summary of cells in cropped DEMs:")
  print(summary(sapply(cropped_dems, ncell)))
  print("Summary of relative cell counts (total vs. cropped):")
  print(summary(ncell / sapply(cropped_dems, ncell)))
  
  ################################################################################
  #                            SAVE CROPPED DEMs
  ################################################################################
  
  # Function to save the cropped DEMs as .asc files
  save_raster <- function(i) {
    fn <- file.path(out_dir, paste0(basename(dem_path), "_grid_", i, ".asc"))
    if (!file.exists(fn)) {
      dem <- cropped_dems[[i]]
      writeRaster(dem, fn, NAflag = -9999, overwrite = TRUE)
      temp <- readLines(fn)
      write.table(temp, fn, row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  }
  
  # Apply the save_raster function to all cropped DEMs
  lapply(seq_along(cropped_dems), save_raster)
}

################################################################################
#                            CLEAN UP UNNECESSARY FILES
################################################################################

# Remove .prj and .xml files (optional)
print("Removing unnecessary .prj and .xml files in the output directory.")
list.files(out_dir, pattern = "prj$", full.names = TRUE) %>% file.remove()
list.files(out_dir, pattern = "xml$", full.names = TRUE) %>% file.remove()

print("All done!")






