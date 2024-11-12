# Load required packages
library(parallel)
library(terra)
library(dplyr)

# Define working directories and file paths
dir <- "D:\\SoRockies\\"
pj_dir <- paste0(dir, "PJ\\")
exe_dir <- paste0(dir, "ClimateNA_v750\\")  # Update to your actual path
exe <- "ClimateNA_v7.50.exe"  # Path to the ClimateNA executable

# Ensure ClimateNA executable exists
if (!file.exists(paste0(exe_dir, exe))) stop("ClimateNA executable not found!")

# Set input directory for DEM files
in_dir <- paste0(mso_dir, "Data\\DEM\\cropped_dem\\")
if (!dir.exists(in_dir)) stop("DEM input directory not found!")

# List DEM files (assuming .tif files from GEE export)
in_files <- list.files(in_dir, pattern = "tif$", full.names = TRUE)
if (length(in_files) == 0) stop("No DEM files found in input directory!")
print(in_files)

# Set output directory for ClimateNA results
out_dir <- paste0(mso_dir, "Data\\Climate\\")
if (!dir.exists(out_dir)) dir.create(out_dir)

# Define climate periods for ClimateNA
norm_years <- seq(1980, 2022, by = 10)
norm_years <- paste0("Normal_", norm_years - 29, "_", norm_years, ".nrm")
periods <- c(norm_years, "13GCMs_ensemble_ssp126_2011-2040.gcm")

# Set MSY (Monthly/Season/Yearly) option for ClimateNA
msy <- "M"

# Detect available CPU cores
cores <- detectCores() - 1  # Use one less core to leave some resources free
if (cores <= 0) stop("No CPU cores available for parallel processing!")

# Function to run ClimateNA on a DEM file for a specific period
run_climateNA <- function(prd, out_dir_, msy, exe, exe_dir, in_file_) {
  out <- strsplit(prd, "[.]")[[1]][1]
  
  if (!dir.exists(paste0(out_dir_, "/", out, msy))) {
    # Command to call the ClimateNA executable with the input DEM and period
    cmd <- paste(
      shQuote(paste0(exe_dir, exe)),  # Executable path
      "-P", prd,                      # Climate period
      "-M", msy,                      # Monthly/Season/Yearly output option
      "-I", shQuote(in_file_),        # Input DEM file
      "-O", shQuote(out_dir_)         # Output directory
    )
    
    # Run the command
    system(cmd)
  }
}

# Main loop to process each DEM file
system.time({
  lapply(1:length(in_files), function(i) {
    in_file_ <- in_files[i]
    
    # Create an output name for the DEM (based on the file name)
    outname <- gsub(".tif", "", basename(in_file_))
    outname <- gsub("dem", "mso", outname)
    out_dir_ <- paste0(out_dir, outname)
    
    if (!dir.exists(out_dir_)) dir.create(out_dir_)
    
    if (!all(dir.exists(paste0(out_dir_, "/", gsub(".gcm", "M", gsub(".nrm", "M", periods)))))) {
      
      # Run ClimateNA for each period in parallel
      clust <- makeCluster(cores)
      parLapply(cl = clust, X = periods, fun = run_climateNA, 
                out_dir_ = out_dir_, msy = msy, exe = exe, exe_dir = exe_dir, 
                in_file_ = in_file_)
      stopCluster(clust)
    }
  })
})

# Output final message
cat("ClimateNA processing complete for all DEM files.\n")
