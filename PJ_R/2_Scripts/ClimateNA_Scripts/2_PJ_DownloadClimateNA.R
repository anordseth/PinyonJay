# Load required libraries
library(parallel)
library(terra)
library(dplyr)
library(ClimateNAr)

# Set up director.ies
base_dir <- "D:\\"
pj_dir <- paste0(base_dir, "LivingMaps_PJ\\")
exe_dir <- paste0(base_dir, "ClimateNA_v750\\")
exe <- "ClimateNA_v7.50.exe"  

in_dir <- paste0(pj_dir, "StateDEMs\\cropped_dems\\")
out_dir <- paste0(pj_dir, "ClimateNA_Outputs\\")

# List all DEM files
all_files <- list.files(in_dir, pattern = "asc$", full.names = TRUE)

# Extract state names from file names
state_names <- unique(gsub("DEM_([^_]+)_grid_.*\\.asc", "\\1", basename(all_files)))

# Define climate periods for ClimateNA
norm_years <- seq(1980, 2022, by = 10)
norm_years <- paste0("Normal_", norm_years - 29, "_", norm_years, ".nrm")
periods <- c(norm_years, "13GCMs_ensemble_ssp126_2011-2040.gcm")

# Set MSY (Monthly/Season/Yearly) option for ClimateNA
msy <- "M"

# Limit cores to available processors
cores <- min(length(periods) * 7, detectCores() - 1)

# Function to run ClimateNA on a DEM file for a specific period
func <- function(prd, out_dir, msy, exe, exe_dir, in_file) {
  
  library(terra)
  library(dplyr)
  library(ClimateNAr)
  
  # Generate output subdirectory
  out <- strsplit(prd, "[.]")[[1]][1]
  sub_dir <- paste0(out_dir, paste0(out, msy))
  
  # Skip processing if output already exists
  if (!dir.exists(sub_dir)) {
    dir.create(sub_dir, recursive = TRUE)
    
    # Run ClimateNA
    ClimateNA_cmdLine(
      exe = paste0(exe_dir, exe),
      wkDir = exe_dir,
      period = prd,
      MSY = msy,
      inputFile = in_file,
      outputFile = sub_dir
    )
  }
}

# Process each state's DEM files separately
for (state in state_names) {
  state_files <- all_files[grep(paste0("DEM_", state, "_grid_"), all_files)]
  state_out_dir <- paste0(out_dir, state, "\\")
  if (length(state_files) == 0) {
    cat("No DEM files found for", state, "\n")
    next
  }
  
  # Create state output directory if it doesn't exist
  if (!dir.exists(state_out_dir)) dir.create(state_out_dir, recursive = TRUE)
  
  # Start processing DEM files for the state
  system.time({
    lapply(seq_along(state_files), function(i) {
      
      # Track progress
      progress <- paste0("Processing ", state, ": ", i, "/", length(state_files))
      percentage <- signif((i / length(state_files)) * 100, 2)
      cat(progress, "(", percentage, "%)", "\r")
      in_file <- state_files[i]
      
      # Generate output directory for current file
      outname <- gsub(".asc", "", basename(in_file))
      outname <- gsub("DEM", "pj", outname)
      file_out_dir <- paste0(state_out_dir, outname, "\\")
      
      # Create output directory if it doesn't exist
      if (!dir.exists(file_out_dir)) dir.create(file_out_dir, recursive = TRUE)
      
      # Skip processing if outputs for all periods exist
      if (!all(file.exists(paste0(file_out_dir, gsub(".gcm", "M", gsub(".nrm", "M", periods)))))) {
        
        # Parallel processing for climate periods
        clust <- makeCluster(cores)
        parLapply(clust, periods, func,
                  out_dir = file_out_dir, msy = msy, exe = exe,
                  exe_dir = exe_dir, in_file = in_file)
        stopCluster(clust)
      }
    })
  })
  cat("\nClimateNA processing complete for", state, "\n")
  
  
  
}
cat("ClimateNA processing complete for all states.\n")











