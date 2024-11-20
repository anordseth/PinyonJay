
# Load required libraries
library(parallel)
library(terra)
library(ClimateNAr)

# Set up directories for Windows
base_dir <- "C:\\Users\\annanordseth\\Documents\\LivingMaps\\"
# pj_dir <- file.path(base_dir, "PinyonJay")
pj_dir <- paste0(base_dir, "PinyonJay")
# exe_dir <- file.path(base_dir, "ClimateNA_v750")
exe_dir <- paste0(base_dir, "ClimateNA_v750\\")
exe <- "ClimateNA_v7.50.exe"
# in_dir <- file.path(pj_dir, "1_Data", "ShapesTIFs", "cropped_dem")
in_dir <- paste(pj_dir, "1_Data", "ShapesTIFs", "cropped_dem\\", sep = "\\")
# out_dir <- file.path(pj_dir, "1_Data", "Climate")
out_dir <- paste(pj_dir, "1_Data", "Climate", sep = "\\")

# Verify directories
if (!file.exists(file.path(exe_dir, exe))) stop("ClimateNA executable not found!")
if (!dir.exists(in_dir)) stop("DEM input directory not found!")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# List DEM files, excluding any with 'xml'
in_files <- list.files(in_dir, pattern = "asc$", full.names = TRUE)
if (length(in_files) == 0) stop("No DEM files found in input directory!")

# Define climate periods for ClimateNA
norm_years <- seq(1990, 2022, by = 10)
norm_years <- paste0("Normal_", norm_years - 29, "_", norm_years, ".nrm")
periods <- c(norm_years, "13GCMs_ensemble_ssp126_2011-2040.gcm")

# Set MSY (Monthly/Season/Yearly) option for ClimateNA
msy <- "M"

# Limit cores to prevent system overload
cores <- min(length(periods) * 7, detectCores() - 2)

# Function to run ClimateNA on a DEM file for a specific period
func <- function(prd, out_dir_, msy, exe, exe_dir, in_file_) {
  library(ClimateNAr)
  library(terra)
  
  out <- strsplit(prd, "[.]")[[1]][1]
  
  if (!dir.exists(paste0(out_dir_, "\\", out, msy))) {
    
    # Run ClimateNA
    ClimateNA_cmdLine(
      exe = exe,
      wkDir = exe_dir,
      period = prd,
      MSY = msy,
      inputFile = in_file_,
      outputFile = out_dir_
    )
  }
}


# Start processing DEM files
system.time({
  lapply(seq_along(in_files), function(i) {
    # Track progress
    progress <- paste0(i, "/", length(in_files))
    percentage <- signif((i / length(in_files)) * 100, 2)
    cat(progress, "(", percentage, "%)", "\r")
    # Current input file
    in_file_ <- in_files[i]
    # Extract and modify file name
    outname <- basename(in_file_) # Extracts file name
    outname <- gsub(".asc", "", outname) # Removes .asc
    outname <- gsub("dem", "pj", outname) # Replaces "dem" with "pj"
    # Create unique output directory
    # out_dir_ <- file.path(out_dir, outname)
    out_dir_ <- paste(out_dir, outname, sep = "\\")
    if (!dir.exists(out_dir_)) dir.create(out_dir_, recursive = TRUE)
    # Skip processing if outputs for all periods exist
    if (!all(file.exists(file.path(out_dir_, gsub(".gcm", "M", gsub(".nrm", "M", periods)))))) {
      # Parallel processing for climate periods
      clust <- makeCluster(cores)
      tryCatch({
        parLapply(cl = clust, X = periods, fun = func,
                  out_dir_ = out_dir_, msy = msy, exe = exe,
                  exe_dir = exe_dir, in_file_ = in_file_)
      }, finally = {
        stopCluster(clust) 
      })
    }
  })
})

cat("ClimateNA processing complete for all DEM files.\n")





