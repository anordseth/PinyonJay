# Install necessary packages (uncomment if not already installed)
# install.packages("devtools")
# devtools::install_local('path/to/ClimateNAr.zip') # Replace with actual path

# Load required libraries
library(parallel)
library(terra)
library(dplyr)
library(ClimateNAr) # Ensure ClimateNAr is loaded globally

# Set up directories for Windows machine
base_dir <- "C:\\Users\\annanordseth\\Documents\\"
pj_dir <- file.path(base_dir, "LivingMaps", "PinyonJay")
exe_dir <- file.path(base_dir, "ClimateNA_v750")
exe <- "ClimateNA_v7.50.exe"  # Update if the ClimateNA version differs

in_dir <- file.path(pj_dir, "1_Data", "ShapesTIFs", "cropped_dem")
out_dir <- file.path(pj_dir, "1_Data", "Climate")

# Verify the ClimateNA executable and DEM input directory
if (!file.exists(file.path(exe_dir, exe))) stop("ClimateNA executable not found!")
if (!dir.exists(in_dir)) stop("DEM input directory not found!")

# List DEM files, excluding any with 'xml'
in_files <- list.files(in_dir, pattern = "asc$", full.names = TRUE)
if (length(in_files) == 0) stop("No DEM files found in input directory!")

# Create output directory if it doesn't exist
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

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
  # Generate output subdirectory
  out <- strsplit(prd, "[.]")[[1]][1]
  sub_dir <- file.path(out_dir, paste0(out, msy))
  
  # Skip processing if output already exists
  if (!dir.exists(sub_dir)) {
    dir.create(sub_dir, recursive = TRUE)
    
    # Run ClimateNA
    ClimateNA_cmdLine(
      exe = file.path(exe_dir, exe),
      wkDir = exe_dir,
      period = prd,
      MSY = msy,
      inputFile = in_file,
      outputFile = sub_dir
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
    
    in_file <- in_files[i]
    
    # Generate output directory for current file
    outname <- gsub(".asc", "", basename(in_file))
    outname <- gsub("dem", "pj", outname)
    file_out_dir <- file.path(out_dir, outname)
    
    # Create output directory if it doesn't exist
    if (!dir.exists(file_out_dir)) dir.create(file_out_dir, recursive = TRUE)
    
    # Skip processing if outputs for all periods exist
    if (!all(file.exists(file.path(file_out_dir, gsub(".gcm", "M", gsub(".nrm", "M", periods)))))) {
      # Parallel processing for climate periods
      clust <- makeCluster(cores)
      parLapply(clust, periods, func,
                out_dir = file_out_dir, msy = msy, exe = exe, 
                exe_dir = exe_dir, in_file = in_file)
      stopCluster(clust)
    }
  })
})

cat("ClimateNA processing complete for all DEM files.\n")





###################################





# install.packages("devtools")
# install.packages('path/ClimateNAr.zip', repos=NULL, type='source')

# Load required libraries
library(parallel)
library(terra)
library(dplyr)

# Set up directories based on OS
if (.Platform$OS.type == "unix") {  # macOS/Linux paths
  base_dir <- "/Users/aen/Documents/ORISE Postdoc/LivingMaps"
  pj_dir <- file.path(base_dir, "PinyonJay")
  exe_dir <- file.path(base_dir, "ClimateNA_v750")  # Adjust if ClimateNA is installed elsewhere
  exe <- "ClimateNA_v7.50.exe"  # Update if the ClimateNA version differs
  
  in_dir <- file.path(pj_dir, "1_Data/ShapesTIFs/cropped_dem/")
  out_dir <- file.path(pj_dir, "1_Data/Climate/")
} else {  # Windows paths
  base_dir <- "C:\\Users\\annanordseth\\Documents\\"
  pj_dir <- paste0(base_dir, "LivingMaps\\PinyonJay\\")
  exe_dir <- paste0(base_dir, "ClimateNA_v750\\")
  exe <- "ClimateNA_v7.50.exe" 
  
  in_dir <- paste0(pj_dir, "1_Data\\ShapesTIFs\\cropped_dem\\")
  out_dir <- paste0(pj_dir, "1_Data\\Climate\\")
}

# # Verify the ClimateNA executable and DEM directory
# if (!file.exists(file.path("C:\\Users\\annanordseth\\Documents\\LivingMaps\\ClimateNA_v750"))) stop("ClimateNA executable not found!")
# if (!dir.exists(in_dir)) stop("DEM input directory not found!")

# List DEM files, excluding any with 'xml'
in_files <- list.files(in_dir, pattern = "asc$", full.names = TRUE)
in_files <- in_files[!grepl("xml", in_files)]
if (length(in_files) == 0) stop("No DEM files found in input directory!")

# Create output directory if it doesn't exist
#if (!dir.exists(out_dir)) dir.create(out_dir)

# Define climate periods for ClimateNA
norm_years <- seq(1980, 2022, by = 10)
norm_years <- paste0("Normal_", norm_years - 29, "_", norm_years, ".nrm")
periods <- c(norm_years, "13GCMs_ensemble_ssp126_2011-2040.gcm")

# Set MSY (Monthly/Season/Yearly) option for ClimateNA
msy <- "M"

detectCores()
(cores <- length(periods) * 7)

# Function to run ClimateNA on a DEM file for a specific period
func <- function(prd, out_dir_, msy, exe, exe_dir, in_file_) {
  library(ClimateNAr)
  library(terra)
  
  out <- strsplit(prd, "[.]")[[1]][1]
  
  if (!dir.exists(paste0(out_dir_, "/", out, msy))) {

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

system.time({
  lapply(1:length(in_files), function(i){
    div <- paste0(i, "/", length(in_files))
    per <- signif((i/length(in_files))*100, 2)
    per <- paste0("(", per, "%)")
    cat(div, per, "\r")
    
    in_file_ <- in_files[i]
    
    outname <- strsplit(in_file_, "\\\\")[[1]][6]
    outname <- gsub(".asc", "", outname)
    outname <- gsub("dem", "pj", outname)
    out_dir_ <- paste0(out_dir, outname)
    if(!dir.exists(out_dir_)) dir.create(out_dir_)
    
    if(!all(dir.exists(paste0(out_dir_, "/", gsub(".gcm", "M", (gsub(".nrm", "M", periods))))))){
      
      # Loop through each period in parallel
      clust <- makeCluster(cores)
      parLapply(cl = clust, X = periods, fun = func, 
                out_dir_ = out_dir_, msy = msy, exe = exe, exe_dir = exe_dir, 
                in_file_ = in_file_)
      stopCluster(clust)
    }
  })
})

cat("ClimateNA processing complete for all DEM files.\n")
