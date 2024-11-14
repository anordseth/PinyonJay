library(parallel)

getwd()

dir <- "D:\\"
mso_dir <- paste0(dir, "MSO\\")

exe_dir <- paste0(dir, "ClimateNA_v742\\")
exe <- "ClimateNA_v7.42.exe"
file.exists(paste0(exe_dir, exe))

in_dir <- paste0(mso_dir, "Data\\DEM\\cropped_dem\\")
dir.exists(in_dir)
(in_files <- list.files(in_dir, pattern = "asc"))
(in_files <- in_files[!grepl("xml", in_files)])
(in_files <- paste0(in_dir, in_files))
file.exists(in_files)

out_dir <- paste0(mso_dir, "Data\\Climate\\")
if(!dir.exists(out_dir)) dir.create(out_dir)

(norm_years <- seq(1980, 2022, by = 10))
(norm_years <- paste0("Normal_", norm_years-29, "_", norm_years, ".nrm"))
(periods <- c(norm_years, "13GCMs_ensemble_ssp126_2011-2040.gcm"))

msy <- "M"

detectCores()
(cores <- length(periods) * 7)


# Loop through each DEM chunk ----
func <- function(prd, out_dir_, msy, exe, exe_dir, in_file_){
  library(ClimateNAr)
  library(terra)
  
  out <- strsplit(prd, "[.]")[[1]][1]
  
  if(!dir.exists(paste0(out_dir_, "\\", out, msy))){
    
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
    outname <- gsub("dem", "mso", outname)
    out_dir_ <- paste0(out_dir, outname)
    if(!dir.exists(out_dir_)) dir.create(out_dir_)
    
    if(!all(dir.exists(paste0(out_dir_, "\\", gsub(".gcm", "M", (gsub(".nrm", "M", periods))))))){
      
      # Loop through each period in parallel
      clust <- makeCluster(cores)
      parLapply(cl = clust, X = periods, fun = func, 
                out_dir_ = out_dir_, msy = msy, exe = exe, exe_dir = exe_dir, 
                in_file_ = in_file_)
      stopCluster(clust)
    }
  })
})


