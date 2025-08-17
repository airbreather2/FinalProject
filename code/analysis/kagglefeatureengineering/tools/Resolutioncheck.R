library(terra)
library(sf)

# Set data path and filenames
data_path <- "../../../data/finaldatasets/covariates/Covariates"

file_names <- c(
  "aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif",
  "clay_0_30cm.tif", 
  "elevation_world.tif",
  "irrigated_gmia_v5_aei_pct.asc",
  "phh2o_0_30cm.tif",
  "sand_0_30cm.tif",
  "silt_0_30cm.tif", 
  "soc_0_30cm.tif",
  "soil_nitrogen_0_30cm.tif",
  "rast_adm2_gdp_perCapita_1990_2022.tif"
)

files <- file.path(data_path, file_names)

# Function to extract spatial info from raster
check_resolution <- function(file_path) {
  tryCatch({
    r <- rast(file_path)
    
    res_xy <- res(r)
    ext_vals <- ext(r)
    crs_info <- crs(r)
    epsg_code <- tryCatch(CRS(crs_info)$epsg, error = function(e) NA)
    
    data.frame(
      file = basename(file_path),
      resolution_x = res_xy[1],
      resolution_y = res_xy[2],
      ncols = ncol(r),
      nrows = nrow(r),
      xmin = ext_vals[1],
      xmax = ext_vals[2],
      ymin = ext_vals[3],
      ymax = ext_vals[4],
      crs = crs_info,
      epsg = epsg_code,
      crs_type = if (is.lonlat(r)) "Geographic (degrees)" else "Projected (meters or km)",
      stringsAsFactors = FALSE
    )
    
  }, error = function(e) {
    data.frame(
      file = basename(file_path),
      resolution_x = NA,
      resolution_y = NA,
      ncols = NA,
      nrows = NA,
      xmin = NA,
      xmax = NA,
      ymin = NA,
      ymax = NA,
      crs = paste("ERROR:", e$message),
      epsg = NA,
      crs_type = NA,
      stringsAsFactors = FALSE
    )
  })
}

# Run check on all files
resolution_info <- do.call(rbind, lapply(files, check_resolution))

# Display full table
cat("📦 Spatial Resolution Summary:\n")
print(resolution_info)

# Check resolution consistency
res_x_unique <- unique(na.omit(resolution_info$resolution_x))
res_y_unique <- unique(na.omit(resolution_info$resolution_y))

cat("\n📏 Unique X Resolutions:\n")
print(res_x_unique)

cat("\n📐 Unique Y Resolutions:\n")
print(res_y_unique)

cat("\n✅ All files have same X resolution:", length(res_x_unique) == 1)
cat("\n✅ All files have same Y resolution:", length(res_y_unique) == 1)

# Highlight files with inconsistent resolutions
if (length(res_x_unique) > 1 || length(res_y_unique) > 1) {
  cat("\n⚠️ Files with different resolutions:\n")
  print(resolution_info[, c("file", "resolution_x", "resolution_y")])
}

# CRS Type Summary
cat("\n🗺️ Coordinate System Type Summary:\n")
print(table(resolution_info$crs_type))

# EPSG code summary
if (any(!is.na(resolution_info$epsg))) {
  cat("\n🔢 EPSG Codes Present:\n")
  print(table(resolution_info$epsg, useNA = "ifany"))
}
