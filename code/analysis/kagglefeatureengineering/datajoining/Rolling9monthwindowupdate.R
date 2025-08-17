library(terra)
library(dplyr)

# Load in main dataset data
dat <- read.csv("../../../../data/finaldatasets/testdata/finalfr/testdata.csv")


#################Salar nc file extraction func #######################

extract_weather_variable_salar <- function(df, nc_path, var_prefix = "x", window_length = 9, start_year = 1979) {
  # Compute sowing year
  df$sowing_year <- ifelse(df$start_date > df$end_date, 
                           df$Observation.period - 1, 
                           df$Observation.period)
  
  # Load raster
  r <- rast(nc_path)
  n_layers <- nlyr(r)
  
  # Create date sequence starting from start_year (default 1979)
  # Assuming monthly data starting from January of start_year
  start_date_ref <- as.Date(paste(start_year, "01", "01", sep = "-"))
  all_dates <- seq(start_date_ref, by = "month", length.out = n_layers)
  
  # Extract values at coordinates
  coords <- df[, c("Conversion.for.longitude", "Conversion.for.latitude")]
  extracted <- extract(r, coords)[, -1] # Remove ID column
  
  # Name columns using constructed dates
  colnames(extracted) <- paste0(var_prefix, "_", format(all_dates, "%Y_%m"))
  
  # Add ID to extracted values
  var_vals <- cbind(id = df$id, extracted)
  
  # Extract features for each observation
  result_list <- lapply(1:nrow(df), function(i) {
    row <- df[i, ]
    
    # Compute window dates
    start_date <- as.Date(paste(row$sowing_year, row$start_date, "01", sep = "-"))
    window_dates <- seq(start_date, by = "month", length.out = window_length)
    window_cols <- paste0(var_prefix, "_", format(window_dates, "%Y_%m"))
    
    # Fill in with NAs for missing columns
    vals <- rep(NA_real_, window_length)
    matching_cols <- window_cols %in% colnames(var_vals)
    
    if (any(matching_cols)) {
      # Extract matching values
      available_cols <- window_cols[matching_cols]
      vals[matching_cols] <- as.numeric(var_vals[i, available_cols])
    }
    
    c(row$id, row$sowing_year, vals)
  })
  
  # Build result data frame
  result <- as.data.frame(do.call(rbind, result_list))
  colnames(result) <- c("id", "sowing_year", paste0(var_prefix, 1:window_length))
  
  # Convert types
  result$id <- as.integer(result$id)
  result$sowing_year <- as.integer(result$sowing_year)
  result[, 3:ncol(result)] <- lapply(result[, 3:ncol(result)], as.numeric)
  
  return(result)
}





##############New NETCDF extraction code #############################


extract_weather_variable <- function(df, nc_path, var_prefix = "x", window_length = 9, startdate) {
  
  # Compute sowing year
  df$sowing_year <- ifelse(df$start_date > df$end_date,
                           df$Observation.period - 1,
                           df$Observation.period)
  
  # Load raster and get time
  r <- rast(nc_path)
  all_dates <- time(r)  # Use actual time values from NetCDF
  
  # Extract values at coordinates
  coords <- df[, c("Conversion.for.longitude", "Conversion.for.latitude")]
  extracted <- extract(r, coords)[, -1]  # Remove ID column
  
  # Name columns using actual dates
  colnames(extracted) <- paste0(var_prefix, "_", format(all_dates, "%Y_%m"))
  
  # Add ID to extracted values
  var_vals <- cbind(id = df$id, extracted)
  
  # Extract features for each observation
  result_list <- lapply(1:nrow(df), function(i) {
    row <- df[i, ]
    
    # Compute window dates
    start_date <- as.Date(paste(row$sowing_year, row$start_date, "01", sep = "-"))
    window_dates <- seq(start_date, by = "month", length.out = window_length)
    window_cols <- paste0(var_prefix, "_", format(window_dates, "%Y_%m"))
    
    # Fill in with NAs for missing columns
    vals <- rep(NA_real_, window_length)
    matching_cols <- window_cols %in% colnames(var_vals)
    if (any(matching_cols)) {
      vals[matching_cols] <- as.numeric(var_vals[i, window_cols[matching_cols]])
    }
    
    c(row$id, row$sowing_year, vals)
  })
  
  # Build result data frame
  result <- as.data.frame(do.call(rbind, result_list))
  colnames(result) <- c("id", "sowing_year", paste0(var_prefix, 1:window_length))
  
  # Convert types
  result$id <- as.integer(result$id)
  result$sowing_year <- as.integer(result$sowing_year)
  result[, 3:ncol(result)] <- lapply(result[, 3:ncol(result)], as.numeric)
  
  return(result)
}

# temp_features <- extract_weather_variable(dat, "temperature.nc", "temp")
# precip_features <- extract_weather_variable(dat, "precipitation.nc", "prc")


###################  Merging df into main #####################################

merge_weather_data <- function(main_df, weather_df) {
  
  # Get the feature column names (everything except id and sowing_year)
  feature_cols <- setdiff(names(weather_df), c("id", "sowing_year"))
  
  # Check which columns already exist in main dataset
  existing_cols <- intersect(feature_cols, names(main_df))
  
  # Remove existing columns from main dataset
  if (length(existing_cols) > 0) {
    main_df <- main_df[, !names(main_df) %in% existing_cols]
  }
  
  # Keep only first occurrence of each ID in weather_df
  weather_df <- weather_df[!duplicated(weather_df$id), ]
  
  # Merge the weather data
  result <- main_df %>%
    left_join(weather_df, by = "id")
  
  return(result)
}

# Usage:
# temp_df <- extract_weather_variable(dat, "temperature.nc", "temp", 9, "1979")
# dat <- merge_weather_data(dat, temp_df)
# 
# precip_df <- extract_weather_variable(dat, "precipitation.nc", "prc", 9, "1979")  
# dat <- merge_weather_data(dat, precip_df)




##################################Run the function for desired monthly feature ##################################
#Run the extraction code
#temperature
temp_df <- extract_weather_variable_salar(
  dat,
  "../../../../data/finaldatasets/covariates/Covariates/temperature_1979_2022.nc",
  "temp",
   9,
  "1979"
)

#Precipitation 
prc_df <- extract_weather_variable_salar(
  dat,
  "../../../../data/finaldatasets/covariates/Covariates/total_precipitation_1979_2022.nc",
  "prc",
   9,
  "1979"
)

#NMVI
NVMI_df <- extract_weather_variable(
  dat,
  "../../../../data/finaldatasets/covariates/Covariates/NDVI_monthly_10km_2004_2020.nc",
  "NDVI",
  9,
  "2004"
)

#Evaporation 
Evap_df <- extract_weather_variable(
  dat,
  "../../../../data/finaldatasets/covariates/Covariates/evaporation.nc",
  "Evap",
  9,
  "2004"
)

#Evaporative stress
EvapS_df <- extract_weather_variable(
  dat,
  "../../../../data/finaldatasets/covariates/Covariates/evap_stress.nc",
  "EvapS",
  9,
  "2004"
)

#Soil moisture 
Soilmoisture_df <- extract_weather_variable(
  dat,
  "../../../../data/finaldatasets/covariates/Covariates/soil_moisture.nc",
  "SoilM",
  9,
  "2004"
)

#Transpiration
transpiration_df <- extract_weather_variable(
  dat,
  "../../../../data/finaldatasets/covariates/Covariates/transpiration.nc",
  "Trans",
  9,
  "2004"
)


#####################################Merge into the full dataset ##############################################



#merge temp
dat <- merge_weather_data(dat, temp_df)

#merge prc
dat <- merge_weather_data(dat, prc_df)

#merge NDMI
dat <- merge_weather_data(dat, NVMI_df)

#merge Evaporation
dat <- merge_weather_data(dat, Evap_df)

#merge Evap S
dat <- merge_weather_data(dat, EvapS_df)

#merge soil moisture 
dat <- merge_weather_data(dat, Soilmoisture_df)

#merge Transpiration
dat <- merge_weather_data(dat, transpiration_df)



#----------------------------------------------------------------------------

extract_static_covariates <- function(df) {
  coords <- df[, c("Conversion.for.longitude", "Conversion.for.latitude")]
  
  covariate_paths <- list(
    Elevation = "../../../../data/finaldatasets/covariates/Covariates/elevation_world.tif",
    AEZ = "../../../../data/finaldatasets/covariates/Covariates/aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif",
    Clay = "../../../../data/finaldatasets/covariates/Covariates/clay_0_30cm.tif",
    Sand = "../../../../data/finaldatasets/covariates/Covariates/sand_0_30cm.tif",
    Silt = "../../../../data/finaldatasets/covariates/Covariates/silt_0_30cm.tif",
    Soil.pH = "../../../../data/finaldatasets/covariates/Covariates/phh2o_0_30cm.tif",
    Soil.organic.carbon..g.C.kg.1. = "../../../../data/finaldatasets/covariates/Covariates/soc_0_30cm.tif",
    pr_irrigated = "../../../../data/finaldatasets/covariates/Covariates/irrigated_gmia_v5_aei_pct.asc",
    Soil_N = "../../../../data/finaldatasets/covariates/Covariates/soil_nitrogen_0_30cm.tif"
  )
  
  for (varname in names(covariate_paths)) {
    r <- rast(covariate_paths[[varname]])
    
    # Remove existing column if present
    if (varname %in% names(df)) {
      df[[varname]] <- NULL
    }
    
    # Extract and assign
    df[[varname]] <- extract(r, coords, ID = FALSE)[, 1]
  }
  
  return(df)
}

# === Run extraction ===
dat <- extract_static_covariates(dat)

# === Write output ===
write.csv(dat, "../../../../data/finaldatasets/testdata/finalfr/Treetest1.csv")


