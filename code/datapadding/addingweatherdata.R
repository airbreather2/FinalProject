library(nasapower)
library(dplyr)

# Read your existing dataset
wheat_data <- read.csv("../../data/Data/wheat_data.csv")

# Create a function to get climate data for a single coordinate
get_climate_for_point <- function(lon, lat, start_date, end_date) {
  climate_data <- get_power(
    community = "ag",
    lonlat = c(lon, lat),
    pars = c("PRECTOT", "T2M"), # Precipitation and temperature
    dates = c(start_date, end_date),
    temporal_api = "daily"
  )
  
  # Calculate annual average or other summary statistics
  summary_data <- climate_data %>%
    summarize(
      avg_rainfall = mean(PRECTOT),
      total_rainfall = sum(PRECTOT),
      avg_temp = mean(T2M)
    )
  
  return(summary_data)
}

# Apply this function to each row in your dataset
# This is a simplified approach - you might want to use apply() or purrr functions
for(i in 1:nrow(wheat_data)) {
  # Get climate data for this coordinate
  climate_result <- get_climate_for_point(
    wheat_data$Longitude..E.W.[i], 
    wheat_data$Latitude..N.S.[i],
    "2020-01-01", "2020-12-31"  # Adjust dates as needed
  )
  
  # Add the climate data to your dataframe
  wheat_data$avg_rainfall[i] <- climate_result$avg_rainfall
  wheat_data$total_rainfall[i] <- climate_result$total_rainfall
  wheat_data$avg_temp[i] <- climate_result$avg_temp
}

# Save the updated dataset
write.csv(wheat_data, "wheat_data_with_climate.csv", row.names = FALSE)