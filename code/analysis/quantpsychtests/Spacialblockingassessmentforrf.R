#!/usr/bin/env Rscript
# Complete Spatial Analysis for Wheat Yield Data
# Includes Moran's I testing and Variogram analysis per AEZ group

library(tidyverse)
library(gstat)        # For variograms
library(sp)           # Spatial objects
library(geosphere)    # Distance calculations
library(corrplot)     # Correlation plots
library(blockCV)      # Spatial CV
library(spdep)        # For Moran's I
library(ranger)       # For random forest

#set seed to test random forest
set.seed(1010)

source("dataprep.R")

# =============================================================================
# MORAN'S I ANALYSIS - FIXED VERSION
# =============================================================================

# Fixed Moran's I test with proper error handling
simple_morans <- function(data, yield_col, lon_col, lat_col) {
  
  # Extract coordinates and yields
  coords <- data[, c(lon_col, lat_col)]
  yields <- data[[yield_col]]
  
  # Remove missing values
  complete_idx <- complete.cases(coords, yields)
  coords <- coords[complete_idx, ]
  yields <- yields[complete_idx]
  
  # Check if enough points remain
  if(nrow(coords) < 10) {
    cat("Insufficient data points\n")
    return(NA)
  }
  
  # FIX: Ensure coords is a proper data frame with column names
  coords <- as.data.frame(coords)
  colnames(coords) <- c("lon", "lat")
  
  # Create spatial weights and test
  tryCatch({
    knn <- knearneigh(coords, k = min(8, nrow(coords)-1))
    nb <- knn2nb(knn) 
    w <- nb2listw(nb, style = "W")
    result <- moran.test(yields, w)
    
    # Print result
    cat("Moran's I =", round(result$estimate[1], 3), 
        "| p =", round(result$p.value, 4))
    
    if(result$p.value < 0.05 && result$estimate[1] > 0.1) {
      cat(" ❌ SPATIAL AUTOCORR!\n")
    } else {
      cat(" ✅ No spatial autocorr\n")
    }
    
    return(list(morans_i = result$estimate[1], p_value = result$p.value))
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NA)
  })
}

cat("\n=== MORAN'S I ANALYSIS BY AEZ GROUP ===\n")

# Test each group with sample size info
morans_results <- data.frame()

for(group in c("irrigated_soils", "temperate", "sub-tropics", "severe_limitations", 
               "steep_terrain", "desert", "tropics", "cold", "boreal")) {
  
  group_data <- wheat_rf %>% filter(group == !!group)
  
  cat("\n", group, "(n =", nrow(group_data), "): ")
  
  if(nrow(group_data) < 10) {
    cat("Too few points\n")
    next
  }
  
  result <- simple_morans(group_data, "Grain.yield..tons.ha.1.", 
                          "Conversion.for.longitude", "Conversion.for.latitude")
  
  if(!is.na(result) && is.list(result)) {
    morans_results <- rbind(morans_results, data.frame(
      group = group,
      n_points = nrow(group_data),
      morans_i = result$morans_i,
      p_value = result$p_value,
      significant = result$p_value < 0.05,
      strong_autocorr = result$p_value < 0.05 & result$morans_i > 0.1
    ))
  }
}

# Print summary table
cat("\n=== MORAN'S I SUMMARY TABLE ===\n")
print(morans_results)

# =============================================================================
# VARIOGRAM ANALYSIS - FIXED VERSION
# =============================================================================

# Fixed variogram function with better error handling
calculate_autocorr_range <- function(data, target_col, coord_cols, max_dist_km = 500, group_name = "") {
  
  cat("\n=== VARIOGRAM ANALYSIS FOR", group_name, "===\n")
  
  # Check if enough data points
  if(nrow(data) < 20) {
    cat("Insufficient data points for variogram analysis (n =", nrow(data), ")\n")
    return(NULL)
  }
  
  tryCatch({
    # Create a copy to avoid modifying original data
    data_copy <- data
    
    # Convert to spatial object
    coordinates(data_copy) <- coord_cols
    proj4string(data_copy) <- CRS("+proj=longlat +datum=WGS84")
    
    # Transform to projected coordinate system for accurate distance calculation
    # Use a more universal UTM zone or auto-detect
    data_proj <- spTransform(data_copy, CRS("+proj=utm +zone=33 +datum=WGS84"))
    
    # Calculate empirical variogram with smaller bins for better resolution
    variogram_emp <- variogram(
      formula = as.formula(paste(target_col, "~ 1")),
      data = data_proj,
      cutoff = max_dist_km * 1000,  # Convert km to meters
      width = 20000  # 20km bins
    )
    
    # Check if variogram calculation was successful
    if(nrow(variogram_emp) < 3) {
      cat("Insufficient variogram points calculated\n")
      return(NULL)
    }
    
    # Fit theoretical variogram model with better initial parameters
    initial_psill <- var(data[[target_col]], na.rm = TRUE)
    initial_range <- max_dist_km * 1000 / 3  # Start with 1/3 of max distance
    
    variogram_fit <- fit.variogram(
      variogram_emp,
      model = vgm(psill = initial_psill, 
                  model = "Sph",  # Spherical model
                  range = initial_range,
                  nugget = initial_psill * 0.1)  # Small nugget
    )
    
    # Extract the range parameter
    autocorr_range_m <- variogram_fit$range[2]  # In meters
    autocorr_range_km <- autocorr_range_m / 1000  # Convert to km
    
    cat("Autocorrelation range:", round(autocorr_range_km, 1), "km\n")
    cat("Recommended block size:", round(autocorr_range_km * 2, 1), "km\n")
    
    # Create plot
    variogram_plot <- ggplot(variogram_emp, aes(x = dist/1000, y = gamma)) +
      geom_point(size = 3, color = "blue") +
      geom_line(data = variogramLine(variogram_fit, maxdist = max_dist_km * 1000) %>%
                  mutate(dist = dist/1000), 
                aes(x = dist, y = gamma), color = "red", size = 1.2) +
      geom_vline(xintercept = autocorr_range_km, linetype = "dashed", color = "red") +
      labs(
        x = "Distance (km)",
        y = "Semivariance",
        title = paste("Variogram Analysis -", group_name),
        subtitle = paste("Range:", round(autocorr_range_km, 1), "km | Recommended block size:", 
                         round(autocorr_range_km * 2, 1), "km"),
        caption = "Blue dots = empirical | Red line = fitted model | Dashed = range"
      ) +
      theme_minimal()
    
    return(list(
      range_km = autocorr_range_km,
      variogram_model = variogram_fit,
      empirical_variogram = variogram_emp,
      plot = variogram_plot,
      recommended_block_size = autocorr_range_km * 2
    ))
    
  }, error = function(e) {
    cat("Error in variogram analysis:", e$message, "\n")
    return(NULL)
  })
}

# Run variogram analysis for groups with significant spatial autocorrelation
cat("\n=== VARIOGRAM ANALYSIS FOR GROUPS WITH SPATIAL AUTOCORRELATION ===\n")

variogram_results <- list()

# Test groups that showed spatial autocorrelation in Moran's I
significant_groups <- morans_results %>% 
  filter(strong_autocorr == TRUE) %>% 
  pull(group)

if(length(significant_groups) > 0) {
  for(group in significant_groups) {
    group_data <- wheat_rf %>% filter(group == !!group)
    
    result <- calculate_autocorr_range(
      data = group_data,
      target_col = "Grain.yield..tons.ha.1.",
      coord_cols = c("Conversion.for.longitude", "Conversion.for.latitude"),
      max_dist_km = 500,
      group_name = group
    )
    
    if(!is.null(result)) {
      variogram_results[[group]] <- result
      
      # Save plot
      ggsave(paste0("variogram_", group, ".png"), result$plot, 
             width = 10, height = 6, dpi = 300)
    }
  }
} else {
  cat("No groups showed strong spatial autocorrelation requiring variogram analysis\n")
}

# =============================================================================
# SPATIAL CROSS-VALIDATION RECOMMENDATIONS
# =============================================================================

cat("\n=== SPATIAL CROSS-VALIDATION RECOMMENDATIONS ===\n")

# Compile recommendations
recommendations <- data.frame()

for(group in names(variogram_results)) {
  rec_distance <- variogram_results[[group]]$recommended_block_size
  
  recommendations <- rbind(recommendations, data.frame(
    group = group,
    morans_i = morans_results[morans_results$group == group, "morans_i"],
    autocorr_range_km = variogram_results[[group]]$range_km,
    recommended_block_size_km = rec_distance,
    min_separation_km = ceiling(rec_distance / 50) * 50  # Round up to nearest 50km
  ))
}

if(nrow(recommendations) > 0) {
  cat("\nRECOMMENDATIONS TABLE:\n")
  print(recommendations)
  
  cat("\n=== IMPLEMENTATION GUIDANCE ===\n")
  cat("For groups with spatial autocorrelation, use spatial block cross-validation with:\n")
  
  for(i in 1:nrow(recommendations)) {
    cat("•", recommendations$group[i], ": minimum", recommendations$min_separation_km[i], "km separation\n")
  }
  
} else {
  cat("✅ No strong spatial autocorrelation detected in any group\n")
  cat("→ Standard random cross-validation may be appropriate\n")
  cat("→ Your overfitting might be due to other factors (high dimensionality, small sample sizes)\n")
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n=== FINAL ANALYSIS SUMMARY ===\n")
cat("Total groups analyzed:", nrow(morans_results), "\n")
cat("Groups with spatial autocorrelation:", sum(morans_results$strong_autocorr, na.rm = TRUE), "\n")
cat("Groups requiring spatial CV:", length(variogram_results), "\n")

if(length(variogram_results) > 0) {
  avg_block_size <- mean(sapply(variogram_results, function(x) x$recommended_block_size))
  cat("Average recommended block size:", round(avg_block_size, 1), "km\n")
  
  cat("\n=== NEXT STEPS ===\n")
  cat("1. Implement spatial block cross-validation for affected groups\n")
  cat("2. Use recommended block sizes from variogram analysis\n") 
  cat("3. Expect lower but more honest R² values\n")
  cat("4. Consider feature selection to reduce overfitting\n")
  
} else {
  cat("\n=== ALTERNATIVE EXPLANATIONS FOR OVERFITTING ===\n")
  cat("Since no strong spatial autocorrelation was detected:\n")
  cat("1. High dimensionality (26+ predictors for small samples)\n")
  cat("2. Multicollinearity between climate variables\n")
  cat("3. Temporal autocorrelation (same locations across years)\n")
  cat("4. Need for feature selection and regularization\n")
}

cat("\nAnalysis complete! Check plots saved as variogram_[group].png\n")