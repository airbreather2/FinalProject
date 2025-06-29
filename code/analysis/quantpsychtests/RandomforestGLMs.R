#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: RandomforestGLMs.R
# Description: Experimenting with random forest and GLMs to see if these approaches can be used
# Arguments: 
# Date: June 2025

# Usage:
# 
# To run the script:
# Rscript RandomforestGLMs.R


##########Modules##########################################
library(tidyverse)  # For data manipulation and visualization
library(openxlsx)  # Reading data from Excel files
library(ggplot2)
library(gtsummary)  
library(furrr)
library(ggplot2)
library(dplyr)
library(gt)
library(tidyr)
library(randomForest) #using this package as variables are automatically converted to characters/factors
library(ranger)
###########################################################
#set seed to source data


#set seed to test random forest
set.seed(1010)


source("dataprep.R")

##########################################################Run overall RF ################################################

# Run Random Forest
rf_model <- ranger(
  Grain.yield..tons.ha.1. ~ ., 
  data = wheat_rf,
  num.trees  = 500,
  importance = "impurity",
  num.threads = 7
)

# Model summary
print(rf_model)

print(rf_model$variable.importance)


############################################## for loop for comparing AEZ values #############################

# Get unique AEZ values
unique_aez <- unique(wheat_rf$AEZ)

# Initialize lists to store results
rf_models <- list()
model_performance <- data.frame()

# Loop through each AEZ
for(aez in unique_aez) {
  # Subset data for current AEZ
  aez_data <- wheat_rf %>% filter(AEZ == aez)
  
  # Skip if insufficient data
  if(nrow(aez_data) < 10) {
    cat("Skipping AEZ", aez, "- insufficient data (n =", nrow(aez_data), ")\n")
    next
  }
  
  # Run Random Forest
  rf_model <- ranger(
    Grain.yield..tons.ha.1. ~ ., 
    data = aez_data,
    num.trees = 500,
    importance = "impurity",
    num.threads = 7
  )
  
  # Store model
  rf_models[[as.character(aez)]] <- rf_model
  
  # Store performance metrics
  model_performance <- rbind(model_performance, data.frame(
    AEZ = aez,
    n_samples = nrow(aez_data),
    r_squared = rf_model$r.squared,
    oob_error = rf_model$prediction.error,
    rmse = sqrt(rf_model$prediction.error)
  ))
  
  cat("Completed AEZ", aez, "- R² =", round(rf_model$r.squared, 3), "\n")
}

print(model_performance)


############################################## for loop for comparing by grouped AEZ values #############################

# Get unique group values (make sure you're using wheat_rf_grouped!)
unique_groups <- unique(wheat_rf_grouped$group)

# Initialize lists to store results
rf_models <- list()
model_performance <- data.frame()

# Loop through each GROUP (not AEZ)
for(group_name in unique_groups) {
  # Subset data for current GROUP
  group_data <- wheat_rf_grouped %>% filter(group == group_name)
  
  # Skip if insufficient data
  if(nrow(group_data) < 10) {
    cat("Skipping group", group_name, "- insufficient data (n =", nrow(group_data), ")\n")
    next
  }
  
  # Run Random Forest (exclude grouping variables from predictors)
  rf_model <- ranger(
    Grain.yield..tons.ha.1. ~ . -AEZ -new_class -group -description, 
    data = group_data,
    num.trees = 500,
    importance = "impurity",
    num.threads = 7
  )
  
  # Store model
  rf_models[[as.character(group_name)]] <- rf_model
  
  # Store performance metrics
  model_performance <- rbind(model_performance, data.frame(
    group = group_name,
    n_samples = nrow(group_data),
    r_squared = rf_model$r.squared,
    oob_error = rf_model$prediction.error,
    rmse = sqrt(rf_model$prediction.error)
  ))
  
  cat("Completed group", group_name, "- R² =", round(rf_model$r.squared, 3), "\n")
}

print(model_performance)

################################################Testing groups with training splits ############################################

# Function to test model performance on unseen data
# This prevents overfitting and gives realistic R² values
quick_validation <- function(group_name) {
  # Step 1: Get all data for one climate group (e.g., "irrigated_soils")
  group_data <- wheat_rf %>% filter(group == group_name)
  
  # Skip groups with too few samples (need minimum for reliable testing)
  if(nrow(group_data) < 100) return(NULL)
  
  # Step 2: Random 80/20 split - randomly divide data into train/test sets
  # sample() picks random row numbers for training (80% of total rows)
  train_idx <- sample(nrow(group_data), 0.8 * nrow(group_data))
  train_data <- group_data[train_idx, ]      # 80% for training the model
  test_data <- group_data[-train_idx, ]      # 20% for testing (model never sees this!)
  
  # Step 3: Train Random Forest model ONLY on training data (80%)
  # Model learns patterns from this subset only
  rf_model <- ranger(
    Grain.yield..tons.ha.1. ~ . -AEZ -new_class -group -description -Country -Location -State.Region.County.Province -Conversion.for.latitude -Conversion.for.longitude -unit_code #exclude AEZ variables
    data = train_data,  # CRITICAL: Only use training data here
    num.trees = 500
  )
  
  # Step 4: Test model performance on completely unseen data (20%)
  # This gives realistic performance - how well model works on NEW data
  pred <- predict(rf_model, test_data)  # Predict yields for unseen samples
  test_r2 <- cor(pred$predictions, test_data$Grain.yield..tons.ha.1.)^2  # Calculate real R²
  
  # Step 5: Return both training and test performance
  # train_r2 = how well model fits training data
  # test_r2 = how well model predicts new data  
  # Gap between them = amount of overfitting
  return(c(train_r2 = rf_model$r.squared, test_r2 = test_r2))
}

# Test validation on your main climate groups
# This will show if your original R² = 0.44 was real or inflated
cat("=== VALIDATION REALITY CHECK ===\n")
for(group in c("irrigated_soils", "temperate", "sub-tropics", "severe_limitations", "temperate", "steep_terrain", "desert", "tropics")) {
  result <- quick_validation(group)
  
  # Only print results if group had enough data
  if(!is.null(result)) {
    # Print comparison of training vs test performance
    # Train = R² on data used for training
    # Test = R² on completely new data (the TRUE performance)
    # Overfitting = how much performance drops on new data
    cat(group, ": Train =", round(result[1], 3), "| Test =", round(result[2], 3), 
        "| Overfitting =", round(result[1] - result[2], 3), "\n")
  }
}



# How to interpret results:
# - Test R² close to Train R² = Good! Model generalizes well
# - Test R² much lower than Train R² = Overfitting problem
# - Test R² close to your original results = Your methodology was sound
# - Test R² much lower than original = Your original R² was inflated

#Things to check next! 

#multicollinearity between covariates

#spacial correlation


