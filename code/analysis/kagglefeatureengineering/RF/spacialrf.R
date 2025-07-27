# =============================================================================
# CLEAN GRF CROSS-VALIDATION PIPELINE (USING SPATIOTEMPORAL HAVERSINE GROUPS)
# =============================================================================

# Load libraries
suppressPackageStartupMessages({
  library(SpatialML)
  library(dplyr)
  library(ggplot2)
  library(Metrics)
  library(geosphere)  # for haversine distance
})

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

load_and_prepare_data <- function(file_path) {
  df <- read.csv(file_path)
  required_cols <- c("yield", "lon", "lat", "year")
  df <- df[complete.cases(df[required_cols]), ]
  return(df)
}

# =============================================================================
# 2. HAVERSINE-BASED SPATIOTEMPORAL FOLD CREATION
# =============================================================================

create_spatiotemporal_groups <- function(df, radius_km = 100) {
  coords <- df[, c("lon", "lat")]
  dists <- distm(coords, fun = distHaversine) / 1000  # distance in km
  n <- nrow(coords)
  visited <- rep(FALSE, n)
  group <- rep(NA_integer_, n)
  group_id <- 1
  
  for (i in 1:n) {
    if (!visited[i]) {
      members <- which(dists[i, ] <= radius_km)
      group[members] <- group_id
      visited[members] <- TRUE
      group_id <- group_id + 1
    }
  }
  
  df$spatial_group <- group
  df$combined_group <- paste0("S", df$spatial_group, "_T", df$year)
  
  # Convert to factor groups and assign fold numbers
  unique_groups <- unique(df$combined_group)
  k_folds <- 5
  set.seed(123)
  group_folds <- sample(rep(1:k_folds, length.out = length(unique_groups)))
  group_map <- data.frame(combined_group = unique_groups, fold = group_folds)
  df <- left_join(df, group_map, by = "combined_group")
  
  return(df)
}

# =============================================================================
# 3. MODEL FITTING FUNCTION
# =============================================================================

fit_grf_model <- function(train_data, formula, bw, ntree, mtry) {
  coords_train <- as.matrix(train_data[, c("lon", "lat")])
  model <- SpatialML::grf(
    formula = formula,
    dframe = train_data,
    coords = coords_train,
    bw = bw,
    kernel = "adaptive",
    ntree = ntree,
    mtry = mtry,
    geo.weighted = TRUE,
    importance = "impurity",
    forests = FALSE,
    print.results = FALSE
  )
  return(model)
}

# =============================================================================
# 4. PREDICTION FUNCTION
# =============================================================================

predict_grf <- function(model, test_data) {
  predict(
    object = model,
    new.data = test_data,
    x.var.name = "lon",
    y.var.name = "lat",
    local.w = 1,
    global.w = 0
  )
}

# =============================================================================
# 5. METRICS CALCULATION
# =============================================================================

calculate_metrics <- function(actual, predicted) {
  rmse_val <- rmse(actual, predicted)
  mae_val <- mae(actual, predicted)
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2_val <- 1 - (ss_res / ss_tot)
  data.frame(RMSE = rmse_val, MAE = mae_val, R2 = r2_val, n = length(actual))
}

# =============================================================================
# 6. CROSS-VALIDATION FUNCTION
# =============================================================================

run_cv_fold <- function(df, fold_num, formula, bw, ntree, mtry) {
  train_data <- df[df$fold != fold_num, ]
  test_data  <- df[df$fold == fold_num, ]
  tryCatch({
    model <- fit_grf_model(train_data, formula, bw, ntree, mtry)
    predictions <- predict_grf(model, test_data)
    metrics <- calculate_metrics(test_data$yield, predictions)
    list(success = TRUE, metrics = metrics, predictions = data.frame(
      fold = fold_num, actual = test_data$yield, predicted = predictions))
  }, error = function(e) {
    cat("    Error in fold", fold_num, ":", e$message, "\n")
    list(success = FALSE, metrics = data.frame(RMSE = NA, MAE = NA, R2 = NA, n = nrow(test_data)), predictions = data.frame())
  })
}

# =============================================================================
# 7. HYPERPARAMETER SEARCH
# =============================================================================

hyperparameter_search <- function(df, formula, param_grid, k_folds = 5) {
  results <- list()
  all_predictions <- data.frame()
  for (i in 1:nrow(param_grid)) {
    params <- param_grid[i, ]
    fold_results <- list()
    fold_predictions <- data.frame()
    for (fold in 1:k_folds) {
      result <- run_cv_fold(df, fold, formula, params$bw, params$ntree, params$mtry)
      fold_results[[fold]] <- result$metrics
      if (result$success) {
        fold_predictions <- rbind(fold_predictions, cbind(result$predictions, params))
      }
    }
    metrics_df <- do.call(rbind, fold_results)
    successful_folds <- sum(!is.na(metrics_df$RMSE))
    if (successful_folds > 0) {
      weights <- metrics_df$n / sum(metrics_df$n, na.rm = TRUE)
      summary_metrics <- data.frame(
        bw = params$bw, ntree = params$ntree, mtry = params$mtry,
        RMSE_mean = weighted.mean(metrics_df$RMSE, weights, na.rm = TRUE),
        MAE_mean = weighted.mean(metrics_df$MAE, weights, na.rm = TRUE),
        R2_mean = weighted.mean(metrics_df$R2, weights, na.rm = TRUE),
        RMSE_sd = sd(metrics_df$RMSE, na.rm = TRUE),
        MAE_sd = sd(metrics_df$MAE, na.rm = TRUE),
        R2_sd = sd(metrics_df$R2, na.rm = TRUE),
        successful_folds = successful_folds,
        total_folds = k_folds
      )
      results[[i]] <- summary_metrics
      all_predictions <- rbind(all_predictions, fold_predictions)
    }
  }
  list(results = do.call(rbind, results), predictions = all_predictions)
}

# =============================================================================
# 8. MAIN EXECUTION FUNCTION
# =============================================================================

run_grf_analysis <- function(file_path) {
  df <- load_and_prepare_data(file_path)
  df <- create_spatiotemporal_groups(df)
  exclude_vars <- c("yield", "fold", "lon", "lat", "spatial_group", "combined_group", "location_id")
  predictor_vars <- setdiff(names(df), exclude_vars)
  model_formula <- as.formula(paste("yield ~", paste(predictor_vars, collapse = " + ")))
  
  param_grid <- expand.grid(
    bw = c(30, 50, 70),
    ntree = c(100, 300),
    mtry = c(3, min(6, length(predictor_vars)))
  )
  
  cv_results <- hyperparameter_search(df, model_formula, param_grid)
  results_sorted <- cv_results$results[order(cv_results$results$RMSE_mean), ]
  best_params <- results_sorted[1, ]
  
  final_model <- fit_grf_model(df, model_formula,
                               best_params$bw, best_params$ntree, best_params$mtry)
  
  list(
    data = df,
    results = results_sorted,
    predictions = cv_results$predictions,
    best_params = best_params,
    final_model = final_model
  )
}

# =============================================================================
# 9. RUN THE ANALYSIS
# =============================================================================

analysis_results <- run_grf_analysis("../../../../data/finaldatasets/testdata/imputation/imputed_dataset.csv")
