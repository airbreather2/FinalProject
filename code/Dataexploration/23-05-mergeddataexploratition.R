#!/usr/bin/env Rscript
################################################################################
# WHEAT DATASET EXPLORATION AND VISUALIZATION
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: 23-05-mergeddataexploratition.R
# Description: Clean exploration of wheat dataset with visualizations
# Date: May 2025
################################################################################

# Clear environment
rm(list = ls(all.names = TRUE))

################################################################################
# LOAD LIBRARIES
################################################################################

# Load essential packages (in order to avoid conflicts)
library(tidyverse)   # Data manipulation and visualization
library(leaflet)     # Interactive maps  
library(gt)          # Professional tables
library(knitr)       # Table output
library(plotly)      # Interactive plots
library(htmlwidgets) # Save interactive plots

################################################################################
# DATA LOADING AND INITIAL EXPLORATION
################################################################################

# Load wheat dataset
cat("Loading wheat dataset...\n")
data <- read.csv("../../data/Data/23-05-mergedwheatdata.csv", fileEncoding = "latin1")

# Basic data inspection
cat("Dataset loaded successfully!\n")
cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("Column names:\n")
print(names(data))


# Display data structure
str(data)
head(data)

#save col headers as csv

headers <- names(data)

write.csv(headers, "headers.csv", row.names = FALSE, col.names = FALSE, sep = ",")

################################################################################
# DATA CLEANING AND YIELD DATASET CREATION
################################################################################

cat("Creating clean yield dataset...\n")

# Create clean yield dataset with renamed columns
yield <- data %>%
  select(
    Country, 
    Observation.period, 
    Location, 
    Continent, 
    State.Region.County.Province, 
    longitude = Conversion.for.longitude,
    latitude = Conversion.for.latitude,
    yield_value = Grain.yield..tons.ha.1.
  ) %>%
  # Convert to appropriate data types
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    yield_value = as.numeric(yield_value),
    Observation.period = as.numeric(Observation.period)
  ) %>%
  # Filter for valid data
  filter(
    !is.na(yield_value),
    !is.na(longitude),
    !is.na(latitude),
    yield_value <= 30,  # Remove extreme outliers
    nchar(as.character(Observation.period)) == 4  # Valid 4-digit years
  )

cat("Clean yield dataset created:", nrow(yield), "observations\n")

################################################################################
# INTERACTIVE MAP VISUALIZATION
################################################################################

cat("Creating interactive map...\n")

# Create color palette for yield values
pal <- colorNumeric(palette = "YlOrRd", domain = yield$yield_value)

# Create interactive leaflet map
yield_map <- leaflet(yield) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~pal(yield_value),
    popup = ~paste(
      "<b>Country:</b>", Country, "<br>",
      "<b>Location:</b>", Location, "<br>",
      "<b>Year:</b>", Observation.period, "<br>",
      "<b>Yield:</b>", round(yield_value, 2), "tons/ha"
    ),
    radius = 4,
    fillOpacity = 0.7,
    stroke = FALSE
  ) %>%
  addLegend(
    "bottomright", 
    pal = pal,
    values = ~yield_value, 
    title = "Yield (tons/ha)",
    opacity = 0.8
  )

# Display map
yield_map

# Save map
dir.create("visualisation", showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(yield_map, "visualisation/wheat_yield_map.html")

################################################################################
# EXPLORATORY DATA ANALYSIS
################################################################################

cat("Creating exploratory visualizations...\n")

# 1. Temporal distribution of observations
temporal_plot <- ggplot(yield, aes(x = Observation.period)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution of Wheat Yield Observations by Year",
    x = "Year", 
    y = "Number of Observations"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

print(temporal_plot)

# 2. Country and continent overview
country_summary <- yield %>%
  count(Country, Continent, name = "observations") %>%
  arrange(desc(observations))

# Bar plot of observations by country (top 15)
country_plot <- country_summary %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(Country, observations), y = observations, fill = Continent)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 15 Countries by Number of Observations",
    x = "Country", 
    y = "Number of Observations",
    fill = "Continent"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

print(country_plot)

# 3. Professional table of country statistics
country_table <- country_summary %>%
  slice_head(n = 20) %>%
  gt() %>%
  tab_header(
    title = "Wheat Yield Observations by Country",
    subtitle = "Top 20 countries with most observations"
  ) %>%
  cols_label(
    Country = "Country",
    Continent = "Continent", 
    observations = "Observations"
  ) %>%
  fmt_number(columns = observations, decimals = 0) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(country_table)

################################################################################
# MISSING DATA ANALYSIS
################################################################################

cat("Analyzing missing data patterns...\n")

# Calculate missing data percentages by country
missing_data_country <- data %>%
  group_by(Country) %>%
  summarise(
    Total_Obs = n(),
    across(everything(), ~round(mean(is.na(.)) * 100, 2), .names = "Missing_{.col}"),
    .groups = 'drop'
  ) 

# Create summary table of missing data
missing_summary_table <- missing_data_country %>%
  slice_head(n = 15) %>%
  select(Country, Total_Obs, starts_with("Missing_")) %>%
  kable(caption = "Missing Data Summary by Country (Top 15)")

print(missing_summary_table)

# Prepare data for heatmap visualization
heatmap_data <- missing_data_country %>%
  select(-Total_Obs) %>%
  pivot_longer(
    cols = starts_with("Missing_"), 
    names_to = "Variable", 
    values_to = "Percent_Missing",
    names_prefix = "Missing_"
  ) %>%
  # Clean variable names
  mutate(
    Variable = str_replace_all(Variable, "_", " "),
    Variable = str_to_title(Variable)
  ) %>%
  # Filter out variables with no missing data
  group_by(Variable) %>%
  filter(sum(Percent_Missing) > 0) %>%
  ungroup()

# Static missing data heatmap
static_heatmap <- ggplot(heatmap_data, aes(x = Variable, y = Country, fill = Percent_Missing)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "white", 
    mid = "orange", 
    high = "red", 
    midpoint = 50,
    name = "% Missing"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  labs(
    title = "Missing Data Heatmap by Country", 
    x = "Variable", 
    y = "Country"
  )

print(static_heatmap)

heatmap_data <- data %>%
  group_by(Country) %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100), .groups = 'drop') %>%
  pivot_longer(cols = -Country, 
               names_to = "Variable", 
               values_to = "Pct_Missing")

# Create interactive heatmap - NO FILTERING
interactive_heatmap <- plot_ly(
  data = heatmap_data,
  x = ~Variable,
  y = ~Country,
  z = ~Pct_Missing,
  type = "heatmap",
  colorscale = "RdYlBu",
  reversescale = TRUE,
  hovertemplate = "Country: %{y}<br>Variable: %{x}<br>Missing: %{z}%<extra></extra>"
) %>%
  layout(
    title = "Missing Data Heatmap - ALL Variables",
    xaxis = list(title = "Variable", tickangle = -45),
    yaxis = list(title = "Country"),
    width = 1200  # Make wider for more variables
  )

print(interactive_heatmap)

# Save interactive heatmap
htmlwidgets::saveWidget(interactive_heatmap, "visualisation/missing_datajoined_interactive.html")

################################################################################
# YIELD ANALYSIS AND ML READINESS
################################################################################

cat("Analyzing yield patterns and ML readiness...\n")

# Country-level yield analysis
yield_analysis <- yield %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    years_covered = n_distinct(Observation.period),
    year_range = paste(min(Observation.period), "-", max(Observation.period)),
    avg_yield = round(mean(yield_value, na.rm = TRUE), 2),
    yield_sd = round(sd(yield_value, na.rm = TRUE), 2),
    min_yield = round(min(yield_value, na.rm = TRUE), 2),
    max_yield = round(max(yield_value, na.rm = TRUE), 2),
    # Calculate yield trend (requires at least 3 observations)
    yield_trend = case_when(
      n() < 3 ~ "Insufficient data",
      cor(Observation.period, yield_value, use = "complete.obs") > 0.3 ~ "Strong increase",
      cor(Observation.period, yield_value, use = "complete.obs") > 0.1 ~ "Moderate increase", 
      cor(Observation.period, yield_value, use = "complete.obs") < -0.3 ~ "Strong decrease",
      cor(Observation.period, yield_value, use = "complete.obs") < -0.1 ~ "Moderate decrease",
      TRUE ~ "Stable"
    ),
    .groups = 'drop'
  ) %>%
  arrange(desc(observations))

# Machine Learning readiness assessment
ml_readiness <- yield_analysis %>%
  mutate(
    time_span = years_covered,
    data_density = round(observations / pmax(years_covered, 1), 2),
    ml_suitability = case_when(
      observations >= 20 & years_covered >= 10 ~ "Excellent",
      observations >= 15 & years_covered >= 8 ~ "Very Good",
      observations >= 10 & years_covered >= 5 ~ "Good",
      observations >= 5 & years_covered >= 3 ~ "Moderate",
      TRUE ~ "Poor"
    )
  ) %>%
  select(Country, observations, years_covered, data_density, avg_yield, yield_trend, ml_suitability) %>%
  arrange(desc(observations))

# Display results
cat("\nYield Analysis Summary:\n")
print(yield_analysis %>% slice_head(n = 10))

cat("\nMachine Learning Readiness Assessment:\n")
print(ml_readiness %>% slice_head(n = 15))

# Create professional ML readiness table
ml_table <- ml_readiness %>%
  slice_head(n = 20) %>%
  gt() %>%
  tab_header(
    title = "Machine Learning Readiness Assessment",
    subtitle = "Countries ranked by data availability"
  ) %>%
  cols_label(
    Country = "Country",
    observations = "Obs.",
    years_covered = "Years",
    data_density = "Density",
    avg_yield = "Avg Yield",
    yield_trend = "Trend",
    ml_suitability = "ML Suitability"
  ) %>%
  fmt_number(columns = c(avg_yield), decimals = 2) %>%
  fmt_number(columns = c(data_density), decimals = 1) %>%
  data_color(
    columns = ml_suitability,
    colors = scales::col_factor(
      palette = c("red", "orange", "yellow", "lightgreen", "green"),
      domain = c("Poor", "Moderate", "Good", "Very Good", "Excellent")
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(ml_table)

################################################################################
# CONTINENT-LEVEL ANALYSIS
################################################################################

if ("Continent" %in% names(data)) {
  cat("Creating continent-level analysis...\n")
  
  # Continent summary
  continent_summary <- yield %>%
    group_by(Continent) %>%
    summarise(
      countries = n_distinct(Country),
      observations = n(),
      avg_yield = round(mean(yield_value, na.rm = TRUE), 2),
      yield_range = paste(round(min(yield_value, na.rm = TRUE), 1), "-", 
                          round(max(yield_value, na.rm = TRUE), 1)),
      .groups = 'drop'
    ) %>%
    arrange(desc(observations))
  
  cat("\nContinent Summary:\n")
  print(continent_summary)
  
  # Continent yield comparison
  continent_plot <- ggplot(yield, aes(x = Continent, y = yield_value, fill = Continent)) +
    geom_boxplot(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = "Wheat Yield Distribution by Continent",
      x = "Continent",
      y = "Yield (tons/ha)"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  print(continent_plot)
}

################################################################################
# SUMMARY STATISTICS AND DATA QUALITY REPORT
################################################################################

cat("Generating final summary report...\n")

# Overall data quality summary
data_quality <- list(
  original_dataset = list(
    rows = nrow(data),
    columns = ncol(data),
    countries = n_distinct(data$Country, na.rm = TRUE)
  ),
  clean_yield_dataset = list(
    rows = nrow(yield),
    columns = ncol(yield),
    countries = n_distinct(yield$Country),
    year_range = paste(min(yield$Observation.period), "-", max(yield$Observation.period)),
    yield_range = paste(round(min(yield$yield_value), 2), "-", round(max(yield$yield_value), 2), "tons/ha")
  ),
  data_coverage = list(
    countries_with_good_ml_data = sum(ml_readiness$ml_suitability %in% c("Good", "Very Good", "Excellent")),
    countries_with_moderate_data = sum(ml_readiness$ml_suitability == "Moderate"),
    countries_with_poor_data = sum(ml_readiness$ml_suitability == "Poor")
  )
)

cat("\n" + paste(rep("=", 60), collapse = "") + "\n")
cat("WHEAT DATASET EXPLORATION SUMMARY\n")
cat(paste(rep("=", 60), collapse = "") + "\n")

cat("\nOriginal Dataset:\n")
cat("- Rows:", data_quality$original_dataset$rows, "\n")
cat("- Columns:", data_quality$original_dataset$columns, "\n")
cat("- Countries:", data_quality$original_dataset$countries, "\n")

cat("\nClean Yield Dataset:\n")
cat("- Observations:", data_quality$clean_yield_dataset$rows, "\n")
cat("- Countries:", data_quality$clean_yield_dataset$countries, "\n")
cat("- Year Range:", data_quality$clean_yield_dataset$year_range, "\n")
cat("- Yield Range:", data_quality$clean_yield_dataset$yield_range, "\n")

cat("\nMachine Learning Readiness:\n")
cat("- Countries with Good+ ML data:", data_quality$data_coverage$countries_with_good_ml_data, "\n")
cat("- Countries with Moderate ML data:", data_quality$data_coverage$countries_with_moderate_data, "\n")
cat("- Countries with Poor ML data:", data_quality$data_coverage$countries_with_poor_data, "\n")

cat("\nFiles Generated:\n")
cat("- visualisation/wheat_yield_map.html\n")
cat("- visualisation/missing_data_interactive.html\n")

cat("\n" + paste(rep("=", 60), collapse = "") + "\n")
cat("EXPLORATION COMPLETE!\n")
cat(paste(rep("=", 60), collapse = "") + "\n")

