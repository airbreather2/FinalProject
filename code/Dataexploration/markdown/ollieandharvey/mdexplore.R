# Wheat Dataset Exploration and Analysis
# Author: Sebastian Dohne
# Date: June 2025

# Project Overview:
# This project aims to predict crop yields in a selected region by leveraging 
# data-driven methods, including multivariate statistical analyses and machine 
# learning (ML) approaches. The dataset includes environmental, climatic, and 
# socio-economic data to build robust models for crop yield prediction.

# Methods being considered: Random Forest, XGboost, elastic nets, and possibly SVMs
# Traditional approaches: General additive models and non-linear regression

# Load Required Libraries ----
library(data.table)    # For fast file reading
library(tidyverse)     # Data manipulation
library(knitr)         # Tables
library(ggplot2)       # Static plots only
library(scales)        # For formatting
library(leaflet)       # Interactive maps
library(gt)            # Table formatting
library(kableExtra)    # Additional table features
library(plotly)        # Interactive plots
library(furrr)         # Parallel processing
library(flextable)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)


# Data Loading and Initial Exploration ----
# Load wheat data with irrigation and rainfall calendar information
# Note: fileEncoding handles special characters in country/location names
# Update this path to match where your CSV file is located:
data <- read.csv("../../../../data/Data/chatgptdata/23-05-mergedwheatdatafixed.csv", fileEncoding = "utf-8")

# Alternative paths to try if the above doesn't work:
# data <- read.csv("./2305mergedwheatdatachatgpt.csv", fileEncoding = "utf-8")
# data <- read.csv("../2305mergedwheatdatachatgpt.csv", fileEncoding = "utf-8") 
# data <- read.csv("C:/full/path/to/your/2305mergedwheatdatachatgpt.csv", fileEncoding = "utf-8")

# Initial data inspection
cat("Dataset dimensions:", dim(data), "\n")
cat("Number of countries:", length(unique(data$Country)), "\n")
cat("Time period covered:", range(data$Observation.period, na.rm = TRUE), "\n")

#######################Data observations #########################################################

# Display all column names for reference
names(data)

# Display all unique pest present values 
sum(!is.na(data$Pest.prescence....64))
unique(data$Pest.prescence....64)

table(data$AEZ)


##################################################################################################

# Data Cleaning and Preprocessing ----
# Select key variables and rename for easier handling
yield <- data %>%
  select(Country, 
         Observation.period, 
         Location, 
         Continent, 
         State.Region.County.Province, 
         longitude = 10,    # Rename for clarity
         latitude = 9,      # Rename for clarity
         yield_value = Grain.yield..tons.ha.1.,
         AEZ) %>%  # Main outcome variable
  mutate(
    # Convert character columns to appropriate numeric types
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    yield_value = as.numeric(yield_value),
    Observation.period = as.numeric(Observation.period)
  ) %>%  
  filter(
    # Remove problematic observations
    !is.na(yield_value),                    # Remove missing yield values
    !is.na(longitude),                      # Remove missing coordinates
    !is.na(latitude),
    yield_value <= 30,                      # Remove unrealistic yield outliers (>30 tons/ha)
    nchar(as.character(Observation.period)) == 4  # Keep only 4-digit years
  )

cat("Cleaned dataset dimensions:", dim(yield), "\n")
cat("Yield range:", range(yield$yield_value), "tons/ha\n")

# Summary statistics for the cleaned dataset
summary(yield)

###################################################################

# Geographic Visualization for yield values ----
# Create color palette for yield values
pal <- colorNumeric(palette = "YlOrRd", domain = yield$yield_value)

# Create interactive map showing global wheat yield distribution
leaflet_map <- leaflet(yield) %>%
  addTiles() %>%  # Add base map tiles
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~pal(yield_value),  # Color by yield value
    popup = ~paste("Country:", Country, "<br>",
                   "Location:", Location, "<br>",
                   "Yield:", round(yield_value, 2), "tons/ha"),
    radius = 3,
    fillOpacity = 0.7
  ) %>%
  addLegend("bottomright", 
            pal = pal,
            values = ~yield_value, 
            title = "Yield (tons/ha)")

# Display the map
leaflet_map

############################Map by AEZ points ##########################

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

world_map_aez <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.3) +
  geom_point(data = yield, 
             aes(x = longitude, y = latitude, color = factor(AEZ)), 
             size = 3, 
             alpha = 0.8) +
  scale_color_viridis_d(name = "AEZ Zone") +
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(
    title = "Wheat Observations by AEZ Zone",
    subtitle = "Agro-Ecological Zone Distribution",
    caption = "Data: Merged wheat dataset"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom"
  )

world_map_aez

########################################################################

# Temporal Analysis ----
# Histogram showing data collection patterns over years
temporal_plot <- ggplot(yield, aes(x = Observation.period)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Wheat Yield Observations Over Time", 
       x = "Year", 
       y = "Number of Observations") +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(temporal_plot)

# Summary of observations and yield by year
yearly_summary <- yield %>%
  group_by(Observation.period) %>%
  summarise(
    n_observations = n(),
    mean_yield = round(mean(yield_value), 2),
    median_yield = round(median(yield_value), 2),
    n_countries = n_distinct(Country),
    .groups = 'drop'
  ) %>%
  arrange(desc(Observation.period))

# Display recent years
head(yearly_summary, 10) %>%
  kable(caption = "Recent Years: Observations and Yield Statistics")

########################################################################################

# Geographic Distribution Analysis ----
# Bar plot showing observations by country, colored by continent
country_plot <- ggplot(yield, aes(x = reorder(Country, -table(Country)[Country]), fill = Continent)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Number of Wheat Yield Observations by Country and Continent", 
       x = "Country", 
       y = "Number of Observations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_fill_viridis_d(option = "plasma")

print(country_plot)

# Create organized summary table by country and continent
country_continent_table <- yield %>%
  group_by(Country, Continent) %>%
  summarise(
    count = n(),
    mean_yield = round(mean(yield_value), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(count))

# Display as formatted table
country_table <- country_continent_table %>%
  head(15) %>%
  gt() %>%
  tab_header(title = "Top 15 Countries by Number of Observations") %>%
  cols_label(
    Country = "Country",
    Continent = "Continent", 
    count = "Observations",
    mean_yield = "Mean Yield (tons/ha)"
  ) %>%
  fmt_number(columns = mean_yield, decimals = 2)

print(country_table)

##################################################################################################

# Missing Data Analysis ----
# Prepare data for missing value heatmap
missing_heatmap_data <- data %>%
  group_by(Country) %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100), .groups = 'drop') %>%
  pivot_longer(cols = -Country, 
               names_to = "Variable", 
               values_to = "Percent_Missing")

# Create heatmap visualization
missing_plot <- ggplot(missing_heatmap_data, aes(x = Variable, y = Country, fill = Percent_Missing)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = "% Missing") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = "Missing Data Heatmap by Country", 
       x = "Variable", 
       y = "Country")

print(missing_plot)

##################################################################################################


# Interactive heatmap by country ----
# Percentage missing for all variables
pct_missing_all <- data %>%
  group_by(Country) %>%
  summarise(
    Total_Obs = n(),
    across(everything(), ~round(mean(is.na(.)) * 100, 2), .names = "Pct_NA_{.col}")
  ) %>%
  arrange(desc(Total_Obs))

# Create the heatmap_data
heatmap_data <- pct_missing_all %>%
  select(-Total_Obs) %>%
  pivot_longer(cols = -Country, 
               names_to = "Variable", 
               values_to = "Pct_Missing",
               names_prefix = "Pct_NA_")

# Create interactive heatmap
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
    title = "Missing Data Heatmap",
    xaxis = list(title = "Variable"),
    yaxis = list(title = "Country")
  )

print(interactive_heatmap)

##################################################################################################


# Variable Categories Overview ----
# Categorize variables by type
location_time_vars <- c("id", "Data.ID", "Location", "State.Region.County.Province", 
                        "Country", "Continent", "Latitude..N.S.", "Longitude..E.W.", 
                        "Conversion.for.latitude", "Conversion.for.longitude", 
                        "Location.source", "Observation.period", "X", "Y", "unit_code")

crop_management_vars <- c("Wheat.Type", "Crop.variety", "Tillage.type", "Planting.date", 
                          "Harvesting.date", "Flowering.stage", "Treatment", "Treatment.type", 
                          "Water.regime", "Irrigation..mm.", "N.type", "N.rate..kg.N.ha.1.", 
                          "N.fertilizer.management", "P.type", "P.rate..kg.P.ha.1.", 
                          "Straw.return", "Plastic.film.mulching", "Planting.date.1", 
                          "Harvesting.date.1", "rainfed_start_date", "rainfed_end_date", 
                          "irr_start_date", "irr_end_date", "start_date", "end_date")

climate_vars <- c("Climate", "Mean.annual.temperature..Â.C.", "Mean.annual.precipitation..mm.", 
                  "temp1", "temp2", "temp3", "temp4", "temp5", "temp6", "temp7", "temp8", "temp9",
                  "prc1", "prc2", "prc3", "prc4", "prc5", "prc6", "prc7", "prc8", "prc9", 
                  "pr_irrigated", "AEZ", "Elevation")

soil_vars <- c("Soil.type", "Soil.depth..cm.", "Sand", "Silt", "Clay", "Soil.texture", 
               "Soil.organic.carbon..g.C.kg.1.", "Soil.organic.carbon....", "TN..g.N.kg.1.", 
               "C.N.ratio", "Soil.pH", "BD..g.cm.3.", "Soil_N")

yield_performance_vars <- c("Grain.yield..tons.ha.1.", "SE...22", "PFPN..kg.kg..1.", 
                            "ANE..kg.kg.1.", "Replicates")

environmental_vars <- c("Cumulative.N2O.fluxes..kg.N.ha.1.", "SE...56", "SD...57", 
                        "Yield.scaled.N2O.emission..g.N.Mg.1.", "SE...59", "SD...60", 
                        "EFd....", "Emissions..yes.no.")

pest_weed_vars <- grep("Pest|weed|Main.weed", names(data), value = TRUE, ignore.case = TRUE)

# Create summary table
category_summary <- data.frame(
  Category = c("Location & Time", "Crop & Management", "Climate", "Soil Properties", 
               "Yield & Performance", "Environmental Impact", "Pest & Weed"),
  Variables = c(length(location_time_vars), length(crop_management_vars), 
                length(climate_vars), length(soil_vars), length(yield_performance_vars),
                length(environmental_vars), length(pest_weed_vars)),
  Data_Types = c("Mixed", "Mixed", "Numeric", "Mixed", "Numeric", "Numeric", "Mixed")
)

category_table <- category_summary %>%
  gt() %>%
  tab_header(title = "Dataset Structure: Variables by Category") %>%
  cols_label(
    Category = "Variable Category",
    Variables = "Number of Variables",
    Data_Types = "Primary Data Types"
  )

print(category_table)

# Show data types breakdown
data_types <- data %>%
  summarise_all(class) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Type") %>%
  count(Type, name = "Count") %>%
  arrange(desc(Count))

data_types_table <- data_types %>%
  gt() %>%
  tab_header(title = "Data Types Distribution") %>%
  cols_label(Type = "Data Type", Count = "Number of Variables")

print(data_types_table)

# Data Coverage Summary ----
# Function to calculate completeness for variable groups
calculate_completeness <- function(data, var_list, category_name) {
  if(length(var_list) > 0) {
    available_vars <- intersect(var_list, names(data))
    if(length(available_vars) > 0) {
      complete_any <- data %>%
        select(all_of(available_vars)) %>%
        mutate(has_any_data = rowSums(!is.na(.)) > 0) %>%
        pull(has_any_data) %>%
        sum()
      
      complete_all <- data %>%
        select(all_of(available_vars)) %>%
        complete.cases() %>%
        sum()
      
      return(data.frame(
        Category = category_name,
        Variables_Available = length(available_vars),
        Obs_With_Any_Data = complete_any,
        Obs_With_Complete_Data = complete_all,
        Pct_Any_Data = round(complete_any / nrow(data) * 100, 1),
        Pct_Complete_Data = round(complete_all / nrow(data) * 100, 1)
      ))
    }
  }
  return(NULL)
}

# Calculate completeness for each category
coverage_summary <- bind_rows(
  calculate_completeness(data, climate_vars, "Climate"),
  calculate_completeness(data, soil_vars, "Soil Properties"),
  calculate_completeness(data, crop_management_vars, "Crop & Management"),
  calculate_completeness(data, yield_performance_vars, "Yield & Performance"),
  calculate_completeness(data, environmental_vars, "Environmental Impact")
)

coverage_table <- coverage_summary %>%
  gt() %>%
  tab_header(title = "Data Coverage by Category") %>%
  cols_label(
    Category = "Variable Category",
    Variables_Available = "Variables",
    Obs_With_Any_Data = "Obs. w/ Any Data",
    Obs_With_Complete_Data = "Obs. w/ Complete Data",
    Pct_Any_Data = "% Any Data",
    Pct_Complete_Data = "% Complete"
  )

print(coverage_table)

# Variable Completeness Dashboard ----
# Create comprehensive variable summary
var_summary <- data %>%
  summarise_all(list(
    count = ~sum(!is.na(.)),
    pct_complete = ~round(sum(!is.na(.)) / n() * 100, 1),
    min_val = ~ifelse(is.numeric(.), round(min(., na.rm = TRUE), 2), NA),
    max_val = ~ifelse(is.numeric(.), round(max(., na.rm = TRUE), 2), NA),
    n_unique = ~n_distinct(., na.rm = TRUE)
  )) %>%
  pivot_longer(everything(), names_to = "variable_stat", values_to = "value") %>%
  separate(variable_stat, into = c("variable", "statistic"), sep = "_(?=count$|pct_complete$|min_val$|max_val$|n_unique$)") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  arrange(desc(pct_complete))

# Display all variables by completeness
var_summary_table <- var_summary %>%
  gt() %>%
  tab_header(title = "All Variables by Data Completeness") %>%
  cols_label(
    variable = "Variable",
    count = "Count",
    pct_complete = "% Complete",
    min_val = "Min Value",
    max_val = "Max Value", 
    n_unique = "Unique Values"
  ) %>%
  fmt_number(columns = c(pct_complete), decimals = 1)

print(var_summary_table)

# Completeness Visualization ----
# Completeness distribution
completeness_plot <- ggplot(var_summary, aes(x = pct_complete)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Variable Completeness",
       x = "Percentage Complete",
       y = "Number of Variables") +
  geom_vline(xintercept = 50, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 55, y = max(table(cut(var_summary$pct_complete, 20))) * 0.8, 
           label = "50% threshold", color = "red")

print(completeness_plot)

# ML Readiness Assessment ----
# Assess dataset readiness for ML
ml_readiness <- data.frame(
  Assessment_Category = c(
    "Sample Size",
    "Geographic Coverage", 
    "Temporal Coverage",
    "Target Variable (Yield)",
    "Climate Variables",
    "Soil Variables", 
    "Management Variables",
    "Overall ML Readiness"
  ),
  
  Status = c(
    ifelse(nrow(data) >= 100, "✓ Good", "⚠ Limited"),
    ifelse(length(unique(data$Country)) >= 5, "✓ Good", "⚠ Limited"),
    ifelse(max(data$Observation.period, na.rm = TRUE) - min(data$Observation.period, na.rm = TRUE) >= 10, "✓ Good", "⚠ Limited"),
    ifelse(sum(!is.na(data$Grain.yield..tons.ha.1.)) / nrow(data) >= 0.8, "✓ Good", "⚠ Limited"),
    ifelse(sum(!is.na(data$Mean.annual.temperature..Â.C.)) / nrow(data) >= 0.5, "✓ Good", "⚠ Limited"),
    ifelse(sum(!is.na(data$Soil.pH)) / nrow(data) >= 0.3, "✓ Good", "⚠ Limited"),
    ifelse(sum(!is.na(data$N.rate..kg.N.ha.1.)) / nrow(data) >= 0.3, "✓ Good", "⚠ Limited"),
    "Ready for Analysis"
  ),
  
  Details = c(
    paste(nrow(data), "observations"),
    paste(length(unique(data$Country)), "countries"),
    paste(max(data$Observation.period, na.rm = TRUE) - min(data$Observation.period, na.rm = TRUE), "year span"),
    paste(round(sum(!is.na(data$Grain.yield..tons.ha.1.)) / nrow(data) * 100, 1), "% complete"),
    paste(round(sum(!is.na(data$Mean.annual.temperature..Â.C.)) / nrow(data) * 100, 1), "% complete"),
    paste(round(sum(!is.na(data$Soil.pH)) / nrow(data) * 100, 1), "% complete"),
    paste(round(sum(!is.na(data$N.rate..kg.N.ha.1.)) / nrow(data) * 100, 1), "% complete"),
    "Proceed with feature selection"
  )
)

ml_readiness_table <- ml_readiness %>%
  gt() %>%
  tab_header(title = "Machine Learning Readiness Assessment") %>%
  cols_label(
    Assessment_Category = "Category",
    Status = "Status",
    Details = "Details"
  )

print(ml_readiness_table)

# Summary 
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Dataset contains", nrow(data), "observations from", length(unique(data$Country)), "countries\n")
cat("Time period:", range(data$Observation.period, na.rm = TRUE), "\n")
cat("Target variable (yield) completeness:", round(sum(!is.na(data$Grain.yield..tons.ha.1.)) / nrow(data) * 100, 1), "%\n")
