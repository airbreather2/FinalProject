#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: 
# Description: Exploration of the wheatdataset and visualisations to show dataset structure
# Arguments: 
# Date: may 2025 

# Usage:
# 
# To run the script:
# Rscript 

################################################################
library(tidyverse)  # For data manipulation and visualization
library(sf)         # For spatial data handling
library(tidyverse)
library(terra)
library(raster)
library(sf)
library(sp)
library(dismo)
library(sf)        # core vector GIS package
library(units)     # used for precise unit conversion
library(geodata)   # Download and load functions for core datasets
library(openxlsx)  # Reading data from Excel files
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(gtsummary)  
library(furrr)
library(leaflet)
library(ggplot2)
library(dplyr)
library(gt)
library(tidyr)
library(knitr)
library(plotly)
library(htmlwidgets)
#################################################################
#Use R markdown to create a nice data summary report for anna

#use gt tables for nice looking data summaries after cleaning

# Wheat data with irrigation and rainfall calendar
data <- read.csv("../../data/Data/wheat_data_with_calendar.csv", fileEncoding = "latin1")

# First, check if data loaded properly
head(data)  # View first few rows
names(data)  # See all column names
str(data)   # See structure of the data

# Select and rename columns for easier use
yield <- data %>%
  select(Country, 
         Observation.period, 
         Location, 
         Continent, 
         State.Region.County.Province, 
         longitude = Conversion.for.longitude,    # Just rename here
         latitude = Conversion.for.latitude,      # Just rename here
         yield_value = Grain.yield..tons.ha.1.) %>%
  mutate(
    longitude = as.numeric(longitude),     # Convert to numeric here
    latitude = as.numeric(latitude),       # Convert to numeric here
    yield_value = as.numeric(yield_value),  # Convert to numeric here
    Observation.period = as.numeric(Observation.period)
  ) %>%  
  filter(!is.na(yield_value),                       
         !is.na(longitude),
         !is.na(latitude),
         yield_value <= 30)

#filter Observation years that have more than 4 characters
yield <- yield %>% filter(nchar(Observation.period) == 4)


# Create color palette
pal <- colorNumeric(palette = "YlOrRd", domain = yield$yield_value)

# Create Leaflet Map
leaflet(yield) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,  # Use the renamed column
    lat = ~latitude,   # Use the renamed column
    color = ~pal(yield_value),
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

# Histogram of observations per year
ggplot(yield, aes(x = Observation.period)) +  # Closing parenthesis moved here
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Observations per Year", x = "Year", y = "Count")

#########################Observing the number of occurences per country #####################

# Bar plot of observations per country and continent
ggplot(yield, aes(x = reorder(Country, -table(Country)[Country]), fill = Continent)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Number of Observations per Country and Continent", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#pretty tabular version of this
country_continent_table <- yield %>%
  group_by(Country, Continent) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

country_continent_table %>%
  gt() %>%
  tab_header(title = "Number of Observations per Country and Continent") %>%
  cols_label(
    Country = "Country",
    Continent = "Continent", 
    count = "Number of Observations"
  )

#############################Visual Heatmap of NA values across dataset############################

# Prepare data for heatmap
missing_heatmap_data <- data %>%
  group_by(Country) %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = -Country, 
               names_to = "Variable", 
               values_to = "Percent_Missing")

# Create heatmap
ggplot(missing_heatmap_data, aes(x = Variable, y = Country, fill = Percent_Missing)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = "% Missing") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Missing Data Heatmap by Country", 
       x = "Variable", 
       y = "Country")

#################################Interactive summary table#####################################
              
# Percentage missing for all variables
pct_missing_all <- data %>%
  group_by(Country) %>%
  summarise(
    Total_Obs = n(),
    across(everything(), ~round(mean(is.na(.)) * 100, 2), .names = "Pct_NA_{.col}")
  ) %>%
  arrange(desc(Total_Obs))

kable(pct_missing_all, caption = "Percentage Missing by Country")

kable(detailed_summary, caption = "Missing Data Summary by Country")

#############################Interactive heat map visualization #################################

# First, create the heatmap_data (this was missing)
heatmap_data <- pct_missing_all %>%
  select(-Total_Obs) %>%
  pivot_longer(cols = -Country, 
               names_to = "Variable", 
               values_to = "Pct_Missing",
               names_prefix = "Pct_NA_")

# Create interactive heatmap - use heatmap_data instead of data
p <- plot_ly(
  data = heatmap_data,  # This was the issue - should be heatmap_data, not data
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

# Save as HTML
htmlwidgets::saveWidget(p, "visualisation/missing_heatmap_interactive.html")

##################################################################################
# Function to create missing data heatmap by continent
create_missing_data_heatmap <- function(data, save_path = "visualisation/missing_heatmap_by_continent.html") {
  
  # Get unique continents and ensure they're not NA
  unique_continents <- data %>%
    filter(!is.na(Continent)) %>%
    pull(Continent) %>%
    unique()
  
  print(paste("Found", length(unique_continents), "continents:"))
  print(unique_continents)
  
  # Calculate percentage missing by continent for all columns
  pct_missing_continent <- data %>%
    filter(!is.na(Continent)) %>%
    group_by(Continent) %>%
    summarise(
      Total_Obs = n(),
      # Calculate NA percentage for all columns (not just numeric)
      across(everything(), 
             ~round(mean(is.na(.)) * 100, 2), 
             .names = "Pct_NA_{.col}")
    ) %>%
    # Remove the redundant Pct_NA_Continent column
    select(-Pct_NA_Continent) %>%
    arrange(desc(Total_Obs))
  
  print("Missing percentages calculated for continents:")
  print(pct_missing_continent %>% select(Continent, Total_Obs))
  
  # Reshape data for heatmap
  heatmap_data <- pct_missing_continent %>%
    select(-Total_Obs) %>%
    pivot_longer(
      cols = starts_with("Pct_NA_"), 
      names_to = "Variable", 
      values_to = "Pct_Missing",
      names_prefix = "Pct_NA_"
    )
  
  # Clean variable names for better display
  heatmap_data <- heatmap_data %>%
    mutate(
      # Replace underscores with spaces and proper capitalization
      Variable = str_replace_all(Variable, "_", " "),
      Variable = str_to_title(Variable)
    ) %>%
    # Filter out variables with no missing data across all continents
    group_by(Variable) %>%
    filter(sum(Pct_Missing) > 0) %>%
    ungroup()
  
  # Order variables by average missing percentage for better visualization
  variable_order <- heatmap_data %>%
    group_by(Variable) %>%
    summarise(Avg_Missing = mean(Pct_Missing)) %>%
    arrange(Avg_Missing) %>%
    pull(Variable)
  
  heatmap_data$Variable <- factor(heatmap_data$Variable, levels = variable_order)
  
  # Count variables for dynamic sizing
  n_variables <- length(unique(heatmap_data$Variable))
  n_continents <- length(unique(heatmap_data$Continent))
  
  print(paste("Creating heatmap with", n_variables, "variables and", n_continents, "continents"))
  
  # Dynamic sizing based on number of variables
  plot_width <- max(800, n_variables * 35)  # At least 35px per variable
  plot_height <- max(400, n_continents * 60)  # At least 60px per continent
  
  # Create color scale with better contrast
  colors <- list(
    c(0, '#00429d'),    # Dark blue for 0%
    c(0.25, '#93c4d4'), # Light blue for 25%
    c(0.5, '#eeeeee'),  # White/grey for 50%
    c(0.75, '#f4a582'), # Light red for 75%
    c(1, '#ca0020')     # Dark red for 100%
  )
  
  # Create interactive heatmap
  p <- plot_ly(
    data = heatmap_data,
    x = ~Variable,
    y = ~Continent,
    z = ~Pct_Missing,
    type = "heatmap",
    colorscale = colors,
    colorbar = list(
      title = "Missing %",
      tickmode = "linear",
      tick0 = 0,
      dtick = 20
    ),
    hovertemplate = paste(
      "<b>Continent:</b> %{y}<br>",
      "<b>Variable:</b> %{x}<br>",
      "<b>Missing:</b> %{z}%<br>",
      "<extra></extra>"
    ),
    text = ~round(Pct_Missing, 1),
    texttemplate = "%{text}",
    textfont = list(
      color = ~ifelse(Pct_Missing > 50, "white", "black"),
      size = 10
    ),
    showscale = TRUE
  ) %>%
    layout(
      title = list(
        text = "Missing Data Heatmap by Continent",
        font = list(size = 20, weight = "bold")
      ),
      xaxis = list(
        title = "Variable",
        tickangle = -45,  # Angled labels for better readability
        tickfont = list(size = 11),
        side = "bottom"
      ),
      yaxis = list(
        title = "Continent",
        tickfont = list(size = 12),
        autorange = "reversed"  # First continent at top
      ),
      height = plot_height,
      width = plot_width,
      margin = list(l = 100, r = 50, t = 50, b = 150),  # Extra bottom margin for labels
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  
  # Display the plot
  print(p)
  
  # Save the plot
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    htmlwidgets::saveWidget(
      p, 
      save_path,
      selfcontained = TRUE,
      title = "Missing Data Heatmap by Continent"
    )
    print(paste("Heatmap saved to:", save_path))
  }
  
  return(p)
}

# Alternative function to create separate heatmaps for variable groups
create_grouped_heatmaps <- function(data, variable_groups = NULL) {
  
  # Define variable groups if not provided
  if (is.null(variable_groups)) {
    variable_groups <- list(
      "Environmental" = c("temperature", "rainfall", "elevation", "latitude", "longitude"),
      "Agricultural" = c("yield", "crop", "fertilizer", "pesticide", "irrigation"),
      "Economic" = c("GDP", "income", "price", "market", "trade"),
      "Social" = c("population", "education", "health", "labor", "gender")
    )
  }
  
  # Create a heatmap for each group
  plots <- list()
  
  for (group_name in names(variable_groups)) {
    group_vars <- variable_groups[[group_name]]
    
    # Filter data for variables in this group
    filtered_data <- data %>%
      select(Continent, any_of(group_vars), contains(group_vars))
    
    if (ncol(filtered_data) > 1) {  # If we have variables beyond Continent
      p <- create_missing_data_heatmap(
        filtered_data,
        save_path = paste0("visualisation/missing_heatmap_", 
                           tolower(gsub(" ", "_", group_name)), 
                           ".html")
      )
      plots[[group_name]] <- p
    }
  }
  
  return(plots)
}

# Usage example:
# Create main heatmap with all variables
p_all <- create_missing_data_heatmap(data)

###################################################################################################
#Code generated that 


# gives a summary of number of observation years
yield_summary <- yield %>%
  group_by(Country) %>%
  summarise(
    n_years = n_distinct(Observation.period),
    year_range = paste(min(Observation.period), "-", max(Observation.period)),
    avg_yield = mean(yield_value),
    yield_trend = ifelse(cor(Observation.period, yield_value) > 0, "Increasing", "Decreasing")
  ) %>%
  arrange(desc(n_years))

print(yield_summary)

##################################################################################################
#what ML methods might be applicable? 
# Which countries have enough data for ML?
modeling_readiness <- yield %>%
  group_by(Country) %>%
  summarise(
    n_observations = n(),
    year_span = max(Observation.period) - min(Observation.period),
    consecutive_years = sum(diff(sort(unique(Observation.period))) == 1),
    data_density = n_observations / year_span
  ) %>%
  mutate(
    suitable_for_ML = case_when(
      n_observations >= 10 & year_span >= 5 ~ "Good",
      n_observations >= 5 & year_span >= 3 ~ "Moderate",
      TRUE ~ "Poor"
    )
  )

#try and figure out how the heatmap was generated and then try to find a ranking system of countries regarding availability and count etc

