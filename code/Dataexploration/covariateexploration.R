#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: 
# Description: Data exploration of crop calendar wheat dataset
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

#open wheat with covariate data
data <- read.csv("../../data/Data/wheat_data_with_covariats.csv", fileEncoding = "latin1")

# First, check if data loaded properly
head(data)  # View first few rows
names(data)  # See all column names
str(data)   # See structure of the data

# Select and rename columns for easier use
yield <- data %>%
  select(country, 
         year, 
         location, 
         continent, 
         longitude = x,    # Just rename here
         latitude = y,      # Just rename here
         yield_value = yield) %>%
  mutate(
    longitude = as.numeric(longitude),     # Convert to numeric here
    latitude = as.numeric(latitude),       # Convert to numeric here
    yield_value = as.numeric(yield_value),  # Convert to numeric here
    Observation.period = as.numeric(year)
  ) %>%  
  filter(!is.na(yield_value),                       
         !is.na(longitude),
         !is.na(latitude),
         yield_value <= 30)


# Create color palette
pal <- colorNumeric(palette = "YlOrRd", domain = yield$yield_value)

# Create Leaflet Map
leaflet(yield) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,  # Use the renamed column
    lat = ~latitude,   # Use the renamed column
    color = ~pal(yield_value),
    popup = ~paste("country:", country, "<br>",
                   "location:", location, "<br>",
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
ggplot(yield, aes(x = reorder(country, -table(country)[country]), fill = continent)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Number of Observations per country and continent", x = "country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#pretty tabular version of this
country_continent_table <- yield %>%
  group_by(country, continent) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

country_continent_table %>%
  gt() %>%
  tab_header(title = "Number of Observations per country and continent") %>%
  cols_label(
    country = "country",
    continent = "continent", 
    count = "Number of Observations"
  )

#############################Visual Heatmap of NA values across dataset############################

# Prepare data for heatmap
missing_heatmap_data <- data %>%
  group_by(country) %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = -country, 
               names_to = "Variable", 
               values_to = "Percent_Missing")

# Create heatmap
ggplot(missing_heatmap_data, aes(x = Variable, y = country, fill = Percent_Missing)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = "% Missing") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Missing Data Heatmap by country", 
       x = "Variable", 
       y = "country")

#################################Interactive summary table#####################################

# Percentage missing for all variables
pct_missing_all <- data %>%
  group_by(country) %>%
  summarise(
    Total_Obs = n(),
    across(everything(), ~round(mean(is.na(.)) * 100, 2), .names = "Pct_NA_{.col}")
  ) %>%
  arrange(desc(Total_Obs))

kable(pct_missing_all, caption = "Percentage Missing by country")

kable(detailed_summary, caption = "Missing Data Summary by country")

#############################Interactive heat map visualization #################################

# First, create the heatmap_data (this was missing)
heatmap_data <- pct_missing_all %>%
  select(-Total_Obs) %>%
  pivot_longer(cols = -country, 
               names_to = "Variable", 
               values_to = "Pct_Missing",
               names_prefix = "Pct_NA_")

# Create interactive heatmap - use heatmap_data instead of data
p <- plot_ly(
  data = heatmap_data,  # This was the issue - should be heatmap_data, not data
  x = ~Variable,
  y = ~country,
  z = ~Pct_Missing,
  type = "heatmap",
  colorscale = "RdYlBu",
  reversescale = TRUE,
  hovertemplate = "country: %{y}<br>Variable: %{x}<br>Missing: %{z}%<extra></extra>"
) %>%
  layout(
    title = "Missing Data Heatmap",
    xaxis = list(title = "Variable"),
    yaxis = list(title = "country")
  )

# Save as HTML
htmlwidgets::saveWidget(p, "visualisation/missing_heatmapcovariates_interactive.html")


#try and figure out how the heatmap was generated and then try to find a ranking system of countries regarding availability and count etc

