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

data <- read.csv("../../data/Data/combined-wheat.csv", fileEncoding = "latin1")


# First, check if data loaded properly
head(data)  # View first few rows
names(data)  # See all column names
str(data)   # See structure of the data

length(unique(data$Country))

# Select and rename columns for easier use - updated for new column names
yield <- data %>%
  select(Country, 
         Observation.period,  # Using this as year equivalent
         Location, 
         Continent, 
         longitude = Longitude..E.W.,    # Updated column name
         latitude = Latitude..N.S.,      # Updated column name
         yield_value = Grain.yield..tons.ha.1.) %>%  # Updated column name
  mutate(
    longitude = as.numeric(longitude),     # Convert to numeric here
    latitude = as.numeric(latitude),       # Convert to numeric here
    yield_value = as.numeric(yield_value)  # Convert to numeric here
  ) %>%  # Remove the extra closing parenthesis here
  filter(!is.na(yield_value),                       
         !is.na(longitude),
         !is.na(latitude),
         yield_value <= 30) %>%
  # Rename columns to match original script expectations
  rename(country = Country,
         location = Location,
         continent = Continent)


# Create color palette
pal <- colorNumeric(palette = "YlOrRd", domain = yield$yield_value)

# Create Leaflet Map
leaflet(yield) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,  # Use the renamed column
    lat = ~latitude,   # Use the renamed column
    color = ~pal(yield_value),
    popup = ~paste("Country:", country, "<br>",
                   "Location:", location, "<br>",
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
  labs(title = "Number of Observations per Country and Continent", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#pretty tabular version of this
country_continent_table <- yield %>%
  group_by(country, continent) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

country_continent_table %>%
  gt() %>%
  tab_header(title = "Number of Observations per Country and Continent") %>%
  cols_label(
    country = "Country",
    continent = "Continent", 
    count = "Number of Observations"
  )

#############################Visual Heatmap of NA values across dataset############################

# Prepare data for heatmap - using original column names from new dataset
missing_heatmap_data <- data %>%
  group_by(Country) %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100), .groups = 'drop') %>%
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


# Basic missing data per column
missing_per_col <- colSums(is.na(data))
missing_pct_per_col <- (missing_per_col / nrow(data)) * 100

# Quick summary
summary(missing_pct_per_col)

# See which variables have the most/least missing data
data.frame(
  variable = names(data), # or colnames(data)
  missing_count = missing_per_col,
  missing_pct = round(missing_pct_per_col, 1)
) %>% 
  arrange(desc(missing_pct)) %>% 
  head(10) # top 10 most missing variables

# Quick histogram to see the distribution
hist(missing_pct_per_col, 
     main = "Distribution of Missing Data by Variable",
     xlab = "Percentage Missing",
     breaks = 20)

# Count how many variables have <50% missing, <75% missing, etc.
table(cut(missing_pct_per_col, breaks = c(0, 25, 50, 75, 90, 100)))



