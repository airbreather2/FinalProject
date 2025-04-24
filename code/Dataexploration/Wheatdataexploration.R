
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
#Use R markdown to create a nice data summary report for anna

#use gt tables for nice looking data summaries after cleaning

# Download a 1:100m scale world map

wheat <- read.csv("../../data/Data/wheat_data.csv")

#initialize parallelisation
plan(multisession, workers = 8)

#create summaries for each country
countries <- unique(wheat$Country)

# Check the structure
str(wheat)
summary(data)

#sample a smaller size to observe the structure of the data
wheat_subset <- wheat %>% sample_n(100)


#complete data summary
wheat_subset %>% tbl_summary(by = Country)

#observe class of every data column
str(wheat)

#pad data with NAs where problems arise
lapply(wheat, `length<-`, max(lengths(wheat)))


create_summary <- function(group_val){
  group_val %>% tbl_summary(by = Country)
}
              