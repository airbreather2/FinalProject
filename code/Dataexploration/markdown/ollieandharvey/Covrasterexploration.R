#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: Covrasterexploration.R
# Description: exploring rasters provided by salar
# Arguments: 
# Date: 24-06-2025

# Usage:used to explore salars cov rasters
# 
# To run the script:
# Rscript Covrasterexploration.R

library(raster)
library(terra)
library(sf)
library(ggplot2)
library(viridis)
library(tmap)
library(leaflet)

####################Open rasters #####################################

list.files("../../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/")


#AEZ Raster
AEZ <- "../../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif"
AEZ_RASTER <- raster(AEZ)  # Load the TIF file


################BASIC EXPLORATION#########################################

# Basic information
print("=== BASIC RASTER INFO ===")
print(AEZ_RASTER)

# Get coordinate reference system
print("\n=== COORDINATE SYSTEM ===")
cat("CRS:", as.character(crs(AEZ_RASTER)), "\n")

# Get unique values (these are your AEZ zone codes)
print("\n=== UNIQUE AEZ ZONE VALUES ===")
unique_zones <- unique(values(AEZ_RASTER))
unique_zones <- unique_zones[!is.na(unique_zones)]
unique_zones <- sort(unique_zones)
print(unique_zones)
cat("Total number of zones:", length(unique_zones), "\n")

# Get frequency table of zones
print("\n=== AEZ ZONE FREQUENCY ===")
zone_freq <- freq(AEZ_RASTER)
print(zone_freq)

# Basic statistics
print("\n=== BASIC STATISTICS ===")
cat("Min value:", minValue(AEZ_RASTER), "\n")
cat("Max value:", maxValue(AEZ_RASTER), "\n")
cat("Mean value:", mean(values(AEZ_RASTER), na.rm = TRUE), "\n")
