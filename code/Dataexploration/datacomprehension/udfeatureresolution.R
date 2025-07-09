#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: This script finds the resolutions of the features in the dataset I am looking at
# Description:
# Arguments: 
# Date: 03/07/2024

# module
library(raster)


# Usage:
# 
# To run the script:
# Rscript featureresolution.R

AEZ <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/aez_v9v2red_5m_CRUTS32_Hist_8110_100_avg.tif")
Clay <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/clay_0_30cm.tif")
Elevation <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/elevation_world.tif")
irrigated <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/irrigated_gmia_v5_aei_pct.asc")
ph <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/phh2o_0_30cm.tif")
sand <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/sand_0_30cm.tif")
silt <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/silt_0_30cm.tif")
soc <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/soc_0_30cm.tif")
soilnitrogen <- raster("../../../data/MSc - Sebastian/gbcl-wheat-msc/data-raw/otherdata/soil_nitrogen_0_30cm.tif")

rasters <- list(AEZ, Clay, Elevation, irrigated, ph, sand, silt, soc, soilnitrogen)
names(rasters) <- c("AEZ", "Clay", "Elevation", "irrigated", "ph", "sand", "silt", "soc", "soilnitrogen")
lapply(rasters, res)
