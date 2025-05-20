#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: cropcalendarcovariates.R
# Description: Datamerging script which combines the crop calendar wheat dataset with the cropcalendar one
# Arguments: 
# Date: may 2025

# Usage:
# 
# To run the script:
# Rscript cropcalendarcovariates.R

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


#load in wheatdata with calendar    
calendar <- read.csv("../../../data/Data/wheat_data_with_calendar.csv", fileEncoding = "latin1")

#load in wheat data covariates
covariates <- read.csv("../../../data/Data/wheat_data_with_covariats.csv", fileEncoding = "latin1")



