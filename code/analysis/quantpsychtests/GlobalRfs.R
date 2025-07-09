#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: RandomforestGLMs.R
# Description: Experimenting with random forest and GLMs to see if these approaches can be used
# Arguments: 
# Date: June 2025

# Usage:
# 
# To run the script:
# Rscript RandomforestGLMs.R


##########Modules##########################################
library(tidyverse)  # For data manipulation and visualization
library(openxlsx)  # Reading data from Excel files
library(ggplot2)
library(gtsummary)  
library(furrr)
library(ggplot2)
library(dplyr)
library(gt)
library(tidyr)
library(randomForest) #using this package as variables are automatically converted to characters/factors
library(ranger)
###########################################################
#set seed to source data


#set seed to test random forest
set.seed(1010)


source("dataprep.R")

##########################################################Run overall RF ################################################

# Run Random Forest
rf_model <- ranger(
  Grain.yield..tons.ha.1. ~ ., 
  data = wheat_rf,
  num.trees  = 500,
  importance = "impurity",
  num.threads = 7
)

# Model summary
print(rf_model)

print(rf_model$variable.importance)

