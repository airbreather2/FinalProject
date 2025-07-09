#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: R datacheck.R
# Description: Check features of the prepped dataset

#############Modules #################

library(geosphere)
 
#####################################


#set seed to test random forest
set.seed(1010)

#Source prepped data 
source("dataprep.R")

table(wheat_rf$Observation.period)

length(unique(wheat_rf$Location))

# Check data loss
cat("Rows after cleaning:", nrow(wheat_rf), "\n")
cat("Data retention:", round(100 * nrow(wheat_rf) / nrow(wheat1), 1), "%\n")


dist_matrix <- distm(coordinates_matrix, fun = distHaversine)