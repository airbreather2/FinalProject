#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: dataprep.R
# Description:This prepares the data for model testing, selects appropriate covariates so it can be easily loaded into other scripts

library(tidyverse)
library(gstat)        # For variograms
library(sp)           # Spatial objects
library(geosphere)    # Distance calculations
library(corrplot)     # Correlation plots
library(blockCV)      # Spatial CV
library(spdep)        # For Moran's I
library(ranger)       # For random forest

#set seed to test random forest
set.seed(1010)

# Read data
wheat1 <- read.csv("../../../data/Data/chatgptdata/23-05-mergedwheatdatafixed.csv", 
                   fileEncoding = "latin1")

cat("Original data dimensions:", dim(wheat1), "\n")

# Select variables and clean data in one pipeline
wheat_rf <- wheat1 %>%
  select(
    # Core locational variables
    unit_code, Continent, Country, Location, State.Region.County.Province, Observation.period,
    Latitude..N.S., Longitute.E.W.,Conversion.for.latitude, Conversion.for.longitude,X,Y, Location.source, 
    
    #The almighty response variable
    Grain.yield..tons.ha.1.,
    
    #Location features
    Elevation, AEZ, 
    
    # Climate data (complete variables)
    temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9,
    prc1, prc2, prc3, prc4, prc5, prc6, prc7, prc8, prc9, Mean.annual.precipitation..mm., pr_irrigated, Irrigation..mm.,
    
    # Additional variables (some missing data)
    Soil.pH, Sand, Silt, Clay, Soil_N, N.rate..kg.N.ha.1., Soil.type, P.rate..kg.P.ha.1., N.type, P.type, Emissions..yes.no., 
    
    #Management variables (Sowing and harvesting dates for example)
    start_date, end_date, irr_start_date, irr_end_date, rainfed_start_date, rainfed_end_date, Harvesting.date, Harvesting.date.1, Treatment.type, Tillage.type, Treatment, Planting.date, Plastic.film.mulching, N.fertilizer.management, Straw.return,  
    
    #Biological features
    Wheat.Type, Crop.variety, Pest.prescence....64,  Pest.detected...65, Pest.severity.score.......66, 
    
    
  ) %>%
  drop_na(Grain.yield..tons.ha.1.) %>%  # Remove missing yields
  # na.omit() %>%  # Remove rows with any missing predictors
  filter(
    Grain.yield..tons.ha.1. <= 30,  # Remove unrealistic yields
    nchar(as.character(Observation.period)) == 4
  )

# Check data loss
cat("Rows after cleaning:", nrow(wheat_rf), "\n")
cat("Data retention:", round(100 * nrow(wheat_rf) / nrow(wheat1), 1), "%\n")

write_csv(wheat_rf, "../../../data/finaldatasets/testdata/preliminaryRFdata.csv")

#########################################Do AEZ numbered groupings ############################################

# Create AEZ groupings based on the classification table
# aez_groupings <- data.frame(
#   AEZ = 1:33,
#   new_class = c(
#     # AEZ 1-3: new_class 1 (tropics)
#     1, 1, 1,
#     # AEZ 4-6: new_class 2 (tropics) 
#     2, 2, 2,
#     # AEZ 7-9: new_class 3 (sub-tropics)
#     3, 3, 3,
#     # AEZ 10-12: new_class 4 (sub-tropics)
#     4, 4, 4,
#     # AEZ 13-15: new_class 5 (sub-tropics)
#     5, 5, 5,
#     # AEZ 16-18: new_class 6 (temperate)
#     6, 6, 6,
#     # AEZ 19-21: new_class 7 (temperate)
#     7, 7, 7,
#     # AEZ 22-24: new_class 8 (cold)
#     8, 8, 8,
#     # AEZ 25: new_class 9 (steep terrain)
#     9,
#     # AEZ 26: new_class 10 (severe limitations)
#     10,
#     # AEZ 27-28: new_class 11 (irrigated/hydromorphis)
#     11, 11,
#     # AEZ 29: new_class 12 (desert)
#     12,
#     # AEZ 30: new_class 13 (boreal)
#     13,
#     # AEZ 31: new_class 14 (arctic)
#     14,
#     # AEZ 32: new_class 15 (built-up)
#     15,
#     # AEZ 33: new_class 16 (water)
#     16
#   ),
#   group = c(
#     # AEZ 1-6: tropics
#     rep("tropics", 6),
#     # AEZ 7-15: sub-tropics  
#     rep("sub-tropics", 9),
#     # AEZ 16-21: temperate
#     rep("temperate", 6),
#     # AEZ 22-24: cold
#     rep("cold", 3),
#     # AEZ 25: steep terrain
#     "steep_terrain",
#     # AEZ 26: severe_limitations
#     "severe_limitations", 
#     # AEZ 27-28: irrigated/hydromorphis
#     "irrigated_soils", "irrigated_soils",
#     # AEZ 29: desert
#     "desert",
#     # AEZ 30: boreal
#     "boreal",
#     # AEZ 31: arctic
#     "arctic",
#     # AEZ 32: built-up
#     "built_up",
#     # AEZ 33: water
#     "water"
#   ),
#   description = c(
#     "Lowland: semi-arid", "Lowland: sub-humid", "Lowland: humid",
#     "Highland: semi-arid", "Highland: sub-humid", "Highland: humid", 
#     "Warm: semi-arid", "Warm: sub-humid", "Warm: humid",
#     "moderately cool: semi-arid", "moderately cool: sub-humid", "moderately cool: humid",
#     "Cool: semi-arid", "Cool: sub-humid", "Cool: humid",
#     "Moderate: dry", "Moderate: moist", "Moderate: wet",
#     "Cool: dry", "Cool: moist", "Cool: wet",
#     "no permafrost: dry", "no permafrost: moist", "no permafrost: wet",
#     "Dominantly very steep terrain", "Land with severe soil/terrain limitations",
#     "Land with ample irrigated soils", "Dominantly hydromorphis soils",
#     "Arid climate", "Cold climate", "Very cold climate",
#     "Dominantly built-up land", "Dominantly water"
#   )
# )
# 
# # Add groupings to your data
# wheat_rf <- wheat_rf %>%
#   left_join(aez_groupings, by = "AEZ")
