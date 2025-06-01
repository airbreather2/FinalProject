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
library(openxlsx)  # Reading data from Excel files
library(ggplot2)
library(gtsummary)  
library(furrr)
library(ggplot2)
library(dplyr)
library(gt)
library(tidyr)
library(knitr)
library(plotly)
library(htmlwidgets)
library(lubridate)
#################################################################
# Set a seed for reproducibility
set.seed(123)  # Pick any number you like

#load in wheatdata with calendar    
calendar <- read.csv("../../../data/Data/wheat_data_with_calendar.csv", fileEncoding = "latin1")
names(calendar)
#load in wheat data covariates
covariates <- read.csv("../../../data/Data/wheat_data_with_covariats.csv", fileEncoding = "latin1")

############Renaming dumbahh column names for easier wrangling#####################################
#checking names are in order
names(calendar)[grepl("Sand|Silt|Clay|Soil_N|start_date|end_date|pr_irrigated", names(calendar))]
names(covariates)[grepl("Sand|Silt|Clay|Soil_N", names(covariates))]

#Rename Calendar column names
calendar <- calendar %>% 
  rename(
    Sand = `Sand....`,    # replace with actual names you found
    Silt = `Silt....`,
    Clay = `Clay....`
  )

########Test if ID's are the same to make sure correct data is joined ############################
#get test samples conditionally 
# First filter out NA values, then get random sample
testsample <- calendar %>% 
  filter(!is.na(TN..g.N.kg.1.)) %>% 
  sample_n(10)


# Get random sample
testsample <- sample_n(calendar, 10)

# Extract the IDs from your sample
sample_ids <- testsample$id

# Use those IDs to filter covariates
covtest <- subset(covariates, id %in% sample_ids)
# visually compare in global env
####################################another test#########################################

# Check the overlap
existing_ids <- unique(calendar$id)  # your existing dataset
new_ids <- unique(covariates$id)
length(intersect(existing_ids, new_ids))  # how many IDs overlap?
length(setdiff(new_ids, existing_ids))    # how many are truly new?

###############################They are the same! #############################################
#DOESNT WORK
# Convert string "NA" to actual NA values first
covs <- covariates %>% 
  mutate(across(everything(), ~ifelse(. == "NA", NA, .))) %>%
  # Then filter rows that aren't all NA
  filter(if_any(everything(), ~!is.na(.)))

#filter any rows which are entirely NAs 
calendar <- calendar %>% 
  filter(if_any(everything(), ~!is.na(.)))
###############################rename columns that are to be merged/added from cov dataset ######################
#select all of the correct rows! ask salar or find out what start date and end date refers to cause lord knows I dont
calendar_covs <- covs %>% select(
  id,
  Location = location,
  Continent = continent, 
  Observation.period = year,
  Grain.yield..tons.ha.1. = yield,
  Sand,
  Silt,
  Clay,
  Soil.organic.carbon..g.C.kg.1. = SOC,
  Soil.pH = PH,
  Soil_N,
  pr_irrigated,
  start_date,
  end_date,
  AEZ,
  Elevation,
  pr_irrigated,
  temp1,
  temp2,
  temp3,
  temp4,
  temp5,
  temp6,
  temp7,
  temp8,
  temp9,
  prc1,
  prc2,
  prc3,
  prc4,
  prc5,
  prc6,
  prc7,
  prc8,
  prc9,
)



##########################Time to MERGE ########################################################
#make sure all of the names in calendar are the same as those in calendar cov
intersect(names(calendar), names(calendar_covs))

       
       
#merged values
mergedataset <- merge(calendar, calendar_covs, by = "id", suffixes = c("_old", "_new")) 

# Just see what's missing
columns_to_check <- c("id", "Location_old", "Location_new", "Continent_old", "Continent_new", 
                      "Observation.period_old", "Observation.period_new", "Grain.yield..tons.ha.1._old", 
                      "Grain.yield..tons.ha.1._new", "Sand_old", "Sand_new", "Silt_old", "Silt_new", 
                      "Clay_old", "Clay_new", "Soil.organic.carbon..g.C.kg.1._old", 
                      "Soil.organic.carbon..g.C.kg.1._new", "Soil.pH_old", "Soil.pH_new", 
                      "Soil_N_old", "Soil_N_new", "pr_irrigated_old", "pr_irrigated_new", 
                      "start_date_old", "start_date_new", "end_date_old", "end_date_new", 
                      "AEZ", "Elevation", "Silt", "Clay", "Soil_N", "pr_irrigated")

columns_to_check[!columns_to_check %in% names(mergedataset)]



###################################Create merging function############################################

#Created function to overwrite old column to match new column
overwrite <- function(dataset, columnold, columnnew){
  # Use the old column name as the output column name
  dataset[[columnold]] <- ifelse(is.na(dataset[[columnnew]]), 
                                 dataset[[columnold]], 
                                 dataset[[columnnew]])
  # Remove a single column
  dataset[[columnnew]] <- NULL
  
  # Return the modified dataset
  return(dataset)
}

# Systematic consolidation of covariates with dual nomenclatural representations
mergedataset <- overwrite(mergedataset, "Location_old", "Location_new")
mergedataset <- overwrite(mergedataset, "Continent_old", "Continent_new") 
mergedataset <- overwrite(mergedataset, "Observation.period_old", "Observation.period_new")
mergedataset <- overwrite(mergedataset, "Grain.yield..tons.ha.1._old", "Grain.yield..tons.ha.1._new")
mergedataset <- overwrite(mergedataset, "Sand_old", "Sand_new")
mergedataset <- overwrite(mergedataset, "Silt_old", "Silt_new")
mergedataset <- overwrite(mergedataset, "Clay_old", "Clay_new")
mergedataset <- overwrite(mergedataset, "Soil.organic.carbon..g.C.kg.1._old", "Soil.organic.carbon..g.C.kg.1._new")
mergedataset <- overwrite(mergedataset, "Soil.pH_old", "Soil.pH_new")
mergedataset <- overwrite(mergedataset, "Soil_N_old", "Soil_N_new")
mergedataset <- overwrite(mergedataset, "start_date_old", "start_date_new")
mergedataset <- overwrite(mergedataset, "end_date_old", "end_date_new")

#check if merge went correctly 
names(mergedataset)[grepl("_new", names(mergedataset))]
#_new columns were successfully removed! 

#lets remove the _old suffixes :)
names(mergedataset) <- gsub("_old$", "", names(mergedataset))
names(mergedataset)[grepl("_old", names(mergedataset))]
#worked

length(names(mergedataset))


#write csv file to save final manipulated dataset

write.csv(mergedataset, "../../../data/Data/23-05-mergedwheatdata.csv", row.names = FALSE)



