#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: R NAcontinentanalysis.R
# Description: Preparing NA specific dataset to observe variable completeness and ML readiness

####################Modules
library(dplyr)

######################

# Read data
wheat1 <- read.csv("../../../data/Data/chatgptdata/23-05-mergedwheatdatafixed.csv", 
                   fileEncoding = "latin1")
# See all unique country names
unique(wheat1$Country)

#take the USA variables
NAwheat <- wheat1 %>% 
  filter(Country %in% c("Mexico", "United States of America", "Canada"))

NotNA <- wheat1 %>% 
  filter(!Country %in% c("Mexico", "United States of America", "Canada"))

write.csv(NAwheat, "../../../data/Data/explorationdatasets/NAdataset.csv")

write.csv(NotNA, "../../../data/Data/explorationdatasets/AllbutNA.csv")

#Looking at the data 

table(NAwheat$AEZ)

unique(NAwheat$Location)

table(NotNA$AEZ)

unique(NotNA$Location)

table(NotNA$Country)



