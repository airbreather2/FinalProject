#!/usr/bin/env Rscript
# Author: Sebastian Dohne <sed24@ic.ac.uk>
# Script: R NAcontinentanalysis.R
# Description: Preparing NA specific dataset to observe variable completeness and ML readiness


# Read data
wheat1 <- read.csv("../../../../data/Data/chatgptdata/23-05-mergedwheatdatafixed.csv", 
                   fileEncoding = "latin1")


wheat1 %>%$ filter(Country == Mexico)
