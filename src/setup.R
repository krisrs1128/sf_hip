
################################################################################
# Simple experiment using canonical correlation on the SF HIP data
################################################################################

## ---- libraries ----
library("ade4")
library("data.table")
library("FactoMineR")
library("ggplot2")
library("maptools")
library("rgdal")
library("stringr")
library("plyr")
library("dplyr")

## ---- get-data ----
tract_info <- fread("~/Documents/programming/sf_hip/data/processed_data/crime_census_alcohol.csv")
census_ids <- fread("~/Documents/programming/sf_hip/data/processed_data/census_tract_demographics.csv") %>%
    select(Tract2010, Id)
