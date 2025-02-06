rm(list=ls())
getwd()

setwd("/Users/karlavasquez/Documents/sopa 400")

load("Harris_County_Cleaned.RData")

ls()

names(bail)  # to view the first few rows if it's a data frame



## Surname Analysis and Data Merging Script

install.packages(c("httr", "jsonlite", "sf"))


install.packages("wru", dependencies = TRUE, type = "binary")
install.packages("tidycensus", dependencies = TRUE, type = "binary")



# Load required libraries
library(tidyr)
library(zoo)
library(sp)
library(ggpubr)
library(tidyverse)
library(lubridate)
library(wru)
library(tidycensus)
library(ggplot2)




library(sp)
library(wru)
library(tidycensus)




###1
# Set Census API key (replace with your key if needed)
census_api_key("d7f7cd3ab0204387684215b28c87070649056422", install = TRUE)

### 1. Download ACS Income Data by ZIP Code Tabulation Area (ZCTA)
acs_zcta <- get_acs(
  geography = "zcta", 
  variables = c(medincome = "B19013_001"),
  year = 2021
)

# Clean ACS data: rename and select columns
acs_zcta_clean <- acs_zcta %>%
  rename(ZIP = GEOID, median_income = estimate) %>%
  select(ZIP, median_income)


##### Error here
### 2. Download ACS County-Level Income Data for Harris County
#harris_income <- get_acs(
 # geography = "county", 
 # variables = c(medincome = "B19013_001"),
 # state = "TX",
 # county = "Harris",
 # year = 2021
#)






# Download ACS County-Level Income Data for Texas counties and then filter for Harris County
tx_counties_income <- get_acs(
  geography = "county", 
  variables = c(medincome = "B19013_001"),
  state = "TX",
  year = 2021
)

# Filter for Harris County (the NAME column usually contains "Harris County, Texas")
harris_income <- tx_counties_income %>%
  filter(NAME == "Harris County, Texas")

# Extract the median income value (assuming one row is returned)
harris_median_income <- harris_income$estimate[1]

# Extract the median income value
harris_median_income <- harris_income$estimate[1]




# Assuming 'bail' has a 'defZIP' column for defendant ZIP codes
# Ensure ZIP codes are character strings
bail <- bail %>%
  mutate(defZIP = as.character(defZIP))

acs_zcta_clean <- acs_zcta_clean %>%
  mutate(ZIP = as.character(ZIP))

# Merge datasets on ZIP code
bail_income <- bail %>%
  left_join(acs_zcta_clean, by = c("defZIP" = "ZIP"))


#####income

# Define income thresholds based on Harris County median income
low_income_threshold <- harris_median_income * 0.5
moderate_income_threshold <- harris_median_income * 0.8
high_income_threshold <- harris_median_income * 1.2

# Categorize income levels
bail_income <- bail_income %>%
  mutate(income_category = case_when(
    is.na(median_income) ~ "Unhoused",
    median_income < low_income_threshold ~ "Low",
    median_income < moderate_income_threshold ~ "Moderate",
    median_income < high_income_threshold ~ "High",
    TRUE ~ "Very High"
  ))




####now, conduct surname






