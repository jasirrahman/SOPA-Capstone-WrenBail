rm(list=ls())
getwd()

setwd("/Users/karlavasquez/Documents/sopa 400")

load("Harris_County_Cleaned.RData")

ls()

#check names of columns
names(bail)

# load packages
library(tidycensus)
library(dplyr)
library(tidyr)
library(lubridate)

# years to include (2014-2023)
years <- c(2014:2023)

# ACS data for multiple years
get_acs_data <- function(year) {
  get_acs(
    geography = "zcta",
    variables = c(medincome = "B19013_001"),
    year = year
  ) %>%
    mutate(year = year) # Add year column for tracking
}

# Retrieve ACS income data for each year and combine into one dataset
acs_zcta_all_years <- bind_rows(lapply(years, get_acs_data))

# Clean and rename columns
acs_zcta_clean <- acs_zcta_all_years %>%
  rename(ZIP = GEOID, median_income = estimate) %>%
  select(ZIP, median_income, year)

# Retrieve county-level income data for each year
tx_counties_income_all_years <- bind_rows(lapply(years, function(y) {
  get_acs(
    geography = "county",
    variables = c(medincome = "B19013_001"),
    state = "TX",
    year = y
  ) %>%
    filter(NAME == "Harris County, Texas") %>%
    mutate(year = y) # Add year column for tracking
}))

# Extract median income for Harris County per year
harris_income_all_years <- tx_counties_income_all_years %>%
  rename(harris_median_income = estimate) %>%
  select(year, harris_median_income)

# Merge bail data with ACS income data, ensuring proper year alignment
bail_income <- bail %>%
  mutate(year = year(dispDate)) %>%  
  left_join(acs_zcta_clean, by = c("defZIP" = "ZIP", "year" = "year")) %>%
  left_join(harris_income_all_years, by = "year")

# Define income thresholds dynamically based on year
bail_income <- bail_income %>%
  mutate(
    low_income_threshold = harris_median_income * 0.5,
    moderate_income_threshold = harris_median_income * 0.8,
    high_income_threshold = harris_median_income * 1.2,
    income_category = case_when(
      is.na(median_income) ~ "Unhoused",
      median_income < low_income_threshold ~ "Low",
      median_income < moderate_income_threshold ~ "Moderate",
      median_income < high_income_threshold ~ "High",
      TRUE ~ "Very High"
    )
  )


names(bail_income)



# Create a new version of bail_income without other income details but keep income_category)
bail_income <- bail_income %>%
  select(-median_income, -harris_median_income, -low_income_threshold, 
         -moderate_income_threshold, -high_income_threshold)


# Check structure of  datasets
str(bail_income)


# Change rows with blanks in Zip code to 00000, keeping income_category intact
bail_income <- bail_income %>%
  mutate(defZIP = na_if(trimws(defZIP), ""),
         defZIP = ifelse(is.na(defZIP) & income_category == "Unhoused", "00000", defZIP))

# Check structure of the updated dataset
str(bail_income)

# Check for missing values and validate the changes
bail_income %>%
  summarise(
    total_rows = n(),
    missing_zip = sum(is.na(defZIP) | defZIP == ""),
    non_missing_zip = sum(!is.na(defZIP) & defZIP != "")
  )



# Save datasets
#save(bail_income, file = "Harris_County_Merge.RData") 
