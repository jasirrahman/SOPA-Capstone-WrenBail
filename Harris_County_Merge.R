rm(list=ls())
getwd()

setwd("/Users/karlavasquez/Documents/sopa 400")

load("Harris_County_Cleaned.RData")

ls()

names(bail) 


#install.packages("wru", dependencies = TRUE, type = "binary")
#install.packages("tidycensus", dependencies = TRUE, type = "binary")

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



###1
# Set Census API key 
#census_api_key("d7f7cd3ab0204387684215b28c87070649056422", install = TRUE)

readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")




### 1.  ACS Income Data by ZIP Code Tabulation Area (ZCTA)
acs_zcta <- get_acs(
  geography = "zcta", 
  variables = c(medincome = "B19013_001"),
  year = 2021
)


# Cleaning ACS data by renaming GEOID to 'zip code' and estimate to 'median income'
acs_zcta_clean <- acs_zcta %>%
  rename(ZIP = GEOID, median_income = estimate) %>%
  select(ZIP, median_income)

# ACS County-Level Income Data for Texas counties and ifltering for Harris County
tx_counties_income <- get_acs(
  geography = "county", 
  variables = c(medincome = "B19013_001"),
  state = "TX",
  year = 2021
)

# Filter for Harris County ")
harris_income <- tx_counties_income %>%
  filter(NAME == "Harris County, Texas")

# Extract the median income value
harris_median_income <- harris_income$estimate[1]

# Ensure ZIP codes are character strings
bail <- bail %>%
  mutate(defZIP = as.character(defZIP))

acs_zcta_clean <- acs_zcta_clean %>%
  mutate(ZIP = as.character(ZIP))




#####. #Merge
# Merge datasets on ZIP code
bail_income <- bail %>%
  left_join(acs_zcta_clean, by = c("defZIP" = "ZIP"))




#####income

#  income thresholds based on Harris County median income
low_income_threshold <- harris_median_income * 0.5
moderate_income_threshold <- harris_median_income * 0.8
high_income_threshold <- harris_median_income * 1.2

#  income levels
bail_income <- bail_income %>%
  mutate(income_category = case_when(
    is.na(median_income) ~ "Unhoused",
    median_income < low_income_threshold ~ "Low",
    median_income < moderate_income_threshold ~ "Moderate",
    median_income < high_income_threshold ~ "High",
    TRUE ~ "Very High"
  ))

names(bail_income)

### changed rows with blanks on Zip code to 00000

library(dplyr)  

bail_income <- bail_income %>%
  mutate(defZIP = na_if(trimws(defZIP), ""),  
         defZIP = ifelse(is.na(defZIP) & income_category == "Unhoused", "00000", defZIP))


#checking for missing values

bail_income %>%
  summarise(
    total_rows = n(),
    missing_zip = sum(is.na(defZIP) | defZIP == ""),
    non_missing_zip = sum(!is.na(defZIP) & defZIP != "")
  )





