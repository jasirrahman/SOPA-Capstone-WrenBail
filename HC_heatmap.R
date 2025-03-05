# Plot Harris County Heat Map
################### Setup ################### 
#clear environment
rm(list=ls())
#set working directory
setwd("/Users/jasirrahman/Desktop/SOPA Capstone/R/Code")
# Load libraries
library(tidycensus)
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
library(ggplot2)
# Load data
# Note: When running make sure that path for file is specific to your computer
load("/Users/jasirrahman/Desktop/SOPA Capstone/R/Data/Harris_County_Cleaned_v2.RData")

# Set path to the extracted shapefile
shapefile_path <- "/Users/jasirrahman/Desktop/SOPA Capstone/R/Visualization/tl_2023_48201_faces/tl_2023_48201_faces.shp"

# Read shapefile using sf
harris_faces <- st_read(shapefile_path)

# Print basic info
print(harris_faces)

ggplot() +
  geom_sf(data = harris_faces, fill = "lightgray", color = "black", size = 0.3) +
  theme_void() +
  labs(title = "Harris County Faces (Census Blocks)")

options(tigris_use_cache = TRUE)

# Use the correct FIPS code for Harris County, TX (48201)
harris_zips_tigris <- zctas(state = "48", county = "201", year = 2010)

# Print summary
print(harris_zips_tigris)

# Plot roads
ggplot() +
  geom_sf(data = harris_zips_tigris, color = "red", size = 0.2) +
  theme_minimal() +
  labs(title = "Harris County ZIP Codes (TIGER Data)")

#### Chat

# Get Texas ZCTAs for 2010 (the latest year available for ZCTAs in tigris)
texas_zctas <- zctas(state = "TX", year = 2010)

# Print the structure of the dataset
print(texas_zctas)

harris_boundary <- counties(state = "TX", year = 2023) %>%
  filter(NAME == "Harris")

harris_zips <- st_intersection(texas_zctas, harris_boundary)

# Print summary
print(harris_zips)

ggplot() +
  geom_sf(data = harris_zips, fill = "lightblue", color = "black", size = 0.3) +
  theme_minimal() +
  labs(title = "Harris County ZIP Code Tabulation Areas (ZCTA5)",
       subtitle = "Filtered from Texas ZCTAs (2010 TIGER Data)",
       caption = "Source: U.S. Census Bureau via tigris")
