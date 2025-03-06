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

## Load data
# Note: When running make sure that path for file is specific to your computer
load("/Users/jasirrahman/Desktop/SOPA Capstone/R/Data/Harris_County_Cleaned_v2.RData")

# Download shape file here: https://www2.census.gov/geo/tiger/TIGER2023/FACES/tl_2023_48201_faces.zip
# Set path to the extracted shapefile
shapefile_path <- "/Users/jasirrahman/Desktop/SOPA Capstone/R/Visualization/tl_2023_48201_faces/tl_2023_48201_faces.shp"

##### Data Preparation #####

# Read shapefile using sf
harris_faces <- st_read(shapefile_path)

# Print basic info
print(harris_faces)

# Aggregate census blocks into ZIP code areas
harris_zip <- harris_faces %>%
  group_by(ZCTA5CE20) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Check if ZIP codes plot correctly
# Plot the ZIP code areas
ggplot() +
  geom_sf(data = harris_zip, fill = NA, color = "blue") +
  theme_void() +
  ggtitle("ZIP Code Tabulation Areas in Harris County")

###### Plot ZIP Code counts ######

# Create new df, group the bail data by ZIP code
bailZIPCounts <- bail %>% 
  group_by(defZIP) %>% 
  summarize(count = n())    # Aggregate counts

# Rename collumns for merging
bailZIPCounts <- rename(bailZIPCounts, ZCTA5CE20 = defZIP)

# Merge data for heatmap plot
harris_zip_final <- inner_join(harris_zip, bailZIPCounts, by = join_by(ZCTA5CE20))

# Plot Arrest Heatmap
ggplot() +
  geom_sf(data = harris_zip_final, aes(fill = count), color = "white", alpha = 0.8) +
  (scale_fill_viridis_c(option = "plasma", direction = -1)) + 
  theme_void() +
  ggtitle("Heatmap of Arrests by ZIP Code in Harris County") +
  labs(fill = "Observation Count")

###### Plot ZIP Code Bail Amount Data ######
# Create new df, group the bail data by ZIP code
bailZIPAmounts <- bail %>% 
  group_by(defZIP) %>% 
  summarize(bond = mean(bondAmt))   # Aggregate counts

# Rename collumns for merging
bailZIPAmounts <- rename(bailZIPAmounts, ZCTA5CE20 = defZIP)

# Merge data for heatmap plot
harris_zip_bail_final <- inner_join(harris_zip, bailZIPAmounts, by = join_by(ZCTA5CE20))

# Plot Arrest Heatmap
ggplot() +
  geom_sf(data = harris_zip_bail_final, aes(fill = bond), color = "white", alpha = 0.8) +
  (scale_fill_viridis_c(option = "plasma", direction = -1)) + 
  theme_void() +
  ggtitle("Mean Bail Amounts are Higher \nin South and Northeast Houston") +
  labs(fill = "Mean Bond Amount")
