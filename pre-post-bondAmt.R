#Pre-Post Bond Amount Line Graphs
rm(list=ls())
set.seed(18552)

setwd("/Users/jasirrahman/Desktop/SOPA Capstone/R/Code")

library(tidyr)
library(zoo)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
library(sp)
library(cowplot)
library(ggpubr)
library(maps)
library(tidyverse)
library(margins)
library(dplyr)

###### Load and prepare data ######
# Load bail data
load("/Users/jasirrahman/Desktop/SOPA Capstone/R/Data/Harris_County_Mergev2.RData")

# Add pre/post column to bail dataset
bail_income <- bail_income %>% 
  mutate(postOdonnell = ifelse(fileDate > '2019-01-01', TRUE, FALSE))

bail_income <- bail_income %>% 
  mutate(dispYear = (case_when(fileDate < '2015-01-01' ~ 2014,
                               fileDate >= '2015-01-01' & fileDate < '2016-01-01' ~ 2015,
                               fileDate >= '2016-01-01' & fileDate < '2017-01-01' ~ 2016,
                               fileDate >= '2017-01-01' & fileDate < '2018-01-01' ~ 2017,
                               fileDate >= '2018-01-01' & fileDate < '2019-01-01' ~ 2018,
                               fileDate >= '2019-01-01' & fileDate < '2020-01-01' ~ 2019,
                               fileDate >= '2020-01-01' & fileDate < '2021-01-01' ~ 2020,
                               fileDate >= '2021-01-01' & fileDate < '2022-01-01' ~ 2021,
                               fileDate >= '2022-01-01' & fileDate < '2023-01-01' ~ 2022,
                               fileDate >= '2023-01-01' & fileDate < '2024-01-01' ~ 2023,
  )))

#Create pre/post datasets
preReform <- bail_income %>% 
  filter(fileDate <= '2019-01-01') 
# Note: Pre reform period (2014-2019) had 278,258 rows
postReform <- bail_income %>% 
  filter(fileDate > '2019-01-01')
# Note: Post reform period (2019-2024) had 167,735 rows

#Create misdemeanor dataset
misdemeanors <- bail_income %>% 
  filter(courtDivInd == 'MISDEMEANOR')

#Create felonies dataset
felonies <- bail_income %>% 
  filter(courtDivInd == 'FELONY')

# Get mean bail amount for felonies v misdemeanor for pre/post
# Pre
preReform %>% 
  group_by(courtDivInd, income_category) %>% 
  summarize(mean = mean(bondAmt))
# Post
postReform %>% 
  group_by(courtDivInd, income_category) %>% 
  summarize(mean = mean(bondAmt))

# Get counts of felonies v misdemeanor for pre/post
# Pre
preReform %>% 
  group_by(courtDivInd, ) %>% 
  summarize(count = n())
# Post
postReform %>% 
  group_by(courtDivInd) %>% 
  summarize(count = n())

# Note: Proportionally similar rate of felony and misdemeanor dispositions

# Plotting rudimentary pre-post
ggplot(data = misdemeanors, mapping = aes(y=bondAmt, x=as.numeric(postOdonnell), fill = as.factor(income_category)))+
  geom_smooth(method = lm)+
  ggtitle("Misdemeanor Bail Amounts Decreased Following the O'Donnell Reforms")+
  labs(y = "Bail Amount ($)", x = "Pre/Post O'Donnell")+
  scale_x_discrete(limits = c(0,1), labels=c("Pre O'Donnell", "Post O'Donnell"))+ 
  theme(plot.title = element_text(hjust = 0))

#summarize bond amount by year
mean_misd_bond_by_year <- misdemeanors %>%
  group_by(dispYear, income_category) %>%
  summarize(mean_bondAmt = mean(bondAmt, na.rm = TRUE))

mean_fel_bond_by_year <- felonies %>%
  group_by(dispYear, income_category) %>%
  summarize(mean_bondAmt = mean(bondAmt, na.rm = TRUE))


# Step 2: Plot Misdemeanors
ggplot(mean_misd_bond_by_year, aes(x = dispYear, y = mean_bondAmt, color = as.factor(income_category))) +
  geom_line(size = 1.2) +
  geom_point() +
  ggtitle("Mean Misdemeanor Bail Amounts Decreased Overall, \n Disparities Generally Remained") +
  labs(y = "Mean Bail Amount ($)", x = "Year", color = "Income Category") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "blue", size = 1)

# Step 3: Plot Felonies
ggplot(mean_fel_bond_by_year, aes(x = dispYear, y = mean_bondAmt, color = as.factor(income_category))) +
  geom_line(size = 1.2) +
  geom_point() +
  ggtitle("Mean Felony Bail Amounts \n Are Highest for Unhoused Individuals") +
  labs(y = "Mean Bail Amount ($)", x = "Year", color = "Income Category") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 2019, linetype = "dashed", color = "blue", size = 1)

##### Median Bail Amounts?#####

#summarize bond amount by year
med_misd_bond_by_year <- misdemeanors %>%
  group_by(dispYear, income_category) %>%
  summarize(med_bondAmt = median(bondAmt, na.rm = TRUE))

med_fel_bond_by_year <- felonies %>%
  group_by(dispYear, income_category) %>%
  summarize(med_bondAmt = median(bondAmt, na.rm = TRUE))

# Plot median bond amounts
ggplot(med_misd_bond_by_year, aes(x = dispYear, y = med_bondAmt, color = as.factor(income_category))) +
  geom_line(size = 1.2) +
  geom_point() +
  ggtitle("Median Misdemeanor Bail Amount Decreased Most Dramatically for the \n Lowest Income Individuals Post O'Donnell") +
  labs(y = "Median Bail Amount ($)", x = "Year", color = "Income Category") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "blue", size = 1)

# Plot median bond amounts
ggplot(med_fel_bond_by_year, aes(x = dispYear, y = med_bondAmt, color = as.factor(income_category))) +
  geom_line(size = 1.2) +
  geom_point() +
  ggtitle("Mean Misdemeanor Bail Amounts Decreased Overall, \n Disparities Generally Remained") +
  labs(y = "Mean Bail Amount ($)", x = "Year", color = "Income Category") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "blue", size = 1)