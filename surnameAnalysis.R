## Surname Analysis

##preliminary environment
#clear environment
rm(list=ls())
#set working directory
setwd("/Users/jasirrahman/Desktop/SOPA Capstone/R/Code")
#load libraries
library(tidyr)
library(zoo)
library(sp)
library(ggpubr)
library(tidyverse)
library(lubridate)
library(wru)
library(tidycensus)

# Load data
#INSERT DATA#

# Census API: d7f7cd3ab0204387684215b28c87070649056422
census_api_key("d7f7cd3ab0204387684215b28c87070649056422", install = TRUE)

##TO-DO
# Surname analysis
# 1) download census data
# use tidycensus package for this, documentation: https://walker-data.com/tidycensus/articles/basic-usage.html
tx <- get_acs(geography = "zcta", 
              variables = c(medincome = "B19013_001"),
              year = 2021,
              county = NULL)

# 2) Conduct surname analysis
# use wru package for this, documentation: https://cran.r-project.org/web/packages/wru/readme/README.html
predict_race(voter.file = bail, surname.only = TRUE)



