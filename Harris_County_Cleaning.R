#SOPA Inaugural Data Cleaning

##preliminary environment
#clear environment
rm(list=ls())
#set working directory
setwd("/Users/jasirrahman/Desktop/SOPA Capstone/R/Code")
#load libraries
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
library(lubridate)

# Load data
# Note: Requires .txt file of data
# Note: When running make sure that path for file is specific to your computer
data <- read.delim("/Users/jasirrahman/Desktop/SOPA Capstone/R/Data/Weekly_Historical_Criminal_20241102.txt")
# Subset "bail" as dataset where all observations take place between 2014-2024
bail <- subset(data, fda > 20140000)

#################### Converted Code ####################

# Data Cleaning
# Rename columns
colnames(bail) <- c('courtDivInd', 'caseNum', 'fileDate', 'instrumentType',
                    'caseDisp', 'court', 'caseStatus', 'defStatus', 'bondAmt',
                    'curOffense', 'curOffenseLit', 'curLevelDeg', 'comOffense',
                    'comOffenseLit', 'comLevelDeg', 'gjOffense', 'gjOffenseLit',
                    'gjLevelDeg', 'nextAppearanceDate', 'docketType',
                    'nextAppearanceReason', 'defName', 'defSPN', 'defRace', 'defSex',
                    'defDOB', 'defStNum', 'defStName', 'defAptNum', 'defCity',
                    'defState', 'defZIP', 'atyName', 'atySPN', 'atyConnectionCode',
                    'atyConnectionLit', 'compName', 'compAgency',
                    'offenseReportNum', 'dispDate', 'disposition', 'caseNumDivInd',
                    'sentence', 'defCitizen', 'bondException', 'gjDate',
                    'gjCourt', 'gjCDP', 'defBirthState')

# Drop irrelevant columns
bail <- bail %>%
  select(-c(curOffense, comOffense, comOffenseLit, comLevelDeg, gjOffense, 
            gjOffenseLit, gjLevelDeg, nextAppearanceDate, defStNum, defStName, 
            defAptNum, caseNumDivInd, atySPN, atyConnectionCode, compName, 
            gjDate, gjCourt, gjCDP))

# Recode key columns
bail <- bail %>%
  mutate(
    # Changing courtDivInd variable to be string of type of crime
    courtDivInd = case_when(courtDivInd == "002" ~ "MISDEMEANOR",
                            courtDivInd == "003" ~ "FELONY"),
    # Clarifying defRace classifications
    defRace = case_when(defRace == "A" ~ "ASIAN/PACIFIC ISLANDER",
                        defRace == "B" ~ "BLACK",
                        defRace == "I" ~ "NATIVE AMERICAN",
                        defRace == "M" ~ "MULTIRACIAL",
                        defRace == "U" ~ "UNKNOWN",
                        defRace == "W" ~ "WHITE",
                        TRUE ~ NA_character_),
    # Clarify defSex variable (assign all "unknown"/unrecorded cases to "NA")
    defSex = ifelse(defSex %in% c(" ", "","U"), NA, defSex),
    # Apply "U" to values that are not "Y"/"N"
    defCitizen = ifelse(defCitizen %in% c("Y", "N"), defCitizen, "U")
  )

# Remove duplicates for same case/defendant
bail <- bail %>%
  distinct(caseNum, defSPN, .keep_all = TRUE)

# Simplify attorney connection variable
bail <- bail %>%
  mutate(
    atyConnectionLit = coalesce(atyConnectionLit, ""),
    atyConnectionLit = case_when(
      str_detect(atyConnectionLit, "APPOINTED|PUBLIC|FOR DEFENDANT") ~ "Appointed",
      str_detect(atyConnectionLit, "HIRED") ~ "Hired",
      str_detect(atyConnectionLit, "TEMPORARY") ~ "Temporary",
      TRUE ~ atyConnectionLit,
      )
  )

# Consolidate the "NA" Attorney 
bail$atyConnectionLit = ifelse(bail$atyConnectionLit %in% c("Hired", "Appointed", "Temporary"), bail$atyConnectionLit, "NA")

# Drop rows without 'sentence' and identify missing columns
bail <- bail %>% filter(!is.na(sentence))
bail_na <- colSums(is.na(bail)) / nrow(bail)
bail_na[bail_na > 0.3]

### Feature Engineering
## Convert columns to appropriate types
# assign categorical columns
catcols <- c('courtDivInd', 'instrumentType', 'caseDisp', 'caseStatus', 'defStatus', 
             'curOffenseLit', 'curLevelDeg', 'docketType', 'nextAppearanceReason', 
             'defName', 'defRace', 'defSex', 'defCity', 'defState', 'atyName', 
             'atyConnectionLit', 'compAgency', 'offenseReportNum', 'disposition', 
             'sentence')
# convert to characters
bail[catcols] <- lapply(bail[catcols], as.character)
# assign court as integer
intcols <- c('court')
bail[intcols] <- lapply(bail[intcols], as.integer)

# more transformations
bail <- bail %>%
  mutate(
    bondAmt = as.numeric(bondAmt),    # make bondAmt numeric
    fileDate = ymd(fileDate),    #convert fileDate to Date format
    defDOB = ymd(defDOB),    #convert defDOB to Date format
    dispDate = ymd(dispDate),    #convert dispDate to Date format
    defAge = as.numeric(difftime(dispDate, defDOB, units = "days")) / 365.25,    #calculate defAge from Dates
    defAgeCategory = cut(defAge, breaks = c(0, 18, 21, 25, 35, 50, 65, 100),    #assign defAgeCategory based on defAge
                         labels = c("0-18", "19-21", "22-25", "26-35", "36-50", "51-65", "66+"))
  ) %>%
  filter(!is.na(defAge))    #filter out those without defAge

# round defAge down so not in decimal format
bail$defAge <- floor(bail$defAge)

# Create new charge degree variable
bail <- bail %>%
  mutate(
    curDegree = str_extract(curLevelDeg, "\\b[A-Z]+$"),
    pastOffenseNum = row_number(),
    guiltyPlea = disposition %in% c('CONVICTION-PLEA OF GUILTY', 'GUILTY PLEA - JURY VERDICT', 
                                    'PROBATION-PLEA OF GUILTY', 'DEFERRED ADJUD OF GUILT'),
    bondDenied = bondException == "BOND DENIED"
  ) %>%
  select(-curLevelDeg, -defSPN)

# Create "defInState" and "defInCity" variables
bail <- bail %>%
  mutate(
    defInState = defState == "TX",
    defInCity = defCity == "HOUSTON"
  )

# Split defendant name into components
bail <- bail %>%
  separate(defName, into = c("lastName", "firstMiddle"), sep = ", ") %>%
  separate(firstMiddle, into = c("firstName", "middleName"), extra = "merge", fill = "right")



