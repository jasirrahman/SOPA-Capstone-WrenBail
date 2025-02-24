#SOPA Inaugural Data Cleaning

################### Setup ################### 
#clear environment
rm(list=ls())
#set working directory
setwd("/Users/jasirrahman/Desktop/SOPA Capstone/R/Code")
#load libraries
library(tidyr)
library(zoo)
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)

# Load data
# Note: Requires .txt file of data
# Note: When running make sure that path for file is specific to your computer
data <- read.delim("/Users/jasirrahman/Desktop/SOPA Capstone/R/Data/Weekly_Historical_Criminal_20241102.txt")

##### Data Cleaning 1 #####
## Temporal Filter
## initial filter of data from 2007-2024
## due to 'seven year' rule of criminal background checks
## allows for counting number of prior offenses committed by offenders in seven years before study period
bail <- subset(data, fda > 20070000)

## First, for ease of coding, change column names such that they are more comprehensible, easier to work with
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

# Then, for ease of computation, drop irrelevant columns
bail <- bail %>%
  select(-c(instrumentType, caseDisp, caseStatus, curOffense, comOffense, comOffenseLit,
            gjOffense, gjOffenseLit, gjLevelDeg, nextAppearanceDate, docketType, nextAppearanceReason, 
            defStNum, defStName, defAptNum, caseNumDivInd, atySPN, atyConnectionCode, compName, 
            offenseReportNum, gjDate, gjCourt, gjCDP))

# Remove duplicates for same case/defendant
bail <- bail %>%
  distinct(caseNum, defSPN, .keep_all = TRUE)

# Drop duplicate rows
bail <- bail %>% filter(!is.na(sentence))

# Check bondException
print(unique(bail$bondException))
# Check what bondAmt rows show for bondException "REFER TO MAGISTRATE"
bail_magistrate <- bail %>% 
  filter(bondException == "REFER TO MAGISTRATE")
# Two observations, don't need to filter out

# Check other exceptions to GENERAL ORDER BAIL or BOND DENIED
exceptions <- c("19900601","20110714","20150817")
bail_exceptions <- bail %>% 
  filter(bondException == "19900601" | bondException == "20110714" | 
           bondException == "20150817") #just three exceptions - no need to alter/remove

# Bond denials by virtue of crime indicated as either "999999" or "00000*" or "000000"

# Check Unsecured GOB Eligible Cases
gob <- bail %>% 
  filter(bondException == "UNSECURED GOB ELIGIBLE") # 44,777 indicated as eligible for Unsecured GOB
# No case of Unsecured GOB Eligibility has value of 0; uncertain what that means - Jay has been reached out to!

bail <- bail %>% 
  filter(bondException != "BOND DENIED")  # 16,535 had "BOND DENIED"
## NOTE: Would be interesting to explore distribution of who had "BOND DENIED"

# Drop bondAmt not formatted is five integers in a row -- will handle removing those who had BOND DENIED with code of "00000*"
bail <- bail %>% 
  filter(str_detect(bondAmt, "[0-9][0-9][0-9][0-9][0-9][0-9]")) %>% # filters out 15,949 rows without numeric bond data
  filter(bondAmt < 99999) # filters out (337) rows where bond amount set to 99999 - which is an indicator of bond denial

## Create past offense number variable
# Use defSPN to find priorOffenseNum for 7 years prior to the study period
bail <- bail %>% 
  add_count(defSPN) %>% 
  rename(priorOffenseNum = n)

## Subset data to only include study period (2014-2024)
bail <- subset(bail, fileDate > 20140000 & fileDate <= 20240000)

################### Data Cleaning 2 ###################

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

################### Row Cleaning ###################

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

# Identify missing columns
bail_na <- colSums(is.na(bail)) / nrow(bail)
bail_na[bail_na > 0.3]

### Feature Engineering
## Convert columns to appropriate types
# assign categorical columns
catcols <- c('courtDivInd', 'defStatus', 'curOffenseLit', 'curLevelDeg', 'comLevelDeg',
             'defName', 'defRace', 'defSex', 'defCity', 'defState', 'atyName', 
             'atyConnectionLit', 'compAgency', 'disposition', 'sentence', 'defCitizen',
             'bondException', 'defBirthState')

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
                         labels = c("18", "19-21", "22-25", "26-35", "36-50", "51-65", "66+"))
  ) %>%
  filter(!is.na(defAge))    #filter out those without defAge

# round defAge down so not in decimal format
bail$defAge <- floor(bail$defAge)

# filter out observations where defAge < 18
bail <- bail %>% 
  filter(defAge > 18)

# Create new charge degree variable
bail <- bail %>%
  mutate(
    chargeLevel = str_extract(curLevelDeg, "^[A-Z]"),
    chargeDegree = str_extract(curLevelDeg, "\\b[A-Z]+$"),
    guiltyPlea = disposition %in% c('CONVICTION-PLEA OF GUILTY', 'GUILTY PLEA - JURY VERDICT', 
                                    'PROBATION-PLEA OF GUILTY', 'DEFERRED ADJUD OF GUILT'),
    bondDenied = bondException == "BOND DENIED"
  ) %>%
  select(-curLevelDeg)

# Create "defInState" and "defInCity" variables
bail <- bail %>%
  mutate(
    defInState = defState == "TX",
    defInCity = defCity == "HOUSTON"
  )


# Split defendant name into components for wru surname analysis
bail <- bail %>%
  separate(defName, into = c("lastName", "firstMiddle"), sep = ", ") %>%
  separate(firstMiddle, into = c("firstName", "middleName"), extra = "merge", fill = "right")

################### Save as .RData file ################### 
save(bail, file = "/Users/jasirrahman/Desktop/SOPA Capstone/R/Data/Harris_County_Cleaned_v2.RData")
