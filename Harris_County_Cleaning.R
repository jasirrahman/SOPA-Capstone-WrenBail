#SOPA Inaugeral Data Cleaning

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

#load data
data <- read.csv("/Users/jasirrahman/Desktop/SOPA Capstone/R/Data/Weekly_Historical_Criminal_20241012.csv")

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
    courtDivInd = case_when(courtDivInd == 2 ~ "MISDEMEANOR",
                            courtDivInd == 3 ~ "FELONY",
                            TRUE ~ NA_character_),
    defRace = case_when(defRace == "A" ~ "ASIAN/PACIFIC ISLANDER",
                        defRace == "B" ~ "BLACK",
                        defRace == "I" ~ "NATIVE AMERICAN",
                        defRace == "M" ~ "MULTIRACIAL",
                        defRace == "U" ~ "UNKNOWN",
                        defRace == "W" ~ "WHITE",
                        TRUE ~ NA_character_),
    defSex = ifelse(defSex %in% c(" ", "U"), NA, defSex),
    defCitizen = ifelse(defCitizen %in% c("Y", "N"), defCitizen, NA)
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
      TRUE ~ atyConnectionLit
    )
  )

# Drop rows without 'sentence' and identify missing columns
bail <- bail %>% filter(!is.na(sentence))
bail_na <- colSums(is.na(bail)) / nrow(bail)
bail_na[bail_na > 0.3]

# Feature Engineering
# Convert columns to appropriate types
catcols <- c('courtDivInd', 'instrumentType', 'caseDisp', 'caseStatus', 'defStatus', 
             'curOffenseLit', 'curLevelDeg', 'docketType', 'nextAppearanceReason', 
             'defName', 'defRace', 'defSex', 'defCity', 'defState', 'atyName', 
             'atyConnectionLit', 'compAgency', 'offenseReportNum', 'disposition', 
             'sentence')
bail[catcols] <- lapply(bail[catcols], as.character)

intcols <- c('caseNum', 'court')
bail[intcols] <- lapply(bail[intcols], as.integer)

bail <- bail %>%
  mutate(
    bondAmt = as.numeric(bondAmt),
    fileDate = ymd(fileDate),
    defDOB = ymd(defDOB),
    dispDate = ymd(dispDate),
    defAge = as.numeric(difftime(dispDate, defDOB, units = "days")) / 365.25,
    defAgeCategory = cut(defAge, breaks = c(0, 18, 21, 25, 35, 50, 65, 100),
                         labels = c("0-18", "19-21", "22-25", "26-35", "36-50", "51-65", "66+"))
  ) %>%
  filter(!is.na(defAge))

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

# Extract sentence components
bail <- bail %>%
  mutate(
    jail_time = case_when(str_detect(sentence, "LIFE") ~ "25 YEARS",
                          str_detect(sentence, "HCJ|CONFINEMENT|TDC") ~ sentence,
                          TRUE ~ "NO JAIL TIME"),
    fine_amount = as.numeric(str_extract(sentence, "\\$\\d+")),
    probation = str_extract(sentence, "PROBATION\\s.*")
  )

# Convert jail/probation time to days
convert_to_days <- function(time_str) {
  if (is.na(time_str)) return(0)
  num <- as.numeric(str_extract(time_str, "\\d+"))
  if (str_detect(time_str, "YEARS")) return(num * 365)
  if (str_detect(time_str, "MONTHS")) return(num * 30)
  if (str_detect(time_str, "DAYS")) return(num)
  return(0)
}

bail <- bail %>%
  mutate(
    jail_days = sapply(jail_time, convert_to_days),
    probation_days = sapply(probation, convert_to_days)
  )



