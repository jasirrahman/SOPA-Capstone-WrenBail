# clear environment
rm(list = ls())

# set and get wd
setwd("/Users/jasirrahman/Desktop/SOPA Capstone/R/Code")
getwd()

# load datasets
load("/Users/jasirrahman/Desktop/SOPA Capstone/R/Data/Harris_County_Merge_v2.RData")

# load packages
library(tidyr)
library(dplyr)
library(broom)
library(stargazer)

# set reference level
bail_income$income_category <- factor(bail_income$income_category)
levels(bail_income$income_category)
bail_income$income_category <- relevel(bail_income$income_category, ref = "Moderate")

# changes "NA" strings to NAs
bail_income$atyConnectionLit <- replace(bail_income$atyConnectionLit, bail_income$atyConnectionLit == "NA", NA)

# linear regression (base findings off of this one)
model1 <- lm(bondAmt ~ income_category*courtDivInd + defRace + defSex + atyConnectionLit + priorOffenseNum + chargeDegree, data = bail_income)
print(summary(model1))
stargazer(model1, type='html', digits = 2, title = 'Model of Bail Amounts by Income, Selected Covariates', 
          covariate.labels = c('Income Category: High', 'Income Category: Low', 'Unhoused', 'Income Category: Very High',
                               'Charge Type: Misdemeanor', 'Defendant Race: Black', 'Defendant Race: Native American', 
                               'Defendant Race: Unkown', 'Defendant Race: White', 'Defendant Sex: Male', 'Attorney Connection: Hired',
                               'Attorney Connection: Temporary Counsel', 'Prior Offense Number', 'Charge Degree: B', 'Charge Degree: C',
                               'Charge Degree: S', 'Interaction: High Income Misdemeanant', 'Interaction: Low Income Misdemeanant', 
                               'Interaction: Unhoused Misdemeanant', 'Interaction: Very High Income Misdemeanant'),
          dep.var.labels = 'Bail Amount',
          style = 'qje', out = 'Lm_1.html')

# create different datasets (Jasir's Code)
preReform <- bail_income %>% 
  filter(fileDate <= '2019-01-01')
postReform <- bail_income %>% 
  filter(fileDate > '2019-01-01')

# create more datasets (Sowmya's Code)
premis <- subset(preReform, courtDivInd == "MISDEMEANOR")
prefel <- subset(preReform, courtDivInd == "FELONY")
postmis <- subset(postReform, courtDivInd == "MISDEMEANOR")
postfel <- subset(postReform, courtDivInd == "FELONY")

# more regressions
model2 <- lm(bondAmt ~ income_category, data = premis)
summary(model2)
model3 <- lm(bondAmt ~ income_category, data = prefel)
summary(model3)
model4 <- lm(bondAmt ~ income_category, data = postmis)
summary(model4)
model5 <- lm(bondAmt ~ income_category, data = postfel)
summary(model5)

# other regressions
model6 <- lm(bondAmt ~ income_category*courtDivInd, data = preReform)
summary(model6)
model7 <- lm(bondAmt ~ income_category*courtDivInd, data = postReform)
summary(model7)

# Conner's Regressions (what i used to write my blurb)
bail_income$high_income <- ifelse(bail_income$income_category == "High" | 
                                    bail_income$income_category == "Very High", 1, 0 )
model_check <- lm(bondAmt ~ high_income, data = bail_income)
summary(model_check)

bail_income$postodonnell <- ifelse(bail_income$fileDate <= '2019-01-01', 1, 0 )
model_check <- lm(bondAmt ~ high_income*postodonnell, data = bail_income)
summary(model_check)

bail_income$low_income <- ifelse(bail_income$income_category == "Low" | 
                                    bail_income$income_category == "Unhoused", 1, 0 )
model_check <- lm(bondAmt ~ low_income, data = bail_income)
summary(model_check)

# Group Means
premis_mean <- premis %>%
  group_by(income_category) %>%
  summarize(mean_value = mean(bondAmt, na.rm = TRUE))
print(premis_mean)

prefel_mean <- prefel %>%
  group_by(income_category) %>%
  summarize(mean_value = mean(bondAmt, na.rm = TRUE))
print(prefel_mean)

postmis_mean <- postmis %>%
  group_by(income_category) %>%
  summarize(mean_value = mean(bondAmt, na.rm = TRUE))
print(postmis_mean)

postfel_mean <- postfel %>%
  group_by(income_category) %>%
  summarize(mean_value = mean(bondAmt, na.rm = TRUE))
print(postfel_mean)
