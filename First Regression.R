# clear environment
rm(list = ls())

# set and get wd
setwd("C:/Users/viswa/Downloads")
getwd()

# load datasets
load("Bail Income.RData")

# set reference level
bail_income$income_category <- factor(bail_income$income_category)
levels(bail_income$income_category)
bail_income$income_category <- relevel(bail_income$income_category, ref = "Moderate")

# not sure what this code exactly does
bail_income$atyConnectionLit <- replace(bail_income$atyConnectionLit, bail_income$atyConnectionLit == "NA", NA)

# simple bivariate linear regression with interaction
model1 <- lm(bondAmt ~ income_category*courtDivInd, data = bail_income)
summary(model1)

# load packages
library(tidyr)
library(dplyr)
library(broom)

# create different datasets (Jasir's Code)
preReform <- bail_income %>% 
  filter(fileDate <= '2019-01-01')
postReform <- bail_income %>% 
  filter(fileDate > '2019-01-01')
misdemeanors <- bail_income %>% 
  filter(chargeLevel == 'M')
felonies <- bail_income %>% 
  filter(chargeLevel == 'F')
preReform %>% 
  group_by(chargeLevel, income_category) %>% 
  summarize(mean = mean(bondAmt))
postReform %>% 
  group_by(chargeLevel, income_category) %>% 
  summarize(mean = mean(bondAmt))

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