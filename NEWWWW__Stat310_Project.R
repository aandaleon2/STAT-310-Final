################################################
# Data
################################################
library(readxl)
poc <- read_excel("R/STAT310/POC_All Regions.xlsx")
View(poc)

# Modify Variables
poc$ethnicity <- as.factor(poc$ethnicity)
poc$position <- as.factor(poc$position)
poc$region <- as.factor(poc$region)
poc$ethnicity <- relevel(poc$ethnicity, "White or European American")
poc$position <- relevel(poc$position, "chorus")
poc$region <- relevel(poc$region, "central")

# Subset Data into Two Data Sets
library(tidyverse)
poc <- poc %>% group_by(region)
poc1 <- poc %>% select(1:8) #Group 1: 2013-2015
poc2 <- poc %>% select(1:3, 9:13) #Group 2: 2016-2019

View(poc1); View(poc2)

# Add Living Wage Variable - - - - - (Binary Response Variable)
# Add Race Variable

## Group 1: 2013-2015
poc1 <- poc1 %>% mutate(living_wage = case_when(
  is.na(averagecontract_salary...8) ~ "missing",
  averagecontract_salary...8 >= 816 & region == 'central' ~ "yes",
  averagecontract_salary...8 >= 936 & region == 'western' ~ "yes",
  averagecontract_salary...8 >= 889 & region == 'eastern' ~ "yes",
  TRUE ~ "no")); poc1$living_wage <- factor(poc1$living_wage)

poc1 <- poc1 %>% mutate(y1 = case_when(living_wage == 'yes' ~ 1,
                                      living_wage == 'no' ~ 0))
poc1$y1 <- factor(poc1$y1)

poc1 <- poc1 %>% mutate(race = case_when(
  ethnicity == 'White or European American' ~ 1, TRUE ~ 0,)) 
poc1$race <- factor(poc1$race)

poc1 <- poc1 %>% relocate(averagecontract_salary...8, .before = contract_2013_15)
poc1 <- poc1 %>% relocate(living_wage, .after = averagecontract_salary...8)
poc1 <- poc1 %>% relocate(y1, .after = living_wage)
poc1 <- poc1 %>% relocate(race, .after = ethnicity)
View(poc1)

## Group 2: 2016-2019
poc2 <- poc2 %>% mutate(living_wage = case_when(
  is.na(averagecontract_salary...13) ~ "missing",
  averagecontract_salary...13 >= 816 & region == 'central' ~ "yes",
  averagecontract_salary...13 >= 936 & region == 'western' ~ "yes",
  averagecontract_salary...13 >= 889 & region == 'eastern' ~ "yes",
  TRUE ~ "no")); poc2$living_wage <- factor(poc2$living_wage)

poc2 <- poc2 %>% mutate(y2 = case_when(living_wage == 'yes' ~ 1,
                                      living_wage == 'no' ~ 0))
poc2$y2 <- factor(poc2$y2)

poc2 <- poc2 %>% mutate(race = case_when(
  ethnicity == 'White or European American' ~ 1, TRUE ~ 0,))
poc2$race <- factor(poc2$race)

poc2 <- poc2 %>% relocate(averagecontract_salary...13, .before = contract_2016_19)
poc2 <- poc2 %>% relocate(living_wage, .after = averagecontract_salary...13)
poc2 <- poc2 %>% relocate(y2, .after = living_wage)
poc2 <- poc2 %>% relocate(race, .after = ethnicity)
View(poc2)

################################################
# Models
################################################

# Model 1: Group 1 2013-2015
library(car)
model1 <- glm(y1 ~ position + region + ethnicity + contract_2013_15, 
              family = binomial(link = "logit"), data = poc1)

# Model 2: Group 2 2016-2019
model2 <- glm(y2 ~ position + region + ethnicity + contract_2016_19, 
              family = binomial(link = "logit"), data = poc2)

################################################
# Testing / Analysis
################################################

# Q1
Anova(model1)
Anova(model2)

# Q2
model11 <- glm(y1 ~ position + region + race + contract_2013_15, 
               family = binomial(link = "logit"), data = poc1)

model22 <- glm(y2 ~ position + region + race + contract_2016_19, 
               family = binomial(link = "logit"), data = poc2)
summary(model11)
summary(model22)
Anova(model11)
Anova(model22)
