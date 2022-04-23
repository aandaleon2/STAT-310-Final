# Data
library(readxl)
poc <- read_excel("R/STAT310/POC_All Regions.xlsx")
View(poc)

# Modify Variables
poc$ethnicity <- as.factor(poc$ethnicity)
poc$position <- as.factor(poc$position)
poc$region <- as.factor(poc$region)

# Subset Data into Two Datasets
library(tidyverse)
poc0 <- poc %>% group_by(region)
poc1 <- poc %>% select(1:8) #Group 1: 2013-2015
poc2 <- poc %>% select(1:3, 9:13) #Group 2: 2016-2019
View(poc1); View(poc2)

# Add Living Wage Variable - - - - - (Binary Response Variable)
## Group 1: 2013-2015
poc1 <- poc1 %>% mutate(living_wage = case_when(
  is.na(averagecontract_salary...8) ~ "missing",
  averagecontract_salary...8 >= 816 & region == 'central' ~ "yes",
  averagecontract_salary...8 >= 936 & region == 'western' ~ "yes",
  averagecontract_salary...8 >= 889 & region == 'eastern' ~ "yes",
  TRUE ~ "no")) #true = if else statement

poc1 <- poc1 %>% mutate(y = case_when(living_wage == 'yes' ~ 1,
                                          living_wage == 'no' ~ 0))
poc1 <- poc1 %>% relocate(averagecontract_salary...8, .before = contract_2013_15)
poc1 <- poc1 %>% relocate(living_wage, .after = averagecontract_salary...8)
poc1 <- poc1 %>% relocate(y, .after = living_wage)
View(poc1)

## Group 2: 2016-2019
poc2 <- poc2 %>% mutate(living_wage = case_when(
  is.na(averagecontract_salary...13) ~ "missing",
  averagecontract_salary...13 >= 816 & region == 'central' ~ "yes",
  averagecontract_salary...13 >= 936 & region == 'western' ~ "yes",
  averagecontract_salary...13 >= 889 & region == 'eastern' ~ "yes",
  TRUE ~ "no"))

poc2 <- poc2 %>% mutate(y = case_when(living_wage == 'yes' ~ 1,
                                          living_wage == 'no' ~ 0))
poc2 <- poc2 %>% relocate(averagecontract_salary...13, .before = contract_2016_19)
poc2 <- poc2 %>% relocate(living_wage, .after = averagecontract_salary...13)
poc2 <- poc2 %>% relocate(y, .after = living_wage)
View(poc2)

# Model Selection Group 1: 2013-2015
model1 <- glm(poc1$y ~ poc1$contract_2013_15+poc1$position+poc1$region+poc1$ethnicity+poc1$averagecontract_salary...8
              +poc1$average_overscale...7+poc1$averagemin_salary...6)
summary(model1)
stepAIC(model1)
logmodel1 <- glm(poc1$y~poc1$region+poc1$averagemin_salary...6)
summary(logmodel1)

# Model Selection Group 2: 2016-2019
model2 <- glm(poc2$y ~ poc2$contract_2016_19+poc2$position+poc2$region+poc2$ethnicity+poc2$averagecontract_salary...13
              +poc2$average_overscale...12+poc2$averagemin_salary...11)
summary(model2)
stepAIC(model2)
logmodel2 <- glm(poc2$y ~ poc2$position+poc2$region+poc2$averagecontract_salary...13)
summary(logmodel2)
