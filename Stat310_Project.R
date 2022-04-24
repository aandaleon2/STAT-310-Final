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

# ggplot: analysis average contract salary by different race
library(ggplot2)
## Group1: 2013-2015
ggplot(aes(x=poc1$averagecontract_salary...8, y=poc1$region, colour=poc1$ethnicity), 
       data=poc1)+geom_point()
## Group2: 2016-2019
ggplot(aes(x=poc2$averagecontract_salary...13, y=poc2$region, colour=poc2$ethnicity), 
       data=poc2)+geom_point()

##GEE check the.....
library(gee)
##Group2: 2013-2015
model3 <- gee(poc1$y1~poc1$race, id = poc1$race, 
                           family = binomial(link = "identity"), data = poc1)
summary(model3)
ggplot(aes(x=poc1$averagecontract_salary...8, y=poc1$region, colour=poc1$race), 
       data=poc1)+geom_point()
##Group2: 2016-2019
model4 <- gee(poc2$y2~poc2$race, id = poc2$race, 
              family = binomial(link = "identity"), data = poc2)
summary(model4)
ggplot(aes(x=poc2$averagemin_salary...11, y=poc2$region, colour=poc2$race), 
       data=poc2)+geom_point()
