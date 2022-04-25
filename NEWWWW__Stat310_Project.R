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
poc1 <- na.omit(poc1)
poc2 <- poc %>% select(1:3, 9:13) #Group 2: 2016-2019
poc2 <- na.omit(poc2)

# Add Living Wage Variable 
# Add Binary Response Variable
# Add Race Variable

## Group 1: 2013-2015
poc1 <- poc1 %>% mutate(living_wage = case_when(
  averagecontract_salary...8 >= 816 & region == 'central' ~ "yes",
  averagecontract_salary...8 >= 936 & region == 'western' ~ "yes",
  averagecontract_salary...8 >= 889 & region == 'eastern' ~ "yes",
  TRUE ~ "no")); poc1$living_wage <- factor(poc1$living_wage)
poc1$living_wage <- relevel(poc1$living_wage, "no")

poc1 <- poc1 %>% mutate(y1 = case_when(living_wage == 'yes' ~ 1, TRUE ~ 0))
poc1$y1 <- factor(poc1$y1)

poc1 <- poc1 %>% mutate(race = case_when(
  ethnicity == 'White or European American' ~ 0, TRUE ~ 1,)) 
poc1$race <- factor(poc1$race)

poc1 <- poc1 %>% relocate(averagecontract_salary...8, .before = contract_2013_15)
poc1 <- poc1 %>% relocate(living_wage, .after = averagecontract_salary...8)
poc1 <- poc1 %>% relocate(y1, .after = living_wage)
poc1 <- poc1 %>% relocate(race, .after = ethnicity)
View(poc1)

## Group 2: 2016-2019
poc2 <- poc2 %>% mutate(living_wage = case_when(
  averagecontract_salary...13 >= 816 & region == 'central' ~ "yes",
  averagecontract_salary...13 >= 936 & region == 'western' ~ "yes",
  averagecontract_salary...13 >= 889 & region == 'eastern' ~ "yes",
  TRUE ~ "no")); poc2$living_wage <- factor(poc2$living_wage)
poc2$living_wage <- relevel(poc2$living_wage, "no")
poc2 <- poc2 %>% mutate(y2 = case_when(living_wage == 'yes' ~ 1,
                                      living_wage == 'no' ~ 0))
poc2$y2 <- factor(poc2$y2)

poc2 <- poc2 %>% mutate(race = case_when(
  ethnicity == 'White or European American' ~ 0, TRUE ~ 1,))
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

model11 <- glm(y1 ~ position + region + race + contract_2013_15, 
               family = binomial(link = "logit"), data = poc1)

# Model 2: Group 2 2016-2019
model2 <- glm(y2 ~ position + region + ethnicity + contract_2016_19, 
              family = binomial(link = "logit"), data = poc2) 

model22 <- glm(y2 ~ position + region + race + contract_2016_19, 
               family = binomial(link = "logit"), data = poc2)

################################################
# Testing / Analysis
################################################

# Q1
Anova(model1)
Anova(model2)

# Q2
summary(model11)
summary(model22)
Anova(model11)
Anova(model22)

################################################
# Included in Appendix
################################################

# Living Wage
library(dplyr)
p1 <- poc1 %>% 
  group_by(living_wage) %>%
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(p1, aes(x = "", y = perc, fill = living_wage)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(
    title = "Group 1 (2013-15): Meets Living Wage Thresholds?")) +
  coord_polar(theta = "y") + 
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_void()

p2 <- poc2 %>% 
  group_by(living_wage) %>%
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(p2, aes(x = "", y = perc, fill = living_wage)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(
    title = "Group 2 (2016-19): Meets Living Wage Thresholds?")) +
  coord_polar(theta = "y") + 
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_void()

# Ethnicity & Living Wage
par(mfrow = c(1,2))
e1$poc1.living_wage <- as.numeric(poc1$living_wage)
e1 <- table(poc1$living_wage,poc1$race)
barplot(e1, xlab = 'White and BIPOC', ylab = 'Count', main 
        = "2013-15 Living Wage on Region Threshold",
        col = c("red","lightgreen"),
        density = 50,
        legend = rownames(e1), 
        args.legend = list(x = "topleft"))

e2$poc2.living_wage <- as.numeric(poc2$living_wage)
e2 <- table(poc2$living_wage,poc2$race)
barplot(e2, xlab = 'White and BIPOC', ylab = 'Count', main 
        = "2016-19 Living Wage on Region Threshold",
        col = c("red","lightgreen"),
        density = 50,
        legend = rownames(e1), 
        args.legend = list(x = "topleft"))

e.1 <- data.frame(poc1$ethnicity, poc1$living_wage)
e.1 <- table(e.1)
addmargins(e.1)

e.2 <- data.frame(poc2$ethnicity, poc2$living_wage)
e.2 <- table(e.2)
addmargins(e.2)
