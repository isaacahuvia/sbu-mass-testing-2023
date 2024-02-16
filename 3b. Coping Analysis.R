####  Startup  ####
## Load packages
library(tidyverse)
library(here)
library(scales)
`%+%` <- paste0


## Load data
df <- readRDS(here("Data", "Clean Data with Concept Breadth.rds"))



####  Data Analysis  ####
regression_bivariate <- lm(
  data = df,
  formula = cbas_sum ~ dep_self_id
)
summary(regression_bivariate)

regression_controlled <- lm(
  data = df,
  formula = cbas_sum ~ dep_self_id + phq_sum
)
summary(regression_controlled)

regression_double_controlled <- lm(
  data = df,
  formula = cbas_sum ~ dep_self_id + phq_sum + (dep_dx == "Yes")
)
summary(regression_double_controlled)
