# Replication (self-labeling and avoidance in a new sample, new operationalization)
# Extension: Room for "not sure", break down by b/c s/n avoidance, control for dx as well, beh trt seeking

####  Startup  ####
## Load packages
library(tidyverse)
library(here)
library(scales)
`%+%` <- paste0


## Load data
df <- readRDS(here("Data", "Clean Data with Concept Breadth.rds"))



####  Data Analysis  ####
## CBAS
regression_bivariate <- lm(
  data = df,
  formula = scale(cbas_sum) ~ dep_self_id
)
summary(regression_bivariate)

regression_controlled <- lm(
  data = df,
  formula = scale(cbas_sum) ~ dep_self_id + phq_sum
)
summary(regression_controlled)

regression_double_controlled <- lm(
  data = df,
  formula = scale(cbas_sum) ~ dep_self_id + phq_sum + (dep_dx == "Yes")
)
summary(regression_double_controlled)

## SSI opt-in
regression_bivariate <- glm(
  data = df,
  formula = ssi_opt_in ~ dep_self_id,
  family = "binomial"
)
summary(regression_bivariate)

regression_controlled <- glm(
  data = df,
  formula = ssi_opt_in ~ dep_self_id + phq_sum,
  family = "binomial"
)
summary(regression_controlled)
