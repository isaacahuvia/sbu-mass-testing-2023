####  Startup  ####
## Load packages
library(tidyverse)
library(here)
library(scales)
`%+%` <- paste0


## Load data
df <- readRDS(here("Data", "Clean Data with Concept Breadth.rds"))



####  Data Analysis  ####
regression_bivariate <- glm(data = df,
                            formula = dep_self_id == "Yes" ~ concept_breadth,
                            family = "binomial")
summary(regression_bivariate)

regression_controlled <- glm(data = df,
                             formula = dep_self_id == "Yes" ~ concept_breadth + phq_sum,
                             family = "binomial")
summary(regression_controlled)

regression_double_controlled <- glm(data = df,
                             formula = dep_self_id == "Yes" ~ concept_breadth + phq_sum + (dep_dx == "Yes"),
                             family = "binomial")
summary(regression_double_controlled)

fitted_data <- expand.grid(
  phq_sum = 0:6,
  concept_breadth = seq(-3, 3, .05)
) %>%
  mutate(
    p_self_id = predict(
      regression_controlled,
      newdata = .,
      type = "response"
    )
  )

fitted_data %>%
  ggplot() +
  geom_line(aes(concept_breadth, p_self_id, color = as.character(phq_sum)),
            linewidth = 1) +
  scale_x_continuous(
    name = "Depression Concept Breadth (z-Score)"
  ) +
  scale_y_continuous(
    name = "Probability of Depression Self-Labeling",
    labels = percent_format(),
    limits = c(0, 1)
  ) +
  scale_color_discrete(
    name = "PHQ-2 Sum Score",
    guide = guide_legend(reverse = T)
  ) +
  theme_classic()

# By IV
regress <- function(IV, control = F) {

  if(control) {

    formula = as.formula("concept_breadth ~ " %+% IV %+% " + phq_sum")

  } else {

    formula = as.formula("concept_breadth ~ " %+% IV)

  }

  regression <- lm(
    data = df,
    formula = formula
  )

  summary <- summary(regression)

  return(summary)

}

# 3a. Concept breadth by demographics
regress("age")
regress("race_ethnicity")
regress("gender")
regress("sexuality")
regress("cob_usa")
regress("parent_cob_usa")

# 3b. Concept breadth by personal experience with depression
regress("dep_self_id")
regress("dep_dx")
regress("dep_therapy")
regress("dep_meds")
regress("dep_fam")
regress("dep_friends")

# 3c. Concept breadth by depression beliefs
regress("ipq_timeline")
regress("ipq_control")
regress("ipq_therapy")
regress("ipq_medication")

# 3d. Concept breadth by treatment efficacy beliefs
regress("therapy_eff")
regress("med_eff")

# 3e. Concept breadth by depression literacy
regress("dlit_sum")
