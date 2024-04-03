####  Startup  ####
## Load packages
library(tidyverse)
library(here)
library(scales)
`%+%` <- paste0


## Load data
df <- readRDS(here("Data", "Clean Data with Concept Breadth.rds"))



####  Data Analysis  ####
## 1A. What student characteristics are associated with greater depression concept breadth?
# Demographics

# Personal experience with depression

# Depression beliefs (supplemental)

# Treatment efficacy beliefs (supplemental)

# Depression literacy (supplemental)


## 1B. Is greater depression concept breadth associated with depression self-labeling?
regression_bivariate <- glm(data = df,
                            formula = dep_self_id == "Yes" ~ concept_breadth,
                            family = "binomial")
summary(regression_bivariate)

regression_controlled <- glm(data = df,
                             formula = dep_self_id == "Yes" ~ concept_breadth + phq_sum,
                             family = "binomial")
summary(regression_controlled)

# Figure 1
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


# Checking on depression literacy and race/ethnicity. Race/ethnicity should be recoded to have a more informative comparison group
df <- df %>%
  mutate(
    race_ethnicity_recode = case_when(
      race_ethnicity == "Caucasian/Non-Hispanic White" ~ "White",
      race_ethnicity == "African American or Caribbean-Black or African" ~ "Black",
      race_ethnicity == "Hispanic or Latino or Chicano" ~ "Hispanic",
      race_ethnicity == "Other or Mixed" ~ "Other",
      race_ethnicity %in% c("East Asian", "South East Asian") ~ "Asian",
      T ~ NA_character_
    ) %>%
      factor(levels = c("White", "Black", "Hispanic", "Asian", "Other"))
  )

lm(
  data = df,
  dlit_sum ~ race_ethnicity_recode
) %>%
  summary()

lm(
  data = df,
  concept_breadth ~ race_ethnicity_recode
) %>%
  summary()

chisq.test(
  table(
    df$dep_self_id,
    df$race_ethnicity_recode
  )
)

glm(
  data = df,
  dep_self_id == "Yes" ~ race_ethnicity_recode + phq_sum,
  family = "binomial"
) %>%
  summary()

glm(
  data = df,
  dep_self_id %in% c("Maybe", "Yes") ~ race_ethnicity_recode + phq_sum,
  family = "binomial"
) %>%
  summary()
