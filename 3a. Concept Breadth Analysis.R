####  Startup  ####
## Load packages
library(tidyverse)
library(here)
library(scales)


## Load data
df <- readRDS(here("Data", "Clean Data with Concept Breadth.rds"))



####  Data Analysis  ####

regression_bivariate <- glm(data = model_data,
                            formula = dep_self_id ~ pmil_theta,
                            family = "binomial")
summary(regression_bivariate)

regression_controlled <- glm(data = model_data,
                             formula = dep_self_id ~ pmil_theta + phq,
                             family = "binomial")
summary(regression_controlled)

regression_interaction <- glm(data = model_data,
                              formula = dep_self_id ~ pmil_theta*phq,
                              family = "binomial")
summary(regression_interaction)

fitted_data <- expand.grid(
  phq = 0:6,
  pmil_theta = seq(-3, 3, .01)
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
  geom_line(aes(pmil_theta, p_self_id, color = as.character(phq)),
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

# By demographics
lm(
  data = model_data,
  formula = pmil_theta ~ age
) %>%
  summary()

lm(
  data = model_data,
  formula = pmil_theta ~ ethnicity
) %>%
  summary()

lm(
  data = model_data,
  formula = pmil_theta ~ sexuality
) %>%
  summary()

lm(
  data = model_data,
  formula = pmil_theta ~ gender
) %>%
  summary()

model_data$psych = grepl("psych", model_data$Major, ignore.case = T)

lm(
  data = model_data,
  formula = pmil_theta ~ psych
) %>%
  summary()
