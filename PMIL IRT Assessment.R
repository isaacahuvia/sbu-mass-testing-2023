####  PMIL-D IRT  ####
# https://bookdown.org/bean_jerry/using_r_for_social_work_research/item-response-theory.html

df <- read.csv("/Users/isaacahuvia/Desktop/mass_testing_data.csv",
               header = T)

library(tidyverse)
library(mirt)

count(df, pmil_1)
count(df, pmil_2)
count(df, pmil_3)
count(df, pmil_4)
count(df, pmil_5)

pmil <- df %>%
  select(starts_with("pmil")) %>%
  mutate(across(everything(), ~ recode(.,
    "Definitely not depressed" = 1,
    "Probably not depressed" = 2,
    "Probably depressed" = 3,
    "Definitely depressed" = 4
  )))

mod1 <- mirt(pmil, 1, verbose = T, itemtype = 'graded', SE = TRUE)

mod1
summary(mod1)

M2(mod1, type = "C2", calcNULL = FALSE)

itemfit(mod1)

IRT_parms <- coef(mod1, IRTpars = TRUE, simplify = TRUE)
IRT_parms$items

plot(mod1, type='trace', which.item = c(1,2,3,4,5), facet_items=T,
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8),
     theta_lim = c(-3, 3),
     main = "")

plot(mod1, type='infotrace', which.item = c(1,2,3,4,5), facet_items=T,
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8),
     theta_lim = c(-3, 3),
     main="")

df$pmil_theta <- fscores(mod1)

model_data <- df %>%
  mutate(across(c(phq_1, phq_2), ~ recode(
    .,
    "Not at all" = 0,
    "Several days" = 1,
    "More than half the days" = 2,
    "Nearly every day" = 3,
    "[Decline to Answer]" = NA_real_)),
    dep_self_id = dep_self_id == "Yes",
    phq = phq_1 + phq_2,
    pmil_theta = scale(pmil_theta)) %>%
  select(dep_self_id, phq, pmil_theta)

glm(data = model_data,
    formula = dep_self_id ~ pmil_theta,
    family = "binomial") %>%
  summary()

glm(data = model_data,
    formula = dep_self_id ~ pmil_theta + phq,
    family = "binomial") %>%
  summary()
