raw <- read.csv("H:\\My Drive\\Research\\Projects\\Depression Beliefs\\Mass Testing\\Mass Testing data (Fall 2023) Ahuvia.csv",
               header = T)

library(tidyverse)

df <- raw %>%
  mutate(
    across(
      c(phq_1, phq_2),
      ~ recode(
        .,
        "Not at all" = 0,
        "Several days" = 1,
        "More than half the days" = 2,
        "Nearly every day" = 3,
        "[Decline to Answer]" = NA_real_
      )
    ),
    across(
      starts_with("cbas"),
      ~ recode(
        .,
        "Not at all true for me" = 0,
        "Somewhat true for me" = 1,
        "Moderately true for me" = 2,
        "Very much true for me" = 3,
        "Extremely true for me" = 4,
        "[Decline to Answer]" = NA_real_
      )
    ),
    across(c(brain_neutral, genes_neutral, brain_dys, genes_dys, starts_with("ipq"), starts_with("therapy_eff")),
           ~ gsub("\\s.*$", "", .) %>%
             as.numeric),
    dep_self_id = dep_self_id == "Yes",
    phq = phq_1 + phq_2,
    cbas_cn = cbas_cn_1 + cbas_cn_2,
    cbas_cs = cbas_cs_1 + cbas_cs_2,
    cbas_bn = cbas_bn_1 + cbas_bn_2,
    cbas_bs = cbas_bs_1 + cbas_bs_2,
    meds_eff = therapy_eff21362,
    net_dys = brain_dys + genes_dys - brain_neutral - genes_neutral
  )

lm(data = df,
   formula = cbas_cs ~ dep_self_id + phq) %>%
  summary()

lm(data = df,
   formula = cbas_cn ~ dep_self_id + phq) %>%
  summary()

lm(data = df,
   formula = cbas_bs ~ dep_self_id + phq) %>%
  summary()

lm(data = df,
   formula = cbas_bn ~ dep_self_id + phq) %>%
  summary()

# Hans analysis
library(corrplot)

df %>%
  select(brain_neutral, genes_neutral, brain_dys, genes_dys, net_dys, therapy_eff, meds_eff, phq) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot()
