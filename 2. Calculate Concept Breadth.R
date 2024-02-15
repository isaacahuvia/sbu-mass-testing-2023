####  Startup  ####
## Load packages
library(tidyverse)
library(here)
library(mirt)


## Load data
df <- readRDS(here("Data", "Clean Data.rds"))



####  IRT  ####
count(df, pmil_1)
count(df, pmil_2)
count(df, pmil_3)
count(df, pmil_4)
count(df, pmil_5)

pmil <- df %>%
  select(starts_with("pmil"))

irt_model <- mirt(pmil, 1, verbose = T, itemtype = 'graded', SE = TRUE)

irt_model

summary(irt_model)

M2(irt_model, type = "C2", calcNULL = FALSE, na.rm = T)

itemfit(irt_model, na.rm = T)

IRT_parms <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)
IRT_parms$items

plot(irt_model, type='trace', which.item = c(1,2,3,4,5), facet_items=T,
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8),
     theta_lim = c(-3, 3),
     main = "")

plot(irt_model, type='infotrace', which.item = c(1,2,3,4,5), facet_items=T,
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8),
     theta_lim = c(-3, 3),
     main="")

df$concept_breadth <- as.numeric(scale(fscores(irt_model)))



####  Save Data  ####
saveRDS(df, here("Data", "Clean Data with Concept Breadth.rds"))
