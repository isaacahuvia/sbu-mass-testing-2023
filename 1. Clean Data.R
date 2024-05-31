####  Startup  ####
## Load packages
library(tidyverse)
library(here)
library(missForest)


## Set random number seed
set.seed(68026)


## Load data
raw_fall23 <- read.csv("H:\\My Drive\\Research\\Projects\\Depression Identification\\Mass Testing\\Raw Data - Fall 2023.csv",
                       header = T) %>%
  rename(
    med_eff = therapy_eff21362,
    sex = What.sex.were.you.assigned.at.birth..on.your.original.birth.certificate.,
    gender = Which.of.the.following.commonly.used.terms.best.describes.your.gender.identity.,
    parent_cob_usa = Were.your.parents.born.in.this.country.
  )

raw_spring24 <- read.csv("H:\\My Drive\\Research\\Projects\\Depression Identification\\Mass Testing\\Raw Data - Spring 2024.csv",
                         header = T) %>%
  rename(
    med_eff = therapy_eff21649,
    sex = What.sex.were.you.assigned.at.birth,
    gender = Which.of.the.fo,
    parent_cob_usa = Parents.born.in.USA.
  )


## Combine data
combined_raw_data <- bind_rows(raw_fall23, raw_spring24,
                               .id = "dataset")



####  Clean Data  ####
## Deduplicate
# We shouldn't have any duplicates here, even though we collected data across
# multiple semesters, since all respondents are students in PSY 103; nobody would
# retake it unless they failed. Still, let's double-check that we don't have
# duplicate emails.
combined_raw_data %>%
  filter(duplicated(e.mail)) %>%
  pull(e.mail) %>%
  unique()
combined_raw_data$e.mail[duplicated(combined_raw_data$e.mail)]

## Recode variables, removing irrelevant variables
# See variables in preregistration: https://docs.google.com/document/d/10CX3-Pu8o-2nZBUL1wILzqzIV0pdDcVGGKJKb9BDUnI/edit
recoded_data <- combined_raw_data %>%
  mutate(

    ## Flag NA values across all variables
    across(everything(),
           ~ na_if(., "[Decline to Answer]")),

    id = row_number(),
    dataset = dataset,

    ## Demographics
    age = as.numeric(Age),
    gender = case_when(
      gender == "Man" & sex == "Male" ~ "Cis Man",
      gender == "Woman" & sex == "Female" ~ "Cis Woman",
      !is.na(gender) ~ "TGD",
      is.na(gender) ~ NA_character_
    ),
    race_ethnicity = case_when(
      Enthnicity == "Caucasian/Non-Hispanic White" ~ "White non-Hispanic",
      Enthnicity == "African American or Caribbean-Black or African" ~ "Black non-Hispanic",
      Enthnicity %in% c("East Asian", "South East Asian") ~ "Asian non-Hispanic",
      Enthnicity == "American Indian or Alaska Native" ~ "AI/AN non-Hispanic",
      Enthnicity == "Other or Mixed" ~ "Other or Multiracial non-Hispanic",
      Enthnicity == "Hispanic or Latino or Chicano" ~ "Hispanic",
      T ~ NA_character_
    ) %>%
      factor(levels = c("White non-Hispanic", "Black non-Hispanic", "Asian non-Hispanic", "AI/AN non-Hispanic", "Other or Multiracial non-Hispanic", "Hispanic")),
    sexuality = case_when(
      SexualOrientation == "Heterosexual or straight;" ~ "Heterosexual",
      !is.na(SexualOrientation) ~ "LGB+",
      is.na(SexualOrientation) ~ NA_character_
    ),
    cob_usa = Birth.country.USA. == "Yes",
    parent_cob_usa = parent_cob_usa == "Yes",

    ## Depression beliefs (*your* depression)
    across(
      all_of(c("ipq_1", "ipq_2", "ipq_3", "ipq_4")),
      ~ case_when(
        dep_self_id %in% c("Yes", "Maybe") ~ as.numeric(gsub("\\s.*$", "", .)),
        T ~ NA_real_
      )
    ),

    ## Depression beliefs (depression *in general*)
    across(
      all_of(c("therapy_eff", "med_eff", "genes_dys", "genes_neutral", "brain_dys", "brain_neutral")),
             ~ as.numeric(gsub("\\s.*$", "", .))
    ),

    ## Concept breadth (originally "pmil": Propensity for Mental Illness Labeling)
    across(
      starts_with("pmil"),
      ~ case_match(
        .,
        "Definitely not depressed" ~ 1,
        "Probably not depressed" ~ 2,
        "Probably depressed" ~ 3,
        "Definitely depressed" ~ 4
      )
    ),

    ## Depression symptom severity
    across(
      starts_with("phq"),
      ~ case_match(
        .,
        "Not at all" ~ 0,
        "Several days" ~ 1,
        "More than half the days" ~ 2,
        "Nearly every day" ~ 3
      )
    ),

    ## Cognitive-behavioral avoidance
    across(
      starts_with("cbas"),
      ~ case_match(
        .,
        "Not at all true for me" ~ 0,
        "Somewhat true for me" ~ 1,
        "Moderately true for me" ~ 2,
        "Very much true for me" ~ 3,
        "Extremely true for me" ~ 4
      )
    ),

    ## Behavioral treatment-seeking
    ssi_opt_in = ssi_opt_in == "Yes",

    ## Depression literacy
    dlit_1 = dlit_1 == "FALSE",
    dlit_2 = dlit_2 == "TRUE",
    dlit_3 = dlit_3 == "TRUE",
    dlit_4 = dlit_4 == "FALSE",
    dlit_5 = dlit_5 == "FALSE",
    dlit_6 = dlit_6 == "TRUE",
    dlit_7 = dlit_7 == "TRUE",
    dlit_8 = dlit_8 == "FALSE",

  ) %>%

  select(

    id, dataset,

    age, race_ethnicity, gender, sexuality, cob_usa, parent_cob_usa,

    dep_self_id, dep_dx, dep_therapy, dep_meds, dep_fam, dep_friends,

    # These shouldn't be imputed and aren't going to be used anyhow
    # ipq_timeline = ipq_1,
    # ipq_control = ipq_2,
    # ipq_therapy = ipq_3,
    # ipq_medication = ipq_4,

    therapy_eff, med_eff,

    brain_neutral, genes_neutral, brain_dys, genes_dys,

    starts_with("pmil"),

    starts_with("phq"),

    starts_with("cbas"),

    ssi_opt_in,

    starts_with("dlit")

  )


## Impute data
missForest_output <- recoded_data %>%
  mutate(
    across(
      is.character | is.logical,
      as.factor
    )
  ) %>%
  missForest()

missForest_output$OOBerror

imputed_data <- missForest_output$ximp %>%
  # Re-NA-ify cells that ought to be NA: depression beliefs when dep_self_id != "Yes"
  mutate(
    across(
      starts_with("ipq"),
      ~ if_else(
        dep_self_id == "Yes",
        .,
        NA_real_
      )
    )
  )


## Calculate composites
data_with_composites <- imputed_data %>%
  mutate(

    phq_sum = phq_1 + phq_2,

    cbas_cs_sum = cbas_cs_1 + cbas_cs_2,
    cbas_cn_sum = cbas_cn_1 + cbas_cn_2,
    cbas_bs_sum = cbas_bs_1 + cbas_bs_2,
    cbas_bn_sum = cbas_bn_1 + cbas_bn_2,

    cbas_c_sum = cbas_cs_sum + cbas_cn_sum,
    cbas_b_sum = cbas_bs_sum + cbas_bn_sum,

    cbas_s_sum = cbas_cs_sum + cbas_bs_sum,
    cbas_n_sum = cbas_cn_sum + cbas_bn_sum,

    cbas_sum = cbas_c_sum + cbas_b_sum,

    across(starts_with("dlit"), as.numeric),
    dlit_sum = dlit_1 + dlit_2 + dlit_3 + dlit_4 + dlit_5 + dlit_6 + dlit_7 + dlit_8

  )


## Standardize variables
# Standardize ad-hoc variables without meaningful units (i.e., clinical variables other than PHQ-2)
standardized_data <- data_with_composites %>%
  mutate(
    across(
      c(
        starts_with("cbas"),
        starts_with("ipq"),
        dlit_sum
      ),
      scale
    )
  )


## Filter to only students <= 25 y/o
filtered_data <- standardized_data %>%
  filter(

    age <= 25 # Not pre-registered, but to align with past research

  )



####  Save Data  ####
saveRDS(filtered_data, here("Data", "Clean Data.rds"))
