# step_5a_hypo1.R
# Hypothesis 1: Inclusion
# Tests whether expected places are named in their plan's network based on DAC status.
# Models: logit with plan random intercept, N = community-plan pairs with DAC data
#   - incl_1: included ~ DAC * incorporated + POP_log + per_latino
#   - incl_2: included ~ MHI_log * incorporated + POP_log + per_latino
#   MHI_log is centered at the mean of the community-plan pairs, so incorporated is the effect at mean income
# Run from the project root (kings.Rproj).

library(tidyverse)
library(glmmTMB)
library(modelsummary)

data_dir <- "EJ_DAC_Paper/Data/"
mod_dir <- "EJ_DAC_Paper/Out/mods/"
if (!dir.exists(mod_dir)) dir.create(mod_dir, recursive = TRUE)

all_places <- read_csv(paste0(data_dir, "community_plan.csv")) %>%
   # DAC data not available: no ACS income for the place
   filter(!is.na(DAC)) %>%
   mutate(GSP_ID = as.factor(GSP_ID),
          MHI_log = log(MHI) - mean(log(MHI)),
          POP_log = log(POP))

incl_1 <- glmmTMB(included ~ DAC * incorporated + POP_log + per_latino + (1 | GSP_ID),
                  family = binomial, data = all_places)
incl_2 <- glmmTMB(included ~ MHI_log * incorporated + POP_log + per_latino + (1 | GSP_ID),
                  family = binomial, data = all_places)

incl_mods <- list("(1)" = incl_1, "(2)" = incl_2)

modelsummary(incl_mods, stars = TRUE, output = "markdown")

modelsummary(incl_mods, stars = TRUE, output = paste0(mod_dir, "h1_inclusion.html"))

saveRDS(incl_mods, paste0(mod_dir, "h1_inclusion.RDS"))
