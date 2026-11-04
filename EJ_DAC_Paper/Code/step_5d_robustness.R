# step_5d_robustness.R
# Robustness check for H3 (step_5c): leader distance as a count.
# Models: negative binomial with plan random intercept, N = network nodes with DAC data
#   leader_dist_nona: unweighted shortest path to a GSA; unreachable = plan diameter
#   - count_lead_1: leader_dist_nona ~ DAC * incorporated + POP_log + per_latino + pages_log
#   - count_lead_2: leader_dist_nona ~ MHI_log * incorporated + POP_log + per_latino + pages_log
# Run from the project root (kings.Rproj).

library(tidyverse)
library(glmmTMB)
library(modelsummary)

data_dir <- "EJ_DAC_Paper/Data/"
mod_dir <- "EJ_DAC_Paper/Out/mods/"
if (!dir.exists(mod_dir)) dir.create(mod_dir, recursive = TRUE)

place_nodes <- read_csv(paste0(data_dir, "community_plan.csv")) %>%
   # DAC data not available: no ACS income for the place
   filter(!is.na(DAC)) %>%
   mutate(MHI_log = log(MHI) - mean(log(MHI))) %>%
   # network nodes only
   filter(included == 1) %>%
   mutate(GSP_ID = as.factor(GSP_ID),
          POP_log = log(POP),
          pages_log = log(pages))

count_lead_1 <- glmmTMB(leader_dist_nona ~ DAC * incorporated + POP_log + per_latino + pages_log + (1 | GSP_ID),
                        family = nbinom2, data = place_nodes)
count_lead_2 <- glmmTMB(leader_dist_nona ~ MHI_log * incorporated + POP_log + per_latino + pages_log + (1 | GSP_ID),
                        family = nbinom2, data = place_nodes)

count_mods <- list("(1)" = count_lead_1, "(2)" = count_lead_2)

modelsummary(count_mods, stars = TRUE, shape = term + component ~ model, output = "markdown")

modelsummary(count_mods, stars = TRUE, shape = term + component ~ model,
             output = paste0(mod_dir, "h3_leader_count.html"))
