# step_5c_hypo_3.R
# Hypothesis 3: Distance to leadership (GSA)
# Tests whether DAC places named in the plan are farther from GSA leadership.
# Models: ordinal logit with plan random intercept, N = network nodes with DAC data
#   (places named in the plan; inclusion itself is H1)
#   leader_cat: 1 (direct tie to a GSA) < 2 (one intermediary) < 3+ < unreachable (isolated, or no path to a GSA)
#   positive coefficients = farther from leadership
#   - lead_1: leader_cat ~ DAC * incorporated + POP_log + per_latino + pages_log
#   - lead_2: leader_cat ~ MHI_log * incorporated + POP_log + per_latino + pages_log
#   MHI_log is centered at the mean of the community-plan pairs (H1 sample), so incorporated is the effect at mean income
# Run from the project root (kings.Rproj).

library(tidyverse)
library(ordinal)
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
          pages_log = log(pages),
          leader_cat = factor(leader_cat,
                              levels = c("1", "2", "3+", "unreachable"),
                              ordered = TRUE))

lead_1 <- clmm(leader_cat ~ DAC * incorporated + POP_log + per_latino + pages_log + (1 | GSP_ID),
               data = place_nodes)
lead_2 <- clmm(leader_cat ~ MHI_log * incorporated + POP_log + per_latino + pages_log + (1 | GSP_ID),
               data = place_nodes)

lead_mods <- list("(1)" = lead_1, "(2)" = lead_2)

modelsummary(lead_mods, stars = TRUE, output = "markdown")

modelsummary(lead_mods, stars = TRUE, output = paste0(mod_dir, "h3_leader.html"))

saveRDS(lead_mods, paste0(mod_dir, "h3_leader.RDS"))
