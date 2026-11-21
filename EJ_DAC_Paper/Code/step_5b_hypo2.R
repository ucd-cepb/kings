# step_5b_hypo2.R
# Hypothesis 2: Network engagement (degree = node strength)
# Tests whether places named in the plan have fewer ties based on DAC status.
# Models: negative binomial with plan random intercept, N = network nodes with DAC data
#   (places named in the plan; inclusion itself is H1). Degree = 0 (isolates) are observed zeros.
#   - deg_1: degree ~ DAC * incorporated + POP_log + per_latino + pages_log
#   - deg_2: degree ~ MHI_log * incorporated + POP_log + per_latino + pages_log
#   MHI_log is centered at the mean of the community-plan pairs (H1 sample), so incorporated is the effect at mean income
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

deg_1 <- glmmTMB(degree ~ DAC * incorporated + POP_log + per_latino + pages_log + (1 | GSP_ID),
                 family = nbinom2, data = place_nodes)
deg_2 <- glmmTMB(degree ~ MHI_log * incorporated + POP_log + per_latino + pages_log + (1 | GSP_ID),
                 family = nbinom2, data = place_nodes)

deg_mods <- list("(1)" = deg_1, "(2)" = deg_2)

modelsummary(deg_mods, stars = TRUE, shape = term + component ~ model, output = "markdown")

modelsummary(deg_mods, stars = TRUE, shape = term + component ~ model,
             output = paste0(mod_dir, "h2_degree.html"))

saveRDS(deg_mods, paste0(mod_dir, "h2_degree.RDS"))
