# step_5c_hypo3.R
# Hypothesis 3: Distance to Leadership (GSA)
# Tests whether DAC places are farther from GSA leadership.
# Models: Poisson GLM
#   - leader_dist_nona ~ DAC + POP_log + incorporated + per_latino
#   - leader_dist_nona ~ MHI_log + per_latino + POP_log + incorporated

library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(sjPlot)

setwd("/Users/ajguerra/Documents/projects/kings")
load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_2026")
extract_list <- list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_place_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))

   nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))
   place_nodes <- nodes %>%
      mutate(across(any_of(c('deg', 'pr', 'pr_w', 'alpha', 'in', 'out', 'eig', 'gsp_id')),
                    as.numeric)) %>%
      mutate(
         MHI_log = log(MHI),
         POP_log = log(POP),
         is_place = ifelse(is.na(GEOID20), 0, 1)
      ) %>%
      filter(is_place == 1)

   all_place_nodes <- rbind(all_place_nodes, place_nodes)
}


lead_1 <- glm(leader_dist_nona ~ DAC + POP_log + incorporated + per_latino,
                        family = poisson, data = all_place_nodes)
lead_2 <- glm(leader_dist_nona ~ MHI_log + per_latino + POP_log + incorporated,
                        family = poisson, data = all_place_nodes)

stargazer(lead_1, lead_2, type = 'text')

stargazer(lead_1, lead_2, type='html',
          out = 'EJ_DAC_Paper/Out/mods/h3_leader.html')
