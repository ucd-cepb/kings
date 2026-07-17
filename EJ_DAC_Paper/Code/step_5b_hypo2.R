# step_5b_hypo2.R
# Hypothesis 2: Network Influence (Total degree)
# Tests whether places receive more/less influence based on DAC status.
# Models: Poisson GLM
#   - deg_w ~ DAC + POP_log + incorporated + per_latino
#   - deg_w ~ MHI_log + POP_log + incorporated + per_latino

library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(sjPlot)
library(ggpubr)

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


deg_1 <- glm(`deg_w` ~ DAC + POP_log + incorporated + per_latino,
                     family = poisson, data = all_place_nodes)
deg_2 <- glm(`deg_w` ~ MHI_log + POP_log + incorporated + per_latino,
                     family = poisson, data = all_place_nodes)

stargazer(deg_1, deg_2, type = 'text')

stargazer(deg_1, deg_2, type='html', out = 'EJ_DAC_Paper/Out/mods/h2_degree.html')
