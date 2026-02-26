# step_5b_hypo1_tagged.R
# Hypothesis 1: Network Influence (In-degree / Total degree)
# Tests whether places receive more/less influence based on DAC status.
# Uses tagged networks from step_4_network_stats_tagged.R
# Models: Poisson GLM
#   - in_w ~ DAC + POP_log + incorporated + per_latino
#   - in_w ~ MHI_log + POP_log + incorporated + per_latino
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

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_tagged")
extract_list <- list.files(network_fp)

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_place_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))

   gsp_id <- paste0("gsp_", gsp_ids[g])

   nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))
   place_nodes <- nodes %>%
      mutate(across(any_of(c("deg", "pr", "pr_w", "alpha", "in", "out", "eig", "gsp_id")),
                    as.numeric)) %>%
      mutate(
         MHI_log = log(MHI),
         POP_log = log(POP),
         is_place = ifelse(is.na(GEOID20), 0, 1)
      ) %>%
      filter(is_place == 1)

   all_place_nodes <- rbind(all_place_nodes, place_nodes)
}

in_1 <- glm(`in_w` ~ DAC +
               POP_log +
               incorporated +
               per_latino,
            family = poisson,
            data = all_place_nodes)

in_2 <- glm(`in_w` ~ MHI_log +
               POP_log +
               incorporated +
               per_latino,
            family = poisson,
            data = all_place_nodes)

stargazer(in_1, in_2, type = "text")

# Total degree
deg_1 <- glm(`deg_w` ~ DAC +
               POP_log +
               incorporated +
               per_latino,
            family = poisson,
            data = all_place_nodes)

deg_2 <- glm(`deg_w` ~ MHI_log +
               POP_log +
               incorporated +
               per_latino,
            family = poisson,
            data = all_place_nodes)

stargazer(deg_1, deg_2, type = "text")

stargazer(in_1, in_2, type = "html",
          out = "EJ_DAC_Paper/Out/mods/h2a_influence_indegree_tagged.html")
stargazer(deg_1, deg_2, type = "html",
          out = "EJ_DAC_Paper/Out/mods/h2a_influence_degree_tagged.html")
