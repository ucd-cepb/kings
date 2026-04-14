# step_5b_hypo1.R
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

data_dirs <- list(
   tagged   = "/EJ_Paper/cleaned_extracts_tagged",
   agency   = "/EJ_Paper/cleaned_extracts_agency"
)


# --- Helper: load place nodes from a network directory ---
load_place_nodes <- function(data_dir) {
   network_fp <- paste0(Sys.getenv("BOX_PATH"), data_dir)
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

   return(all_place_nodes)
}

tagged_places <- load_place_nodes(data_dirs[["tagged"]])

agency_places <- load_place_nodes(data_dirs[["agency"]])


deg_1 <- glm(`deg_w` ~ DAC + POP_log + incorporated + per_latino,
                     family = poisson, data = tagged_places)
deg_2 <- glm(`deg_w` ~ MHI_log + POP_log + incorporated + per_latino,
                     family = poisson, data = tagged_places)

stargazer(deg_1, deg_2, type = 'text')

stargazer(deg_1, deg_2, type='html', out = 'EJ_DAC_Paper/Out/mods/h2_degree.html')


deg_agency_1 <- glm(`deg_w` ~ DAC + POP_log + incorporated + per_latino,
                    family = poisson, data = agency_places)
deg_agency_2 <- glm(`deg_w` ~ MHI_log + POP_log + incorporated + per_latino,
                    family = poisson, data = agency_places)

stargazer(deg_agency_1, deg_agency_2, type = 'text')

stargazer(deg_agency_1, deg_agency_2, type='html', out = 'EJ_DAC_Paper/Out/mods/h2_degree_agency.html')