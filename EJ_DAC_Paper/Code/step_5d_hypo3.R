# step_5d_hypo3.R
# Hypothesis 3: Distance to Leadership (GSA)
# Tests whether DAC places are farther from GSA leadership.
# Runs both tagged and agency modes and outputs comparison tables.
# Two distance measures:
#   - leader_dist: unweighted, undirected (preferred)
#   - leader_dist_min_w_nona: weighted, directed (legacy)
# Models: Poisson GLM
#   - leader_dist ~ DAC + POP_log + incorporated + per_latino
#   - leader_dist ~ MHI_log + per_latino + POP_log + incorporated

library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(sjPlot)

load_dot_env()

# --- Directory mapping ---
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
            is_place = ifelse(is.na(GEOID20), 0, 1),
            gsp_id = as.factor(gsp_ids[g])
         ) %>%
         filter(is_place == 1)

      all_place_nodes <- rbind(all_place_nodes, place_nodes)
   }

   return(all_place_nodes)
}

tagged_places <- load_place_nodes(data_dirs[["tagged"]])

agency_places <- load_place_nodes(data_dirs[["agency"]])

# --- Tagged models ---
lead_w_tagged_1 <- glm(leader_dist_nona ~ DAC + POP_log + incorporated + per_latino,
                        family = poisson, data = tagged_places)
lead_w_tagged_2 <- glm(leader_dist_nona ~ MHI_log + per_latino + POP_log + incorporated,
                        family = poisson, data = tagged_places)

# --- Agency models ---
lead_w_agency_1 <- glm(leader_dist_nona ~ DAC + POP_log + incorporated + per_latino,
                        family = poisson, data = agency_places)
lead_w_agency_2 <- glm(leader_dist_nona ~ MHI_log + per_latino + POP_log + incorporated,
                        family = poisson, data = agency_places)

# --- Print ---
stargazer(lead_w_tagged_1, lead_w_tagged_2, lead_w_agency_1, lead_w_agency_2, type = 'text',
          column.labels = c("Tagged", "Tagged", "Agency", "Agency"))

