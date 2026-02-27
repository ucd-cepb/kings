# step_5b_hypo1.R
# Hypothesis 1: Network Influence (In-degree / Total degree)
# Tests whether places receive more/less influence based on DAC status.
# Runs both tagged and agency modes and outputs comparison tables.
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
            is_place = ifelse(is.na(GEOID20), 0, 1)
         ) %>%
         filter(is_place == 1)

      all_place_nodes <- rbind(all_place_nodes, place_nodes)
   }

   return(all_place_nodes)
}

# --- Load both ---
cat("Loading tagged network place nodes...\n")
tagged_places <- load_place_nodes(data_dirs[["tagged"]])
cat("  N =", nrow(tagged_places), "\n")

cat("Loading agency network place nodes...\n")
agency_places <- load_place_nodes(data_dirs[["agency"]])
cat("  N =", nrow(agency_places), "\n")

# --- Tagged models ---
in_tagged_1 <- glm(`in_w` ~ DAC + POP_log + incorporated + per_latino,
                    family = poisson, data = tagged_places)
in_tagged_2 <- glm(`in_w` ~ MHI_log + POP_log + incorporated + per_latino,
                    family = poisson, data = tagged_places)
deg_tagged_1 <- glm(`deg_w` ~ DAC + POP_log + incorporated + per_latino,
                     family = poisson, data = tagged_places)
deg_tagged_2 <- glm(`deg_w` ~ MHI_log + POP_log + incorporated + per_latino,
                     family = poisson, data = tagged_places)

# --- Agency models ---
in_agency_1 <- glm(`in_w` ~ DAC + POP_log + incorporated + per_latino,
                    family = poisson, data = agency_places)
in_agency_2 <- glm(`in_w` ~ MHI_log + POP_log + incorporated + per_latino,
                    family = poisson, data = agency_places)
deg_agency_1 <- glm(`deg_w` ~ DAC + POP_log + incorporated + per_latino,
                     family = poisson, data = agency_places)
deg_agency_2 <- glm(`deg_w` ~ MHI_log + POP_log + incorporated + per_latino,
                     family = poisson, data = agency_places)

# --- Print comparison tables ---
cat("\n=== In-degree: Tagged vs Agency (DAC) ===\n")
stargazer(in_tagged_1, in_agency_1, type = 'text',
          column.labels = c("Tagged", "Agency"))

cat("\n=== In-degree: Tagged vs Agency (MHI) ===\n")
stargazer(in_tagged_2, in_agency_2, type = 'text',
          column.labels = c("Tagged", "Agency"))

cat("\n=== Total degree: Tagged vs Agency (DAC) ===\n")
stargazer(deg_tagged_1, deg_agency_1, type = 'text',
          column.labels = c("Tagged", "Agency"))

cat("\n=== Total degree: Tagged vs Agency (MHI) ===\n")
stargazer(deg_tagged_2, deg_agency_2, type = 'text',
          column.labels = c("Tagged", "Agency"))

# --- Save HTML tables ---
# Individual mode tables
stargazer(in_tagged_1, in_tagged_2, type = 'html',
          out = 'EJ_DAC_Paper/Out/mods/h2a_influence_indegree_tagged.html')
stargazer(in_agency_1, in_agency_2, type = 'html',
          out = 'EJ_DAC_Paper/Out/mods/h2a_influence_indegree_agency.html')
stargazer(deg_tagged_1, deg_tagged_2, type = 'html',
          out = 'EJ_DAC_Paper/Out/mods/h2a_influence_degree_tagged.html')
stargazer(deg_agency_1, deg_agency_2, type = 'html',
          out = 'EJ_DAC_Paper/Out/mods/h2a_influence_degree_agency.html')

# Comparison tables (tagged vs agency side-by-side)
stargazer(in_tagged_1, in_agency_1, in_tagged_2, in_agency_2, type = 'html',
          column.labels = c("Tagged", "Agency", "Tagged", "Agency"),
          out = 'EJ_DAC_Paper/Out/mods/h2a_influence_indegree_comparison.html')
stargazer(deg_tagged_1, deg_agency_1, deg_tagged_2, deg_agency_2, type = 'html',
          column.labels = c("Tagged", "Agency", "Tagged", "Agency"),
          out = 'EJ_DAC_Paper/Out/mods/h2a_influence_degree_comparison.html')
