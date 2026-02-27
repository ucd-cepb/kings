# step_5d_hypo3.R
# Hypothesis 3: Distance to Leadership (GSA)
# Tests whether DAC places are farther from GSA leadership.
# Runs both tagged and agency modes and outputs comparison tables.
# Models: Poisson GLM
#   - leader_dist_min_w_nona ~ DAC + POP_log + incorporated + per_latino
#   - leader_dist_min_w_nona ~ MHI_log + per_latino + POP_log + incorporated

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
            Basin_Subb = as.factor(Basin_Subb),
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
lead_tagged_3 <- glm(leader_dist_min_w_nona ~ DAC + POP_log + incorporated + per_latino,
                      family = poisson, data = tagged_places)
lead_tagged_4 <- glm(leader_dist_min_w_nona ~ MHI_log + POP_log + incorporated + per_latino,
                      family = poisson, data = tagged_places)

# --- Agency models ---
lead_agency_3 <- glm(leader_dist_min_w_nona ~ DAC + POP_log + incorporated + per_latino,
                      family = poisson, data = agency_places)
lead_agency_4 <- glm(leader_dist_min_w_nona ~ MHI_log + POP_log + incorporated + per_latino,,
                      family = poisson, data = agency_places)

# --- Print comparison tables ---
cat("\n=== Leader distance: Tagged vs Agency (DAC) ===\n")
stargazer(lead_tagged_3, lead_agency_3, type = 'text',
          column.labels = c("Tagged", "Agency"))

cat("\n=== Leader distance: Tagged vs Agency (MHI) ===\n")
stargazer(lead_tagged_4, lead_agency_4, type = 'text',
          column.labels = c("Tagged", "Agency"))

# --- Save HTML tables ---
# Individual mode tables
stargazer(lead_tagged_3, lead_tagged_4, type = 'html',
          out = 'EJ_DAC_Paper/Out/mods/h3_leaderdist_tagged.html')
stargazer(lead_agency_3, lead_agency_4, type = 'html',
          out = 'EJ_DAC_Paper/Out/mods/h3_leaderdist_agency.html')

# Comparison table (tagged vs agency side-by-side)
stargazer(lead_tagged_3, lead_agency_3, lead_tagged_4, lead_agency_4, type = 'html',
          column.labels = c("Tagged", "Agency", "Tagged", "Agency"),
          out = 'EJ_DAC_Paper/Out/mods/h3_leaderdist_comparison.html')
