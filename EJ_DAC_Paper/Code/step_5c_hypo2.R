# step_5c_hypo2.R
# Hypothesis 2: Network Influence (Out-degree)
# Tests whether places exercise more/less influence based on DAC status.
# Runs both tagged and agency modes and outputs comparison tables.
# Models: Poisson GLM
#   - out_w ~ DAC + POP_log + incorporated + per_latino
#   - out_w ~ MHI_log + POP_log + incorporated + per_latino

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
out_tagged_1 <- glm(`out_w` ~ DAC + POP_log + incorporated + per_latino,
                     family = poisson, data = tagged_places)
out_tagged_2 <- glm(`out_w` ~ MHI_log + POP_log + incorporated + per_latino,
                     family = poisson, data = tagged_places)

# --- Agency models ---
out_agency_1 <- glm(`out_w` ~ DAC + POP_log + incorporated + per_latino,
                     family = poisson, data = agency_places)
out_agency_2 <- glm(`out_w` ~ MHI_log + POP_log + incorporated + per_latino,
                     family = poisson, data = agency_places)

# --- Print comparison tables ---
cat("\n=== Out-degree: Tagged vs Agency (DAC) ===\n")
stargazer(out_tagged_1, out_agency_1, type = 'text',
          column.labels = c("Tagged", "Agency"))

cat("\n=== Out-degree: Tagged vs Agency (MHI) ===\n")
stargazer(out_tagged_2, out_agency_2, type = 'text',
          column.labels = c("Tagged", "Agency"))
