# step_5d_hypo3.R
# Hypothesis 3: Distance to Leadership (GSA)
# Tests whether DAC places are farther from GSA leadership.
# Models: Poisson GLM
#   - leader_dist_min_w_nona ~ DAC + POP_log + incorporated + per_latino
#   - leader_dist_min_w_nona ~ MHI_log + per_latino + POP_log + incorporated

# ============================================================================
# CONFIGURATION - Must match the mode used in step_3 and step_4
# ============================================================================

network_mode <- "agency"   # Options: "original", "tagged", "agency"

# ============================================================================

library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(sjPlot)

load_dot_env()

# --- Directory mapping ---
data_dirs <- list(
   original = "/EJ_Paper/cleaned_extracts_DACified",
   tagged   = "/EJ_Paper/cleaned_extracts_tagged",
   agency   = "/EJ_Paper/cleaned_extracts_agency"
)

# --- Output file suffix ---
mode_suffix <- if (network_mode == "original") "" else paste0("_", network_mode)

network_fp <- paste0(Sys.getenv("BOX_PATH"), data_dirs[[network_mode]])
extract_list <- list.files(network_fp)

cat("Running in mode:", network_mode, "\n")
cat("Data directory:", network_fp, "\n")

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_place_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))

   gsp_id <- paste0("gsp_",gsp_ids[g])

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

hist(all_place_nodes$leader_dist_min_w_nona)

lead_mod_3 <- glm(leader_dist_min_w_nona ~ DAC +
                     POP_log +
                     incorporated +
                     per_latino,
                  family = poisson,
                  data = all_place_nodes)

lead_mod_4 <- glm(leader_dist_min_w_nona ~ MHI_log +
                     per_latino +
                     POP_log +
                     incorporated,
                  family = poisson,
                  data = all_place_nodes)

stargazer(lead_mod_3, lead_mod_4, type = 'text')

stargazer(lead_mod_3, lead_mod_4, type = 'html',
          out = paste0('EJ_DAC_Paper/Out/mods/h3_leaderdist', mode_suffix, '.html'))
