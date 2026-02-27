# step_5d_hypo3_twopart.R
# Hypothesis 3: Distance to Leadership (GSA) — Two-Part Model
# The leader distance distribution is bimodal: reachable nodes cluster
# at short distances (0–3) while unreachable nodes were previously
# imputed at the network diameter ceiling. This two-part approach
# separates the question into:
#   Part A: Does DAC predict whether a place is reachable from GSA?
#           (Logistic GLM)
#   Part B: Among reachable places, does DAC predict distance to GSA?
#           (Poisson GLM)
# Runs both tagged and agency modes.

library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(MASS)

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

tagged_places <- load_place_nodes(data_dirs[["tagged"]])

agency_places <- load_place_nodes(data_dirs[["agency"]])


cat("\nTagged reachability by DAC:\n")
tagged_places %>%
   group_by(DAC) %>%
   summarise(n = n(),
             reachable = sum(reachable, na.rm = TRUE),
             pct = round(mean(reachable/n, na.rm = TRUE) * 100, 1)) %>%
   print()

cat("\nAgency reachability by DAC:\n")
agency_places %>%
   group_by(DAC) %>%
   summarise(n = n(),
             reachable = sum(reachable, na.rm = TRUE),
             pct = round(mean(reachable/n, na.rm = TRUE) * 100, 1)) %>%
   print()

# ============================================================================
# PART A: Reachability (Logistic GLM)
# ============================================================================

cat("\n=== PART A: Reachability ===\n")

# --- Tagged models ---
reach_tagged_1 <- glm(reachable ~ DAC + POP_log + incorporated + per_latino,
                       family = binomial, data = tagged_places)
reach_tagged_2 <- glm(reachable ~ MHI_log + per_latino + POP_log + incorporated,
                       family = binomial, data = tagged_places)

# --- Agency models ---
reach_agency_1 <- glm(reachable ~ DAC + POP_log + incorporated + per_latino,
                       family = binomial, data = agency_places)
reach_agency_2 <- glm(reachable ~ MHI_log + per_latino + POP_log + incorporated,
                       family = binomial, data = agency_places)

# --- Print ---
cat("\n=== Reachability: Tagged vs Agency (DAC) ===\n")
stargazer(reach_tagged_1, reach_agency_1, type = 'text',
          column.labels = c("Tagged", "Agency"))

cat("\n=== Reachability: Tagged vs Agency (MHI) ===\n")
stargazer(reach_tagged_2, reach_agency_2, type = 'text',
          column.labels = c("Tagged", "Agency"))


# ============================================================================
# PART B: Leader distance conditional on reachable (Poisson )
# ============================================================================

cat("\n=== PART B: Leader distance | reachable ===\n")

# --- Filter to reachable places only ---
tagged_reachable <- tagged_places %>% filter(reachable == 1)
agency_reachable <- agency_places %>% filter(reachable == 1)

cat("Tagged reachable N =", nrow(tagged_reachable), "\n")
cat("Agency reachable N =", nrow(agency_reachable), "\n")

cat("\nLeader distance distribution (tagged reachable):\n")
print(summary(tagged_reachable$leader_dist))
cat("\nLeader distance distribution (agency reachable):\n")
print(summary(agency_reachable$leader_dist))

# --- Poisson models ---
dist_tagged_pois_1 <- glm(leader_dist ~ DAC + POP_log + incorporated + per_latino,
                           family = poisson, data = tagged_reachable)
dist_tagged_pois_2 <- glm(leader_dist ~ MHI_log + per_latino + POP_log + incorporated,
                           family = poisson, data = tagged_reachable)

dist_agency_pois_1 <- glm(leader_dist ~ DAC + POP_log + incorporated + per_latino,
                           family = poisson, data = agency_reachable)
dist_agency_pois_2 <- glm(leader_dist ~ MHI_log + per_latino + POP_log + incorporated,
                           family = poisson, data = agency_reachable)

# --- Print Poisson ---
cat("\n=== Leader distance (Poisson): Tagged vs Agency (DAC) ===\n")
stargazer(dist_tagged_pois_1, dist_agency_pois_1, type = 'text',
          column.labels = c("Tagged", "Agency"))

cat("\n=== Leader distance (Poisson): Tagged vs Agency (MHI) ===\n")
stargazer(dist_tagged_pois_2, dist_agency_pois_2, type = 'text',
          column.labels = c("Tagged", "Agency"))

