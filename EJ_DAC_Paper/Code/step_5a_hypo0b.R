# step_5a_hypo0b.R
# Hypothesis 0b: Place existence in the agency-filtered network
#
# H0a (original): Does DAC status predict whether a place appears in the
#     unmodified GSP network at all?
# H0b (new):      Does DAC status predict whether a place appears in the
#     agency-filtered network (isolates dropped)?
#
# Rationale: A place may be mentioned alongside rivers, basins, or technical
# documents (appearing in the unmodified network) but have zero connections
# to social actors. H0b tests whether DAC places are less likely to be
# socially connected in the governance network.
#
# Models: Binomial GLM (logistic regression)
#   - exists_agency ~ DAC + incorporated + POP_log + per_latino
#   - exists_agency ~ MHI_log + incorporated + POP_log + per_latino
#
# Output: EJ_DAC_Paper/Out/mods/h1_existance_agency.html

library(dotenv)
library(tidyverse)
library(igraph)
library(migraph)
library(stargazer)
library(sjPlot)

load_dot_env()

# --- Load expected places and demographics ---
expected_places <- read_csv("EJ_DAC_Paper/Data/expected_places.csv")
places_raw <- read.csv("EJ_DAC_Paper/Data/places.csv")
census <- read_csv("EJ_DAC_Paper/Data/ACS_2020_DP05/ACS_2020_DP05-Data.csv")
census_latino <- census %>%
   select(GEO_ID, DP05_0071PE) %>%
   mutate(geoid = as.integer(substr(GEO_ID, 10, 16))) %>%
   select(-GEO_ID) %>%
   rename(per_latino = DP05_0071PE)

# --- Load agency networks ---
agency_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_agency/")
extract_list <- list.files(agency_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

places <- list()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(agency_fp, extract_list[g]))

   # Compute total degree and filter to connected nodes (drop isolates)
   net <- set_vertex_attr(net, "degree", value = node_deg(net))
   connected_nodes <- igraph::as_data_frame(net, what = "vertices") %>%
      select(name, degree) %>%
      filter(degree > 0) %>%
      as_tibble()

   gsp_id <- paste0("gsp_", gsp_ids[g])

   places[[gsp_id]] <- expected_places %>%
      filter(GSP_ID == gsp_ids[g]) %>%
      left_join(places_raw, by = c("GEOID20" = "geoid")) %>%
      left_join(census_latino, by = c("GEOID20" = "geoid")) %>%
      left_join(connected_nodes, by = c("NAME20" = "name")) %>%
      mutate(
         exists_agency = ifelse(is.na(degree), 0, 1),
         per_latino = as.numeric(per_latino)
      ) %>%
      select(-degree) %>%
      select(-GSP_ID, GSP_ID)
}

all_places <- bind_rows(places)

# Save for downstream use
saveRDS(places, "EJ_DAC_Paper/Data/place_existance_agency.RDS")

# --- Prepare data for models ---
all_places <- all_places %>%
   mutate(
      DAC = as.factor(DAC),
      incorporated = as.factor(incorporated),
      exists_agency = as.factor(exists_agency),
      MHI_log = log(MHI),
      POP_log = log(POP)
   )

# --- H0b models ---
exists_agency_mod_1 <- glm(exists_agency ~ DAC + incorporated + POP_log + per_latino,
                            family = binomial,
                            data = all_places)

exists_agency_mod_2 <- glm(exists_agency ~ MHI_log + incorporated + POP_log + per_latino,
                            family = binomial,
                            data = all_places)

stargazer(exists_agency_mod_1, exists_agency_mod_2, type = "text")

stargazer(exists_agency_mod_1, exists_agency_mod_2, type = "html",
          out = "EJ_DAC_Paper/Out/mods/h1_existance_agency.html")

cat("\nH0b results saved to EJ_DAC_Paper/Out/mods/h1_existance_agency.html\n")

# --- Summary ---
cat("\nTotal expected places:", nrow(all_places), "\n")
cat("Places in agency network (connected):",
    sum(all_places$exists_agency == 1, na.rm = TRUE), "\n")
cat("Places NOT in agency network:",
    sum(all_places$exists_agency == 0, na.rm = TRUE), "\n")
