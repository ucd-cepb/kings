library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(ggcorrplot)
library(knitr)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")

policies <- read_csv("Policy_Instruments_Paper/Data/policy_instruments_bruno.csv")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

gsp_summary <- data.frame()
dac_summary <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g])) # revert after re run step3
   gsp_id <- paste0("gsp_",gsp_ids[g])
   net_nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))
   gsp_sum <- net_nodes %>%
      select(-c(entity_type, GEOID20, Basin_Subb, lat, lng, GSP_ID)) %>%
      mutate(leader_dist_min = ifelse(is.infinite(leader_dist_min), NA, leader_dist_min)) %>%
      summarise(MHI = mean(MHI, na.rm = TRUE),
                POP = mean(POP, na.rm = TRUE),
                num_app = mean(num_appearances, na.rm = TRUE),
                inc = mean(incorporated, na.rm = TRUE),
                latino = mean(per_latino, na.rm = TRUE),
                eig = mean(eigenvector, na.rm = TRUE),
                lead_min = mean(leader_dist_min, na.rm = TRUE),
                degree = mean(degree, na.rm = TRUE),
                closeness = mean(closeness, na.rm = TRUE),
                admin = mean(admin_sum, na.rm = TRUE),
                basin_plan = mean(basin_plan_sum, na.rm = TRUE),
                sust_criteria = mean(sust_criteria_sum, na.rm = TRUE),
                monitoring_networks = mean(monitoring_networks_sum, na.rm = TRUE),
                projects_mgmt_actions = mean(projects_mgmt_actions_sum, na.rm = TRUE)) %>%
      mutate(gsp_id = gsp_id) %>%
      select(gsp_id, everything())
   gsp_summary <- rbind(gsp_summary, gsp_sum)
}

gsp_summary
summary(gsp_summary)
