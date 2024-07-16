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
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

policies <- read_csv("Policy_Instruments_Paper/Data/policy_instruments_bruno.csv")

replace_letters <- function(x) {
   x <- gsub('Y', 1, x)
   x <- gsub('M', 0.5, x)
   x <- gsub('N', 0, x)
   return(x)
}

letter_cols <- c('Allocations', 'Trading', 'Taxes/Fees', 'Pumping Restrictions', 
                 'Efficiency Incentives') 

policies_clean <- policies %>% 
   select(GSP_ID, all_of(letter_cols)) %>%
   mutate(across(all_of(letter_cols), ~ replace_letters(.)))

colnames(policies_clean) <- c('GSP_ID', 'allocations', 'trading', 'taxes_fees', 
                              'pumping_restrictions', 'efficiency_incentives')
   

gsp_summary <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g])) # revert after re run step3
   gsp_id <- gsp_ids[g]
   
   gsp_stats <- data.frame(
      gsp_id = as.numeric(gsp_id),
      net_density = migraph::network_density(net),
      net_diameter = migraph::network_diameter(net),
      net_components = migraph::network_components(net),
      net_cohesion = migraph::network_cohesion(net),
      net_adhesion = migraph::network_adhesion(net),
      net_degree = migraph::network_degree(net),
      net_betweenness = migraph::network_betweenness(net),
      net_eigenvector = migraph::network_eigenvector(net),
      net_core = migraph::network_core(net),
      net_reciprocity = migraph::network_reciprocity(net),
      net_transitivity = migraph::network_transitivity(net),
      net_assortativity = migraph::network_assortativity(net)
   )
   
   net_nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))
   node_means <- net_nodes %>%
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
                projects_mgmt_actions = mean(projects_mgmt_actions_sum, na.rm = TRUE)) 
   
   gsp_summary <- rbind(gsp_summary, 
                        cbind(gsp_stats, node_means)) 
}

gsp_summary <- tibble(gsp_summary)
gsp_summary 

merged <- gsp_summary %>% 
   left_join(policies_clean, by = c("gsp_id" = "GSP_ID")) %>% 
   mutate(across(everything(), ~ as.numeric(.)))

summary(merged)

cor_places <- cor(merged)
pmat <- cor_pmat(merged)

ggcorrplot(cor_places,
           lab = TRUE,
           digits = 1
)
