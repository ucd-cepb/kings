library(dotenv)
library(tidyverse)
library(stargazer)
library(ggraph)
library(igraph)
library(migraph)
library(data.table)
library(ggcorrplot)

load_dot_env()

place_existance <- readRDS("EJ_DAC_Paper/Data/place_existance.RDS")
all_places <- bind_rows(place_existance)
network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))


all_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   nodes <- igraph::as_data_frame(net, what = "vertices")
   nodes$gsp_id <- gsp_id
   all_nodes <- rbind(all_nodes, nodes)
}

all_place_nodes <- tibble(all_nodes) %>% 
   filter(exists == 1)


all_place_nodes_summary <- all_place_nodes %>% 
   group_by(DAC) %>%
   summarise(num_appearances = mean(num_appearances, na.rm = TRUE),
             incorporated = mean(incorporated, na.rm = TRUE),
             per_latino = mean(per_latino, na.rm = TRUE),
             eigenvector = mean(eigenvector, na.rm = TRUE),
             leader_dist = mean(leader_dist, na.rm = TRUE),
             degree = mean(degree, na.rm = TRUE),
             closeness = mean(closeness, na.rm = TRUE),
             admin = mean(admin_sum, na.rm = TRUE),
             basin_plan = mean(basin_plan_sum, na.rm = TRUE),
             sust_criteria = mean(sust_criteria_sum, na.rm = TRUE),
             monitoring_networks = mean(monitoring_networks_sum, na.rm = TRUE),
             projects_mgmt_actions = mean(projects_mgmt_actions_sum, na.rm = TRUE))

###
### HYPOTHESIS 1: DACs have lower existence than non-disadvantaged communities
###

#cross-tab for (DAC, exists)
with(all_places, table(exists, DAC))

# glm for if DAC influences exists
exists_t <- glm(exists~DAC, data = all_place_nodes); summary(exists_t)

exists_mod <- glm(exists ~ MHI+POP+incorporated+per_latino, data = all_place_nodes); summary(exists_mod)


###
### HYPOTHESIS 2: DACs have lower eigenvector centrality than non-disadvantaged communities
###

# t text for eigenvector centrality
eig_t <- t.test(all_place_nodes$eigenvector ~ all_place_nodes$DAC); eig_t

eig_mod <- lm(eigenvector ~ MHI+POP+incorporated+per_latino+num_appearances, data = all_place_nodes)
eig_mod_2 <- lm(eigenvector ~ MHI+POP+incorporated+per_latino, data = all_place_nodes)

stargazer(eig_mod, eig_mod_2)


###
### HYPOTHESIS 3: DACs have lower leader closeness than non-disadvantaged communities
###

lead_t <- t.test(all_place_nodes$leader_dist ~ all_place_nodes$DAC); lead_t

lead_mod <- lm(leader_dist ~ MHI+POP+incorporated+per_latino+num_appearances, data = all_place_nodes); summary(lead_mod)
lead_mod_2 <- lm(leader_dist ~ MHI+POP+incorporated+per_latino+num_appearances, data = all_place_nodes); summary(lead_mod)

###
### HYPOTHESIS 4a: DACs appear in the ADMIN section of the GSPs less than non-disadvantaged communities
###

admin_t <- t.test(all_place_nodes$admin_sum ~ all_place_nodes$DAC); admin_t

admin_mod <- lm(admin_sum ~ MHI+POP+incorporated+per_latino+num_appearances, data = all_place_nodes); summary(admin_mod)

###
### HYPOTHESIS 4b: DACs appear in the BASIN PLAN section of the GSPs less than non-disadvantaged communities
###

basin_plan_t <- t.test(all_place_nodes$basin_plan_sum ~ all_place_nodes$DAC); basin_plan_t

basin_plan_mod <- lm(basin_plan_sum ~ MHI+POP+incorporated+per_latino+num_appearances, data = all_place_nodes); summary(basin_plan_mod)

###
### HYPOTHESIS 4c: DACs appear in the SUSTAINABLE CRITERIA section of the GSPs less than non-disadvantaged communities
###

sust_criteria_t <- t.test(all_place_nodes$sust_criteria_sum ~ all_place_nodes$DAC); sust_criteria_t

sust_criteria_mod <- lm(sust_criteria_sum ~ MHI+POP+incorporated+per_latino+num_appearances, data = all_place_nodes); summary(sust_criteria_mod)

###
### HYPOTHESIS 4d: DACs appear in the MONITORING NETWORKS section of the GSPs less than non-disadvantaged communities
###

monitoring_networks_t <- t.test(all_place_nodes$monitoring_networks_sum ~ all_place_nodes$DAC); monitoring_networks_t

monitoring_networks_mod <- lm(monitoring_networks_sum ~ MHI+POP+incorporated+per_latino+num_appearances, data = all_place_nodes); summary(monitoring_networks_mod)

###
### HYPOTHESIS 4e: DACs appear in the PROJECT MANAGEMENT ACTIONS section of the GSPs less than non-disadvantaged communities
###

projects_mgmt_actions_t <- t.test(all_place_nodes$projects_mgmt_actions_sum ~ all_place_nodes$DAC); projects_mgmt_actions_t

projects_mgmt_actions_mod <- lm(projects_mgmt_actions_sum ~ MHI+POP+incorporated+per_latino+num_appearances, data = all_place_nodes); summary(projects_mgmt_actions_mod)

stargazer(admin_mod, 
          basin_plan_mod, 
          sust_criteria_mod, 
          monitoring_networks_mod, 
          projects_mgmt_actions_mod, 
          type = "html", 
          out = "EJ_DAC_Paper/Out/step_6a_hypo_testing_node.html")
