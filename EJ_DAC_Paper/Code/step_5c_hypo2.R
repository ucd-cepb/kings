library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(sjPlot)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")
extract_list <- list.files(network_fp)

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

out_1 <- glm(`out_w` ~ DAC+
                POP_log+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

out_2 <- glm(`out_w` ~ MHI_log+
                POP_log+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

# # out_3 <- glm(`out_w_norm` ~ DAC+
#                 POP_log+
#                 incorporated+
#                 per_latino,
#              family=poisson,
#              data = all_place_nodes)
# 
# out_4 <- glm(`out_w_norm` ~ MHI_log+
#                 POP_log+
#                 incorporated+
#                 per_latino,
#              family=poisson,
#              data = all_place_nodes)

stargazer(out_1, out_2, type='text')

stargazer(out_1, out_2, type='html', out = 'EJ_DAC_Paper/Out/mods/h2b_influence_outdegree.html')
# 
# dac_stat <- exp(summary(out_1)$coefficients[2,1]); print(dac_stat)
# mhi_stat <- exp(summary(out_2)$coefficients[2,1])*3.31; print(mhi_stat)
# 
# out_a <- glm(admin_out ~ DAC+
#                 POP_std+
#                 incorporated+
#                 per_latino,
#              family=poisson,
#              data = all_place_nodes)
# 
# 
# out_b <- glm(basin_plan_out ~ DAC+
#                 POP_std+
#                 incorporated+
#                 per_latino,
#              family=poisson,
#              data = all_place_nodes)
# 
# out_c <- glm(sust_criteria_out ~ DAC+
#                 POP_std+
#                 incorporated+
#                 per_latino,
#              family=poisson,
#              data = all_place_nodes)
# 
# out_d <- glm(monitoring_networks_out ~ DAC+
#                 POP_std+
#                 incorporated+
#                 per_latino,
#              family=poisson,
#              data = all_place_nodes)
# 
# out_e <- glm(projects_mgmt_actions_out ~ DAC+
#                 POP_std+
#                 incorporated+
#                 per_latino,
#              family=poisson,
#              data = all_place_nodes)
# 
# stargazer(out_1, out_a, out_b, out_c, out_d, out_e, type='text')
# 
# stargazer(out_1, out_a, out_b, out_c, out_d, out_e, type='html', 
#           out = 'EJ_DAC_Paper/Out/mods/2b_power_by_section.html')
# 
# plot_models(in_3, in_a, in_b, in_c, in_d, in_e,
#             rm.terms = c('per_latino', 'POP_std'),
#             m.labels = c('Indegree',
#                          'Admin indegree',
#                          'Basin plan indegree',
#                          'Sust criteria indegree',
#                          'Monitoring networks indegree',
#                          'Projects mgmt actions indegree'),
#             axis.labels = c('Incorporated', 'DAC'),
#             axis.lim = c(0.5, 5))
