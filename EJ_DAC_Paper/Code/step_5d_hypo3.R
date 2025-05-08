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

lead_mod_3 <- glm(leader_dist_min_w_nona ~ DAC+
                     POP_log+
                     incorporated+
                     per_latino,
                  family=poisson,
                  data = all_place_nodes)


lead_mod_4 <- glm(leader_dist_min_w_nona ~ MHI_log+
                     per_latino+
                     POP_log+
                     incorporated,
                  family=poisson,
                  data = all_place_nodes)

stargazer(in_1, in_2, out_1, out_2, type='text')

stargazer(lead_mod_3, lead_mod_4, type='text')

stargazer(lead_mod_3, lead_mod_4, 
          type='html', out = 'EJ_DAC_Paper/Out/mods/h3_leaderdist.html')


# lead_mod_1 <- glm(leader_dist_min ~ MHI_std+
#                      POP_std+
#                      incorporated+
#                      per_latino,
#                   family=poisson,
#                   data = all_place_nodes)
# 
# 
# lead_mod_2 <- glm(leader_dist_min ~ DAC+
#                      per_latino+
#                      POP_std+
#                      incorporated,
#                   family=poisson,
#                   data = all_place_nodes)
# 
# lead_mod_3 <- glm(leader_dist_min_w ~ MHI_std+
#                      POP_std+
#                      incorporated+
#                      per_latino,
#                   family=poisson,
#                   data = all_place_nodes)
# 
# 
# lead_mod_4 <- glm(leader_dist_min_w ~ DAC+
#                      per_latino+
#                      POP_std+
#                      incorporated,
#                   family=poisson,
#                   data = all_place_nodes)
# 
# stargazer(lead_mod_1, lead_mod_2, lead_mod_3, lead_mod_4, type='text')
# 
# stargazer(lead_mod_1, lead_mod_2, lead_mod_3, lead_mod_4, 
#           type='html', out = 'EJ_DAC_Paper/Out/mods/3a_leader_net.html')


# lead_mod_1 <- glm(leader_dist_min_nona ~ MHI_std+
#                      POP_std+
#                      incorporated+
#                      per_latino,
#                   family=poisson,
#                   data = all_place_nodes)
# 
# 
# lead_mod_2 <- glm(leader_dist_min_nona ~ DAC+
#                      per_latino+
#                      POP_std+
#                      incorporated,
#                   family=poisson,
#                   data = all_place_nodes)

