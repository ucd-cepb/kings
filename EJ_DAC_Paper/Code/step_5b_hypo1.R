library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(sjPlot)
library(ggpubr)

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
         MHI_std = MHI/10000,
         POP_std = POP/1000000,
         is_place = ifelse(is.na(GEOID20), 0, 1)
      ) %>% 
      filter(is_place == 1) 
   
   all_place_nodes <- rbind(all_place_nodes, place_nodes)
}

in_1 <- glm(`in_w` ~ DAC+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

in_2 <- glm(`in_w` ~ MHI_std+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

stargazer(in_1, in_2, type='text')

stargazer(in_1, in_2, type='html', out = 'EJ_DAC_Paper/Out/mods/1a_recognition_net.html')

dac_stat <- exp(summary(in_1)$coefficients[2,1]); print(dac_stat)
mhi_stat <- exp(summary(in_2)$coefficients[2,1])*3.31; print(mhi_stat)

in_a <- glm(admin_in ~ DAC+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)


in_b <- glm(basin_plan_in ~ DAC+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

in_c <- glm(sust_criteria_in ~ DAC+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

in_d <- glm(monitoring_networks_in ~ DAC+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

in_e <- glm(projects_mgmt_actions_in ~ DAC+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

stargazer(in_1, in_a, in_b, in_c, in_d, in_e, type='text')

stargazer(in_1, in_a, in_b, in_c, in_d, in_e, type='html', out = 'EJ_DAC_Paper/Out/mods/1b_recognition_by_section.html')


