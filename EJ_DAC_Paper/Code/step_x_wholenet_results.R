
library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)

load_dot_env()

# place_existance <- readRDS("EJ_DAC_Paper/Data/place_existance.RDS")
# all_places <- bind_rows(place_existance)

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")
extract_list <- list.files(network_fp)

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_place_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   
   nodes <- tibble(igraph::as_data_frame(net, what = "vertices"))
   place_nodes <- nodes %>% 
      mutate(across(any_of(c('deg', 'pr', 'pr_w', 'in', 'in_w', 'in_w2', 'out', 'out_w', 'eig', 'gsp_id')),
                    as.numeric)) %>% 
      mutate(
         MHI_std = MHI/10000,
         POP_std = POP/1000000,
         is_place = ifelse(is.na(GEOID20), 0, 1)
         ) %>% 
      filter(is_place == 1) 
   
   all_place_nodes <- rbind(all_place_nodes, place_nodes)
}

all_place_nodes <- all_place_nodes %>% 
   mutate(pr_std = (pr - mean(pr, na.rm=TRUE)) / sd(pr, na.rm=TRUE) + 1,
          prw_std = (pr_w - mean(pr_w, na.rm=TRUE)) / sd(pr_w, na.rm=TRUE) + 1,
          eig_std = (eig - mean(eig, na.rm=TRUE)) / sd(eig, na.rm=TRUE) + 1
   )

summary(all_place_nodes)

in_1 <- glm(`in` ~ DAC+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

in_2 <- glm(`in` ~ MHI_std+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

in_3 <- glm(`in_w` ~ DAC+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

in_4 <- glm(`in_w` ~ MHI_std+
               POP_std+
               incorporated+
               per_latino,
            family=poisson,
            data = all_place_nodes)

stargazer(in_1, in_2, in_3, in_4, type='text')

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

stargazer(in_a, in_b, in_c, in_d, in_e, type='text')

out_1 <- glm(`out` ~ DAC+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

out_2 <- glm(`out` ~ MHI_std+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

out_3 <- glm(`out_w` ~ DAC+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

out_4 <- glm(`out_w` ~ MHI_std+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

stargazer(out_1, out_2, out_3, out_4, type='text')

out_a <- glm(admin_out ~ DAC+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)


out_b <- glm(basin_plan_out ~ DAC+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

out_c <- glm(sust_criteria_out ~ DAC+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

out_d <- glm(monitoring_networks_out ~ DAC+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

out_e <- glm(projects_mgmt_actions_out ~ DAC+
                POP_std+
                incorporated+
                per_latino,
             family=poisson,
             data = all_place_nodes)

stargazer(out_a, out_b, out_c, out_d, out_e, type='text')

# eig_mod_1<- glm(eig_std ~ MHI_std+
#                    POP_std+
#                    incorporated+
#                    per_latino,
#                 family= inverse.gaussian(link='log'),
#                 data = all_place_nodes)
# 
# eig_mod_2 <- glm(eig_std ~ DAC+
#                     POP_std+
#                     incorporated+
#                     per_latino,
#                  family= inverse.gaussian(link='log'),
#                  data = all_place_nodes)
# 
# stargazer(eig_mod_1, eig_mod_2, type='text')
# 
# pr_mod1<- glm(pr_std ~ MHI_std+
#                  POP_std+
#                  incorporated+
#                  per_latino,
#               family= inverse.gaussian(link='log'),
#               data = all_place_nodes)
# 
# pr_mod2 <- glm(pr_std ~ DAC+
#                   POP_std+
#                   incorporated+
#                   per_latino,
#                family= inverse.gaussian(link='log'),
#                data = all_place_nodes)
# 
# stargazer(pr_mod1, pr_mod2, type='text')
# 
# 
# prw_mod1<- glm(prw_std ~ MHI_std+
#                   POP_std+
#                   incorporated+
#                   per_latino,
#                family= inverse.gaussian(link='log'),
#                data = all_place_nodes)
# 
# prw_mod2 <- glm(prw_std ~ DAC+
#                    POP_std+
#                    incorporated+
#                    per_latino,
#                 family= inverse.gaussian(link='log'),
#                 data = all_place_nodes)
# 
# stargazer(prw_mod1, prw_mod2, type='text')


lead_mod_1 <- glm(leader_dist_min ~ MHI_std+
                     POP_std+
                     incorporated+
                     per_latino,
                  family=poisson,
                  data = all_place_nodes)


lead_mod_2 <- glm(leader_dist_min ~ DAC+
                     per_latino+
                     POP_std+
                     incorporated,
                  family=poisson,
                  data = all_place_nodes)

lead_mod_3 <- glm(leader_dist_min_w ~ MHI_std+
                     POP_std+
                     incorporated+
                     per_latino,
                  family=poisson,
                  data = all_place_nodes)


lead_mod_4 <- glm(leader_dist_min_w ~ DAC+
                     per_latino+
                     POP_std+
                     incorporated,
                  family=poisson,
                  data = all_place_nodes)

stargazer(lead_mod_1, lead_mod_2, lead_mod_3, lead_mod_4, type='text')


lead_mod_1 <- glm(leader_dist_min_nona ~ MHI_std+
                     POP_std+
                     incorporated+
                     per_latino,
                  family=poisson,
                  data = all_place_nodes)


lead_mod_2 <- glm(leader_dist_min_nona ~ DAC+
                     per_latino+
                     POP_std+
                     incorporated,
                  family=poisson,
                  data = all_place_nodes)

lead_mod_3 <- glm(leader_dist_min_w_nona ~ MHI_std+
                     POP_std+
                     incorporated+
                     per_latino,
                  family=poisson,
                  data = all_place_nodes)


lead_mod_4 <- glm(leader_dist_min_w_nona ~ DAC+
                     per_latino+
                     POP_std+
                     incorporated,
                  family=poisson,
                  data = all_place_nodes)

stargazer(lead_mod_1, lead_mod_2, lead_mod_3, lead_mod_4, type='text')
