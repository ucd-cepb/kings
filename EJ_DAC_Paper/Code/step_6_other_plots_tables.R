library(dotenv)
library(tidyverse)
library(stargazer)
library(igraph)
library(migraph)
library(sjPlot)
library(ggpubr)
library(skimr)

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
      mutate(
         MHI_std = MHI/10000,
         POP_std = POP/1000000,
         is_place = ifelse(is.na(GEOID20), 0, 1)
      ) %>% 
      filter(is_place == 1) 
   
   all_place_nodes <- rbind(all_place_nodes, place_nodes)
}
# data summary table

all_place_nodes %>% 
   select( MHI, POP, DAC, incorporated, per_latino, in_w, out_w, leader_dist_min_w_nona) %>%
   rename("MHI" = MHI,
          "Population" = POP,
          "DAC" = DAC,
          "% Latino" = per_latino,
          "Incorporated" = incorporated,
          "Indegree" = in_w,
          "Outdegree" = out_w,
          "Leader Distance" = leader_dist_min_w_nona) %>%
   skim() %>% tibble() %>% 
   select(-c(skim_type, n_missing, complete_rate, numeric.hist,
             numeric.p0,  numeric.p25, numeric.p75, numeric.p100
             )) %>%
   rename("Variable" = skim_variable,
          "Mean" = numeric.mean,
          "SD" = numeric.sd,
          "Median" = numeric.p50) %>% 
   mutate(across(where(is.numeric), ~ round(.x, 3))) %>%   
   write_csv("EJ_DAC_Paper/Out/data_summary.csv")

# interaction plot for in_w, out_w

apn_graph <- all_place_nodes %>% 
   mutate(incorporated = as.factor(incorporated))

int_in <- glm(`in_w` ~ MHI_std*incorporated+
                 POP_std+
                 per_latino,
              family=poisson,
              data = apn_graph)


int_plot_in <- plot_model(int_in, type = 'int', 
                          terms = c('incorporated', 'MHI'),
                          title=" ",
                          legend.title = 'Incorporated',
                          axis.title = c('MHI (1,000 $)', 'Indegree'),
                          axis.lim=list(c(0,10),
                                        c(0,10)))+
   geom_vline(xintercept = c(4.85, 8.16), linetype = "dashed", color = "black") +
   theme_minimal(); int_plot_in

int_out <- glm(`out_w` ~ MHI_std*incorporated+
                  POP_std+
                  per_latino,
               family=poisson,
               data = apn_graph)

int_plot_out <- plot_model(int_out, type = 'int', 
                           terms = c('incorporated', 'MHI'),
                           title=" ",
                           legend.title = 'Incorporated',
                           axis.title = c('MHI (1,000 $)', 'Outdegree'),
                           axis.lim=list(c(0,10),
                                         c(0,10)))+
   geom_vline(xintercept = c(4.85, 8.16), linetype = "dashed", color = "black") +
   theme_minimal(); int_plot_out

int_plot <- ggarrange(int_plot_in, int_plot_out, 
                      ncol = 2, common.legend = TRUE, legend = "bottom",
                      labels = "AUTO"); int_plot

ggsave('EJ_DAC_Paper/Out/int_plot.png', int_plot, width = 7, height = 4)