library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(ggcorrplot)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

places_data <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))[[1]]
   gsp_id <- paste0("gsp_",gsp_ids[g])
   places <- igraph::as_data_frame(induced_subgraph(net, which(V(net)$exists == 1)), what = "vertices")
   places_data <- tibble(rbind(places_data, places))
}

places_data <- places_data %>% 
   filter(!is.na(MHI)) %>% 
   filter(exists==1) %>%
   mutate(leader_dist_min = ifelse(is.infinite(leader_dist_min), NA, leader_dist_min),
          leader_dist_w = ifelse(is.infinite(leader_dist_w), NA, leader_dist_w),
          leader_dist_uw = ifelse(is.infinite(leader_dist_uw), NA, leader_dist_uw)) %>%
   select(-c(Basin_Subb, 
             entity_type,
             lat, lng, 
             GSP_ID, GEOID20, 
             GSA,
             exists, 
             closeness, 
             indegree,
             outdegree,
             leader_dist_w, 
             leader_dist_uw))

summary(places_data)


# correlation matrix

places_data_num <- places_data %>% 
   select(-c(name)) %>% 
   filter(!is.na(leader_dist_min))

cor_places <- cor(places_data_num)
pmat <- cor_pmat(places_data_num)

ggcorrplot(cor_places,
           type='lower',
           lab = TRUE,
           digits = 1,
           p.mat=pmat,
           insig='blank'
           )

# stat comparisons

places_sum <- places_data %>%
   group_by(DAC) %>%
   summarize(across(everything(), mean, na.rm = TRUE)) %>%
   select(-name)

# Melt the data for easier faceting
places_sum_long <- places_sum %>%
   pivot_longer(cols = -DAC, names_to = "variable", values_to = "value")

# Plot with facet wrap
ggplot(places_sum_long, aes(x = DAC, y = value, fill = factor(DAC))) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "DAC", y = "Mean Value", fill = "DAC") +
   theme_minimal() +
   facet_wrap(~ variable, scales = "free_y")
