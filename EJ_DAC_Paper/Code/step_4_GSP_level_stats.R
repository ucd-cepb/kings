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

summary_all <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   new_row <- data.frame(gsp_id = gsp_id,
                         num_node = vcount(net),
                         num_edge = ecount(net),
                         # deg = mean(V(net)$degree, na.rm = TRUE),
                         eig = mean(V(net)$eigenvector, na.rm = TRUE),
                         # clos = mean(V(net)$closeness, na.rm = TRUE),
                         # bet = mean(V(net)$betweenness, na.rm = TRUE),
                         dist = mean(V(net)$leader_dist, na.rm = TRUE)
   )
   summary_all <- rbind(summary_all, new_row)
}

sum_tab <- summary_all %>% 
   select(-gsp_id) %>% 
   summary()

kable(sum_tab)

dac_e_cent <- data.frame()

# compare eigenvector centrality
for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   dac_eig <- mean(V(net)$eigenvector[V(net)$DAC == 0], na.rm = TRUE)
   non_dac_eig <- mean(V(net)$eigenvector[V(net)$DAC == 1], na.rm = TRUE)
   new_row <- data.frame(gsp_id = gsp_id,
                         dac_eig = dac_eig,
                         non_dac_eig = non_dac_eig,
                         hyp = dac_eig > non_dac_eig)
   dac_e_cent <- rbind(dac_e_cent, new_row)
}

summary(dac_e_cent)

dac_deg_df <- data.frame()

# compare degree 
for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   dac_deg <- mean(V(net)$degree[V(net)$DAC == 0], na.rm = TRUE)
   non_dac_deg <- mean(V(net)$degree[V(net)$DAC == 1], na.rm = TRUE)
   new_row <- data.frame(gsp_id = gsp_id,
                         dac_deg = dac_deg,
                         non_dac_deg = non_dac_deg,
                         hyp = dac_deg > non_dac_deg)
   dac_deg_df <- rbind(dac_deg_df, new_row)
}

summary(dac_deg_df)

ggplot(dac_deg_df) +
   geom_freqpoly(aes(non_dac_deg), binwidth=0.001, color = 'blue')+
   geom_freqpoly(aes(dac_deg), binwidth=0.001, color = 'red')

ggplot(dac_e_cent) +
   geom_freqpoly(aes(non_dac_eig), binwidth=0.001, color = 'blue')+
   geom_freqpoly(aes(dac_eig), binwidth=0.001, color = 'red')

places_data <- data.frame()

# compare eigenvector centrality
for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   places <- igraph::as_data_frame(induced_subgraph(net, which(V(net)$exists == 1)), what = "vertices")
   places_data <- tibble(rbind(places_data, places))
}



places_data <- places_data %>% 
   filter(!is.na(MHI)) %>% 
   filter(exists==1)


places_data_num <- places_data[, sapply(places_data, function (x) is.numeric(x))]
places_data_num <- places_data_num %>% select(-c(lat, lng, GSP_ID,GEOID20, exists,closeness))
pmat <- cor_pmat(places_data_num)
cor_places <- cor(places_data_num)
ggcorrplot(cor_places,
           p.mat=pmat,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           insig = 'blank')

deg_mod <- lm(degree ~ MHI+POP+incorporated+per_latino, data = places_data); summary(deg_mod)
eig_mod <- lm(eigenvector ~ MHI+POP+incorporated+per_latino, data = places_data); summary(eig_mod)



## TODO

## clean up stats
## simplify table of contents to 4/5 categories
## tage each edge to the section of the ToC it belongs to