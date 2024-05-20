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
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   places <- igraph::as_data_frame(induced_subgraph(net, which(V(net)$exists == 1)), what = "vertices")
   places_data <- tibble(rbind(places_data, places))
}

places_data <- places_data %>% 
   filter(!is.na(MHI)) %>% 
   filter(exists==1) %>%
   select(-c(Basin_Subb, entity_type, lat, lng, GSP_ID, GEOID20, exists,closeness))

summary(places_data)

places_data_num <- places_data %>% 
   select(-c(name))

pmat <- cor_pmat(places_data_num)
cor_places <- cor(places_data_num)
ggcorrplot(cor_places,
           lab = TRUE,
           p.mat=pmat,
           type = 'lower',
           insig = "blank",
           digits = 1
           )

