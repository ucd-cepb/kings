library(tidyverse)
library(stargazer)
library(ggraph)
library(igraph)
library(migraph)
library(data.table)
library(ggcorrplot)

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

###
### HYPOTHESIS 1: DACs have lower existence than non-disadvantaged communities
###

#cross-tab for (DAC, exists)
with(all_places, table(exists, DAC))

# glm for if DAC influences exists
exists_t <- glm(exists~DAC, data = all_places); summary(exists_t)

exists_mod <- glm(exists ~ MHI+POP+incorporated+per_latino, data = all_places); summary(exists_mod)


###
### HYPOTHESIS 2: DACs have lower eigenvector centrality than non-disadvantaged communities
###

# t text for eigenvector centrality
eig_t <- t.test(all_nodes$eigenvector ~ all_nodes$DAC); eig_t

eig_mod <- lm(eigenvector ~ MHI+POP+incorporated+per_latino, data = all_nodes); summary(eig_mod)


###
### HYPOTHESIS 3: DACs have lower leader closeness than non-disadvantaged communities
###

lead_t <- t.test(all_nodes$leader_dist ~ all_nodes$DAC); lead_t

lead_mod <- lm(leader_dist ~ MHI+POP+incorporated+per_latino, data = all_nodes); summary(lead_mod)

###
### HYPOTHESIS 4: MHI threshold
###

ggplot(all_nodes, aes(x = MHI, y=eigenvector, color = DAC)) + 
   geom_point() +
   geom_smooth(method = "lm", se = FALSE) +
   ylim(0,0.5)

