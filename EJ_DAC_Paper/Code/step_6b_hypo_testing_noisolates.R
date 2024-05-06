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
   isolates <- which(degree(net) == 0)
   net_no_i <- delete_vertices(net, isolates)
   gsp_id <- paste0("gsp_",gsp_ids[g])
   nodes <- igraph::as_data_frame(net_no_i, what = "vertices")
   nodes$gsp_id <- gsp_id
   all_nodes <- rbind(all_nodes, nodes)
}

###
### HYPOTHESIS 1: DACs have lower existence than non-disadvantaged communities
###

#cross-tab for (DAC, exists)
with(all_places, table(exists, DAC))

# ttest for if DAC influences exists
exists_t_noi <- t.test(all_places$exists ~ all_places$DAC)

exists_mod_noi <- lm(exists ~ MHI+POP+incorporated+per_latino, data = all_places)


###
### HYPOTHESIS 2: DACs have lower eigenvector centrality than non-disadvantaged communities
###

eig_t_noi <- t.test(all_nodes$eigenvector ~ all_nodes$DAC)

eig_mod_noi <- lm(eigenvector ~ MHI+POP+incorporated+per_latino, data = all_nodes); summary(eig_mod)


###
### HYPOTHESIS 3: DACs have lower leader closeness than non-disadvantaged communities
###

lead_t_noi <- t.test(all_nodes$leader_dist ~ all_nodes$DAC)

lead_mod_noi <- lm(leader_dist ~ MHI+POP+incorporated+per_latino, data = all_nodes); summary(lead_mod)

###
### HYPOTHESIS 4: MHI threshold
###

ggplot(all_nodes, aes(x = MHI, y=eigenvector, color = DAC)) + 
   geom_point() +
   geom_smooth(method = "lm", se = FALSE) +
   ylim(0,0.5)

