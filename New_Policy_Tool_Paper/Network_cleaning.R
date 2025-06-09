# June 2025
# for Tyler ERGMs
# Network Cleaning, Network Statistics, Core_periphery
# data = Network statistics + cp_fit_score + policy instruments + SES + GSA + Other policy instrument index(total_maybe, sum policy index)

library(ggplot2)
library(corrplot)
library(igraph)
library(statnet)
library(sna)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(broom)
library(stringr)
library(corrplot)
library(reshape2)
library(cluster)
library(lavaan)
library(netUtils)
library(stats)

## 1. load the rds, summary network statistics

file_paths <- list.files(path = "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/cleaned_unfiltered_extracts", pattern = "*.RDS", full.names = TRUE)
file_paths <- file_paths[!grepl("0089.RDS|0053.RDS", file_paths)]

# if Giant Component, change all nonisolated to GC
output_folder_statistics <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/network_statistics/"
output_csv_path <- paste0(output_folder_statistics, "network_statistics_nonisolated.csv")
merged_dataset_path <- paste0(output_folder_statistics, "merged_dataset_nonisolated.csv")


all_network_stats <- data.frame()

for (i in 1:length(file_paths)) {
   
   data <- readRDS(file_paths[i])
   nodes <- data$nodelist
   edges <- data$edgelist
   
   # collapse and weighted
   edges_with_weights <- edges %>%
      group_by(source, target) %>%
      summarize(weight = n(), .groups = 'drop')
   
   nodelist_filtered <- nodes[, c("entity_name", "entity_type"), drop = FALSE]
   colnames(nodelist_filtered)[colnames(nodelist_filtered) == "entity_name"] <- "name"
   
   # drop any isolated
   edges_with_weights <- na.omit(edges_with_weights)
   all_nodes <- unique(c(edges_with_weights$source, edges_with_weights$target))
   nodelist_filtered <- nodelist_filtered[nodelist_filtered$name %in% all_nodes, ]
   
   g <- graph_from_data_frame(d = edges_with_weights, vertices = nodelist_filtered, directed = TRUE)
   
   '#extract the giant component
   components <- igraph::components(g, mode = "weak")
   giant_component_id <- which.max(components$csize)
   vertex_ids <- V(g)[components$membership == giant_component_id]
   g <- giant_component <- igraph::induced_subgraph(g, vertex_ids)'
   
   # extract the gsp id
   file_name <- basename(file_paths[i])
   gsp_id <- as.numeric(stringr::str_extract(file_name, "\\d+"))
   
   # Calculate network statistics 
   num_nodes <- vcount(g)
   num_edges <- ecount(g)
   avg_degree <- mean(igraph::degree(g, mode = "all"))  
   avg_path_length <- mean_distance(g, directed = FALSE, weights = E(g)$weight)  
   
   # Centralization 
   degree_centralization <- centr_degree(g, mode = "all", normalized = TRUE)$centralization
   ?centr_degree
   
   # Transitivity (local and global, weighted)
   transitivity_global <- transitivity(g, type = "global", weights = E(g)$weight)
   transitivity_local <- transitivity(g, type = "local", weights = E(g)$weight)
   transitivity_local_mean <- mean(transitivity_local, na.rm = TRUE)  
   
   # Modularity (weighted)
   wtc <- cluster_walktrap(g, weights = E(g)$weight)
   modularity <- modularity(wtc)
   
   # sum
   network_stats <- data.frame(
      gsp_id = gsp_id,
      num_nodes = num_nodes,
      num_edges = num_edges,
      avg_degree = avg_degree,
      avg_path_length = avg_path_length,
      degree_centralization = degree_centralization,
      transitivity_global = transitivity_global,
      transitivity_local = transitivity_local_mean,
      modularity = modularity
   )
   
   message("✅ Processed: ", file_name)
   
   # append
   all_network_stats <- rbind(all_network_stats, network_stats)
}

## 2. Core-Periphery fitting score

CP_score_all <- data.frame()
for (i in 1:length(file_paths)) {
   
   file_name <- basename(file_paths[i])
   graph_title <- paste("Graph", gsub(".RDS", "", file_name))
   output_file <- file.path(output_folder, paste0( gsub(".RDS$", "", file_name), ".png"))
   
   file_name <- basename(file_paths[i])
   gsp_id <- as.numeric(stringr::str_extract(file_name, "\\d+"))
   
   data <- readRDS(file_paths[i])
   nodes <- data$nodelist
   edges <- data$edgelist
   
   
   edge_list_filtered <- edges[, c("source", "target")]
   
   nodelist_filtered <- nodes[, c("entity_name", "entity_type", "num_appearances"), drop = FALSE]
   colnames(nodelist_filtered)[colnames(nodelist_filtered) == "entity_name"] <- "name"
   
   # non-isolated   
   na_edges <- edge_list_filtered[is.na(edge_list_filtered$source) | is.na(edge_list_filtered$target), ]
   edge_list_filtered <- na.omit(edge_list_filtered)
   
   all_nodes <- unique(c(edge_list_filtered$source, edge_list_filtered$target))
   nodelist_filtered <- nodelist_filtered[nodelist_filtered$name %in% all_nodes, ]
   
   g <- graph_from_data_frame(d = edge_list_filtered, vertices = nodelist_filtered, directed = TRUE)
   
   '# giant component
   components <- igraph::components(g, mode = "weak")
   giant_component_id <- which.max(components$csize)
   vertex_ids <- V(g)[components$membership == giant_component_id]
   g <- igraph::induced_subgraph(g, vertex_ids)'
   
   cp_fit <- core_periphery(g)
   cp_fit_score <- cp_fit$corr
   
   CP_score <- data.frame(
      gsp_id = gsp_id,
      cp_fit_score = cp_fit_score
       )
   
   message("✅ Processed: ", file_name)
   CP_score_all <- rbind(CP_score_all, CP_score)
}


all_network_stats <- merge(all_network_stats, CP_score_all, by = "gsp_id", all.x = TRUE)

summary(all_network_stats$cp_fit_score)

# Write the combined network statistics to CSV
# write.csv(all_network_stats, file = output_csv_path, row.names = FALSE, append = TRUE)


# ------------------------ merge with policy instrument ----------------------------
policy_data <- read.csv("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/policy_by_plan.csv")
policy_data[, 2:6] <- lapply(policy_data[, 2:6], function(x) ifelse(x == 2, 1, ifelse(x == 1, 0.5, 0)))
policy_data$policy_index <- rowSums(policy_data[, 2:6])
policy_data$maybe_total <- rowSums(policy_data[, 2:6] == 0.5)

# or used all_network_stats
network_statistics <- read.csv("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/network_statistics/network_statistics_nonisolated.csv") 
merged_dataset <- merge(network_statistics, policy_data, by = "gsp_id", all.x = TRUE)
print(names(merged_dataset))       
# write.csv(merged_dataset, file = merged_dataset_path, row.names = FALSE, append = TRUE)

# ------------------------ merge with meta_data --------------------------------------

meta_data <- read.csv("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/gsp_ids_with_metadata.csv")
print(names(meta_data))
data_meta <- merged_dataset %>%
   left_join(meta_data %>% select(gsp_id,mult_gsas,priority,exante_collab,
                                  gwsum, exceedance, basin_population, local_govs_per_10k_people, 
                                  max2012dryspell, urbangw_af_log_scaled, percent_dac_by_pop_scaled, 
                                  fract_of_area_in_habitat_log_scaled, maxdryspell_scaled, 
                                  Agr_Share_Of_GDP_scaled, Perc_Bach_Degree_Over25_scaled, 
                                  log_well_MCL_exceedance_count_by_log_pop_scaled, drywellcount2014_2020, 
                                  log_drywell_per_log_person_scaled, dsci_scaled, Republican_Vote_Share
   ), by = "gsp_id")

data_meta <- data_meta %>%
   mutate(mult_gsas = ifelse(mult_gsas == TRUE, 1, 0))
data_meta <- data_meta %>% 
   mutate(exante_collab = ifelse(exante_collab == TRUE, 1, 0))
data_meta <- data_meta %>%
   mutate(priority = case_when(
      priority == "High" ~ 4,
      priority == "Medium" ~ 3,
      priority == "Low" ~ 2,
      priority == "Very Low" ~ 1,
      TRUE ~ NA_real_ 
   ))
data <- data_meta %>%
   mutate(
      Allocations = ifelse(Allocations == 0.5, 0, Allocations),
      Trading = ifelse(Trading == 0.5, 0, Allocations),
      Taxes.Fees = ifelse(Taxes.Fees == 0.5, 0, Taxes.Fees),
      Pumping.Restrictions = ifelse(Pumping.Restrictions == 0.5, 0, Pumping.Restrictions),
      Efficiency.Incentives = ifelse(Efficiency.Incentives == 0.5, 0, Efficiency.Incentives)
   )

# instrument_type
data <- data %>%
   mutate(
      instrument_type = case_when(
         (Allocations == 1 | Pumping.Restrictions == 1) & 
            (Taxes.Fees == 1 | Efficiency.Incentives == 1) ~ 3,
         (Allocations == 1 | Pumping.Restrictions == 1) ~ 1,
         (Taxes.Fees == 1 | Efficiency.Incentives == 1) ~ 2,
         TRUE ~ 0
      ),
      instrument_type = factor(
         instrument_type,
         levels = c(0, 1, 2, 3),
         labels = c("None", "Regulatory", "Market", "Both")
      )
   )

# Now data = Network statistics + cp_fit_score + policy instruments + SES + GSA + Other policy instrument index(total_maybe, sum policy index)
# Ready for model estimation