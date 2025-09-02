
library(dotenv)
library(sf)
library(tidyverse)
library(knitr)
library(broom)
library(statnet)
library(ca)
library(skimr)
library(dotenv)

# visualize as heatplot
library(reshape2)
library(ggplot2)

load_dot_env()
getwd()

network_fp <- paste0(Sys.getenv("BOX_PATH"), 
                     "/network_structure_by_plan/networks_fully_labeled")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))


# Initialize list to store results for all networks
all_gsa_edges_list <- list()

# Loop through all networks in extract_list
for(i in 1:length(extract_list)) {
   cat("Processing network", i, "of", length(extract_list), ":", extract_list[i], "\n")
   
   net_i <- readRDS(paste0(network_fp, "/", extract_list[i]))
   gsp_id <- gsp_ids[i]
   net_i_df_edges <- network::as.data.frame.network(net_i, unit='edges')
   net_i_df_verts <- network::as.data.frame.network(net_i, unit='vertices')
   
   # net_i_igraph <- graph <- igraph::graph_from_data_frame(net_i_df_edges,
   #                                                        vertices = net_i_df_verts)
   
   net_i_gsa_names <- net_i_df_verts %>% 
      tibble() %>% 
      filter(entity_type == 'Local_GSA') %>% 
      pull(name)
   
   # Get all edges where GSAs are either the tail or head
   net_i_gsa_edges <- net_i_df_edges %>% 
      tibble() %>% 
      filter(.tail %in% net_i_gsa_names | .head %in% net_i_gsa_names) %>% 
      select(.tail, .head, weight)
   
   # Create a comprehensive edge table for GSAs
   # First, get edges where GSA is the tail (outgoing)
   gsa_outgoing <- net_i_gsa_edges %>%
      filter(.tail %in% net_i_gsa_names) %>%
      select(gsa = .tail, connected_to = .head, weight)
   
   # Then, get edges where GSA is the head (incoming)  
   gsa_incoming <- net_i_gsa_edges %>%
      filter(.head %in% net_i_gsa_names) %>%
      select(gsa = .head, connected_to = .tail, weight)
   
   # Combine both incoming and outgoing edges
   gsa_all_edges <- bind_rows(gsa_outgoing, gsa_incoming)
   
   # Now pivot to wide format where each GSA has columns for all its connections
   merged_gsa_edges <- gsa_all_edges %>%
      group_by(gsa, connected_to) %>%
      summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
         names_from = connected_to,
         values_from = weight,
         values_fill = 0
      )
   
   # Store results with network identifier
   all_gsa_edges_list[[paste0("network_", i, "_gsp_", gsp_id)]] <- list(
      gsp_id = gsp_id,
      gsa_edges = merged_gsa_edges,
      gsa_names = net_i_gsa_names
   )
   
   cat("Completed network", i, "with", length(net_i_gsa_names), "GSAs\n")
}

# Summary of results
cat("\n=== PROCESSING COMPLETE ===\n")
cat("Total networks processed:", length(all_gsa_edges_list), "\n")
for(i in 1:length(all_gsa_edges_list)) {
   net_name <- names(all_gsa_edges_list)[i]
   gsp_id <- all_gsa_edges_list[[i]]$gsp_id
   num_gsas <- length(all_gsa_edges_list[[i]]$gsa_names)
   cat(sprintf("%s: GSP %s, %d GSAs\n", net_name, gsp_id, num_gsas))
}

# Create master consolidated GSA edges table
cat("\n=== CREATING MASTER TABLE ===\n")

# First, collect all unique edge columns across all networks
all_edge_columns <- character(0)
for(i in 1:length(all_gsa_edges_list)) {
   edge_cols <- colnames(all_gsa_edges_list[[i]]$gsa_edges)[-1] # Exclude 'gsa' column
   all_edge_columns <- unique(c(all_edge_columns, edge_cols))
}

cat("Total unique edge columns found:", length(all_edge_columns), "\n")

# Now create a master table by combining all networks
master_gsa_edges_list <- list()

for(i in 1:length(all_gsa_edges_list)) {
   current_edges <- all_gsa_edges_list[[i]]$gsa_edges
   current_gsp_id <- all_gsa_edges_list[[i]]$gsp_id
   
   # Add GSP_ID column
   current_edges$gsp_id <- current_gsp_id
   
   # Reorder columns: gsa, gsp_id, then all edge columns
   current_edges <- current_edges %>%
      select(gsa, gsp_id, everything())
   
   # Add missing edge columns with NA values
   missing_cols <- setdiff(all_edge_columns, colnames(current_edges))
   if(length(missing_cols) > 0) {
      for(col in missing_cols) {
         current_edges[[col]] <- NA
      }
   }
   
   # Ensure all edge columns are in the same order
   edge_cols_ordered <- c("gsa", "gsp_id", all_edge_columns)
   current_edges <- current_edges %>%
      select(all_of(edge_cols_ordered))
   
   master_gsa_edges_list[[i]] <- current_edges
}

# Combine all into one master table
master_gsa_edges <- bind_rows(master_gsa_edges_list)

cat("Master table created with dimensions:", dim(master_gsa_edges), "\n")

# save to csv

write_csv(master_gsa_edges, "Network_Structure_Paper/Out/master_gsa_edges.csv")

# Show summary of the master table
cat("\n=== MASTER TABLE SUMMARY ===\n")
cat("Total GSAs across all networks:", nrow(master_gsa_edges), "\n")
cat("Total unique GSA names:", length(unique(master_gsa_edges$gsa)), "\n")
cat("Total networks represented:", length(unique(master_gsa_edges$gsp_id)), "\n")
cat("Total unique edge types:", length(all_edge_columns), "\n")


# Melt the data for ggplot
melted_master <- melt(master_gsa_edges, id.vars = c("gsa", "gsp_id")) %>% 
   mutate(value = ifelse(is.na(value), 0, value)) %>% 
   mutate(value = ifelse(value > 0, 1, 0)) # binary presence/absence
# Plot heatmap
ggplot(melted_master, aes(x = variable, y = gsa, fill = value)) +
   geom_tile() +
   scale_fill_viridis_c(option='inferno') +
   theme_minimal() +
   #no labels or legends
   theme(axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none") 

