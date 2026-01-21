'AUG 2025

1. We want percentage of different types or organizations in core versus periphery. 
2. what is the most central organization and type in each network. 
3. if composition of core changes in networks with different levels of core periphery
4. lolog models we want node attribute variables in there for different types of nodes

'

library(network)
library(vegan)
install.packages("vegan")
library(igraph)
library(dplyr)
library(netUtils)
library(ggplot2)
library(tidyr)

# Set up file paths
network_fp <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/networks_fully_labeled/"
extract_list <- list.files(network_fp, pattern = "*.RDS", full.names = FALSE)
extract_list <- extract_list[!grepl("0089.RDS|0053.RDS", extract_list)]

output_dir <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/core_output_labeled/"


cat("Processing", length(extract_list), "network files...\n\n")

# Initialize storage lists
all_networks <- list()
all_edgelists <- list()
all_nodelists <- list()
all_igraphs <- list()
failed_files <- character()

# Process each file using original extraction method
for (i in seq_along(extract_list)) {
   file_name <- extract_list[i]
   cat("Processing file", i, "of", length(extract_list), ":", file_name, "\n")
   
   tryCatch({
      # Load network object
      net_i <- readRDS(paste0(network_fp, file_name))
      
      # Extract edges and vertices using the working approach
      net_i_df_edges <- network::as.data.frame.network(net_i, unit = 'edges')
      net_i_df_verts <- network::as.data.frame.network(net_i, unit = 'vertices')
      
      # Store original counts for comparison
      original_edges <- nrow(net_i_df_edges)
      original_verts <- nrow(net_i_df_verts)
      
      # Remove self-loops
      net_i_df_edges <- net_i_df_edges[net_i_df_edges$.tail != net_i_df_edges$.head, ]
      
      # Uncollapse edges - repeat each edge based on its weight
      if("weight" %in% names(net_i_df_edges)) {
         # Simply repeat each row 'weight' times
         net_i_df_edges <- net_i_df_edges[rep(1:nrow(net_i_df_edges), 
                                              net_i_df_edges$weight), ]
         # Drop the weight column since edges are now individual
         net_i_df_edges$weight <- NULL
      }
      
      # Remove isolated nodes
      all_nodes <- unique(c(net_i_df_edges$.tail, net_i_df_edges$.head))
      net_i_df_verts_filtered <- net_i_df_verts[net_i_df_verts$vertex.names %in% all_nodes, ]
      
      # Convert to igraph for giant component extraction
      net_i_igraph <- igraph::graph_from_data_frame(
         d = net_i_df_edges,
         vertices = net_i_df_verts_filtered,
         directed = TRUE
      )
      
      # Extract giant component
      components <- igraph::components(net_i_igraph, mode = "weak")
      giant_component_id <- which.max(components$csize)
      vertex_ids <- V(net_i_igraph)[components$membership == giant_component_id]
      net_i_igraph_gc <- igraph::induced_subgraph(net_i_igraph, vertex_ids)
      
      # Convert back to data frames
      net_i_df_edges_gc <- as.data.frame(igraph::as_edgelist(net_i_igraph_gc))
      colnames(net_i_df_edges_gc) <- c(".tail", ".head")
      
      # Add edge attributes if they exist
      edge_attrs <- igraph::edge_attr(net_i_igraph_gc)
      if(length(edge_attrs) > 0) {
         for(attr_name in names(edge_attrs)) {
            net_i_df_edges_gc[[attr_name]] <- edge_attrs[[attr_name]]
         }
      }
      
      # Get vertex data frame
      net_i_df_verts_gc <- as.data.frame(igraph::vertex_attr(net_i_igraph_gc))
      if(!"name" %in% names(net_i_df_verts_gc) && !is.null(V(net_i_igraph_gc)$name)) {
         net_i_df_verts_gc$name <- V(net_i_igraph_gc)$name
      }
      
      # Create network object from giant component
      # Allow multiple edges since we uncollapsed them
      net_i_gc <- network::network(
         x = net_i_df_edges_gc,
         vertex.attr = net_i_df_verts_gc,
         directed = TRUE,
         multiple = TRUE  # Allow parallel edges
      )
      
      # Store results with clean names
      clean_name <- gsub("\\.RDS$", "", file_name)
      all_networks[[clean_name]] <- net_i_gc
      all_edgelists[[clean_name]] <- net_i_df_edges_gc
      all_nodelists[[clean_name]] <- net_i_df_verts_gc
      all_igraphs[[clean_name]] <- net_i_igraph_gc
      
      # Report statistics
      gc_edges <- nrow(net_i_df_edges_gc)
      gc_verts <- nrow(net_i_df_verts_gc)
      
      cat("  Success! Original:", original_edges, "edges,", original_verts, "vertices\n")
      cat("           Giant Component:", gc_edges, "edges,", gc_verts, "vertices",
          "(", round(100 * gc_verts/original_verts, 1), "% of nodes)\n")
      
   }, error = function(e) {
      cat("  Error:", e$message, "\n")
      failed_files <- c(failed_files, file_name)
   })
}
















#===============================================================================
# SUMMARY STATISTICS
#===============================================================================
# Report processing results
cat("Successfully processed:", length(all_networks), "networks\n")
if(length(failed_files) > 0) {
   cat("Failed files:", length(failed_files), "\n")
   cat("  ", paste(failed_files, collapse = ", "), "\n")
}
cat("\n")

# Combine all nodelists to get overall statistics
all_entities <- do.call(rbind, lapply(names(all_nodelists), function(name) {
   df <- all_nodelists[[name]]
   if("entity_type" %in% names(df)) {
      data.frame(
         network = name,
         entity_type = df$entity_type,
         stringsAsFactors = FALSE
      )
   } else {
      NULL
   }
}))

if(!is.null(all_entities) && nrow(all_entities) > 0) {
   
   # Overall frequency table
   overall_freq <- table(all_entities$entity_type)
   overall_prop <- prop.table(overall_freq)
   
   cat("OVERALL ENTITY TYPE DISTRIBUTION (Giant Components Only)\n")
   cat("---------------------------------------------------------\n")
   overall_summary <- data.frame(
      Entity_Type = names(overall_freq),
      Count = as.numeric(overall_freq),
      Percentage = round(as.numeric(overall_prop) * 100, 2)
   )
   overall_summary <- overall_summary[order(overall_summary$Count, decreasing = TRUE), ]
   print(overall_summary, row.names = FALSE)
   
   cat("\nTotal entities across all giant components:", nrow(all_entities), "\n")
   cat("Number of unique entity types:", length(unique(all_entities$entity_type)), "\n\n")
   
   # Per-network statistics
   cat("PER-NETWORK ENTITY TYPE STATISTICS (Giant Components)\n")
   cat("------------------------------------------------------\n")
   
   network_stats <- do.call(rbind, lapply(names(all_nodelists), function(name) {
      df <- all_nodelists[[name]]
      edges_df <- all_edgelists[[name]]
      if("entity_type" %in% names(df)) {
         type_counts <- table(df$entity_type)
         data.frame(
            Network = name,
            N_Nodes = nrow(df),
            N_Edges = nrow(edges_df),
            N_Types = length(unique(df$entity_type)),
            Most_Common = names(which.max(type_counts)),
            Most_Common_N = as.numeric(max(type_counts)),
            Most_Common_Pct = round(100 * max(type_counts) / nrow(df), 1)
         )
      } else {
         data.frame(
            Network = name,
            N_Nodes = nrow(df),
            N_Edges = nrow(edges_df),
            N_Types = NA,
            Most_Common = "No entity_type",
            Most_Common_N = NA,
            Most_Common_Pct = NA
         )
      }
   }))
   
   print(network_stats, row.names = FALSE)
   
   # Core-Periphery Analysis
   cat("\n\nCORE-PERIPHERY ANALYSIS\n")
   cat("------------------------\n")
   
   # Store core-periphery results
   all_cp_results <- list()
   
   for(name in names(all_igraphs)) {
      cat("\nAnalyzing", name, "...\n")
      
      tryCatch({
         g <- all_igraphs[[name]]
         
         # Perform core-periphery analysis
         cp_fit <- netUtils::core_periphery(g)
         
         # Store results
         all_cp_results[[name]] <- list(
            corr = cp_fit$corr,
            vec = cp_fit$vec
         )
         
         # Get node attributes
         node_attrs <- all_nodelists[[name]]
         
         # Create mapping of node positions
         core_nodes <- which(cp_fit$vec == 1)
         periphery_nodes <- which(cp_fit$vec == 0)
         
         cat("  Correlation:", round(cp_fit$corr, 3), "\n")
         cat("  Core nodes:", length(core_nodes), "(",
             round(100 * length(core_nodes)/vcount(g), 1), "%)\n")
         cat("  Periphery nodes:", length(periphery_nodes), "(",
             round(100 * length(periphery_nodes)/vcount(g), 1), "%)\n")
         
      }, error = function(e) {
         cat("  Error in core-periphery analysis:", e$message, "\n")
      })
   }
   
   # Overall Core-Periphery Statistics
   if(length(all_cp_results) > 0) {
      cat("\n\nOVERALL CORE-PERIPHERY STATISTICS\n")
      cat("----------------------------------\n")
      
      # Collect all correlations
      correlations <- sapply(all_cp_results, function(x) x$corr)
      cat("Average correlation:", round(mean(correlations, na.rm = TRUE), 3), "\n")
      cat("Median correlation:", round(median(correlations, na.rm = TRUE), 3), "\n")
      cat("Min correlation:", round(min(correlations, na.rm = TRUE), 3), "\n")
      cat("Max correlation:", round(max(correlations, na.rm = TRUE), 3), "\n")
      
      # Aggregate entity type distribution across all networks with correlation info
      all_core_periphery_data <- do.call(rbind, lapply(names(all_cp_results), function(name) {
         if(!is.null(all_cp_results[[name]])) {
            node_attrs <- all_nodelists[[name]]
            if("entity_type" %in% names(node_attrs) && 
               nrow(node_attrs) == length(all_cp_results[[name]]$vec)) {
               data.frame(
                  network = name,
                  entity_type = node_attrs$entity_type,
                  position = ifelse(all_cp_results[[name]]$vec == 1, "Core", "Periphery"),
                  correlation = all_cp_results[[name]]$corr,
                  stringsAsFactors = FALSE
               )
            }
         }
      }))
      
      if(!is.null(all_core_periphery_data) && nrow(all_core_periphery_data) > 0) {
         
         # ========================================================================
         # STATISTIC 1: Percentage of different types in core vs periphery
         # ========================================================================
         cat("\n\n========================================================================")
         cat("\nSTATISTIC 1: PERCENTAGE OF ENTITY TYPES IN CORE VS PERIPHERY")
         cat("\n========================================================================\n")
         
         # Overall composition of core vs periphery
         core_data <- all_core_periphery_data[all_core_periphery_data$position == "Core", ]
         periphery_data <- all_core_periphery_data[all_core_periphery_data$position == "Periphery", ]
         
         # Count entity types in core
         core_entity_counts <- table(core_data$entity_type)
         core_entity_pct <- round(100 * core_entity_counts / sum(core_entity_counts), 2)
         
         # Count entity types in periphery
         periphery_entity_counts <- table(periphery_data$entity_type)
         periphery_entity_pct <- round(100 * periphery_entity_counts / sum(periphery_entity_counts), 2)
         
         # Combine into a comparison table
         all_entity_types <- unique(c(names(core_entity_pct), names(periphery_entity_pct)))
         
         comparison_df <- data.frame(
            Entity_Type = all_entity_types,
            Core_Pct = as.numeric(core_entity_pct[all_entity_types]),
            Periphery_Pct = as.numeric(periphery_entity_pct[all_entity_types]),
            stringsAsFactors = FALSE
         )
         comparison_df[is.na(comparison_df)] <- 0
         comparison_df$Difference = comparison_df$Core_Pct - comparison_df$Periphery_Pct
         comparison_df <- comparison_df[order(abs(comparison_df$Difference), decreasing = TRUE), ]
         
         cat("\nEntity Type Composition: Core vs Periphery\n")
         cat("(Percentage of nodes in each position that are of each type)\n")
         cat("--------------------------------------------------------------\n")
         print(head(comparison_df, 30), row.names = FALSE)
         
         cat("\nSummary:\n")
         cat("- Total nodes in Core:", nrow(core_data), "\n")
         cat("- Total nodes in Periphery:", nrow(periphery_data), "\n")
         
         
   # Distribution visualization (text-based)
   cat("\n\nENTITY TYPE DISTRIBUTION CHART (Giant Components)\n")
   cat("--------------------------------------------------\n")
   top_types <- head(overall_summary, 30)
   for(i in 1:nrow(top_types)) {
      bar_length <- round(top_types$Percentage[i] / 2)  # Scale for display
      cat(sprintf("%-20s %s %.1f%% (%d)\n",
                  top_types$Entity_Type[i],
                  paste(rep("█", bar_length), collapse = ""),
                  top_types$Percentage[i],
                  top_types$Count[i]))
   }
   
} else {
   cat("WARNING: No entity_type variable found in any nodelist.\n")
}




## IF EXCLUDE NONSENSE
#===============================================================================
if(!is.null(all_core_periphery_data) && nrow(all_core_periphery_data) > 0) {
   
   # Filter for "Nonsense" entity type only
   nonsense_data <- all_core_periphery_data[all_core_periphery_data$entity_type == "Nonsense", ]
   
   if(nrow(nonsense_data) > 0) {
      cat("\nNONSENSE ENTITY COUNTS BY CORE/PERIPHERY\n")
      cat("-----------------------------------\n")
      
      # Overall counts
      nonsense_counts <- table(nonsense_data$position)
      nonsense_total <- sum(nonsense_counts)
      
      cat("Total Nonsense entities:", nonsense_total, "\n")
      cat("  - In Core:", as.numeric(nonsense_counts["Core"]), 
          "(", round(100 * nonsense_counts["Core"] / nonsense_total, 1), "%)\n")
      cat("  - In Periphery:", as.numeric(nonsense_counts["Periphery"]), 
          "(", round(100 * nonsense_counts["Periphery"] / nonsense_total, 1), "%)\n")
      
   } else {
      cat("\nNo 'Nonsense' entity type found in the data.\n")
   }
}


## ENTITY TYPE FOR EACH NETWORK
#===============================================================================
if(!is.null(all_core_periphery_data) && nrow(all_core_periphery_data) > 0) {
   
   # Get all unique entity types across all networks
   all_entity_types <- sort(unique(all_core_periphery_data$entity_type))
   
   # Initialize the dataset
   network_dataset <- do.call(rbind, lapply(names(all_cp_results), function(network_name) {
      
      # Extract GSP ID from network name (assuming it's the filename without extension)
      gsp_id <- network_name
      
      # Get CP fit score
      cp_fit_score <- all_cp_results[[network_name]]$corr
      
      # Get network data
      network_data <- all_core_periphery_data[all_core_periphery_data$network == network_name, ]
      
      # Get core data only
      core_data <- network_data[network_data$position == "Core", ]
      
      # Count total core nodes
      core_count <- nrow(core_data)
      
      # Calculate percentage of each entity type in core
      if(core_count > 0) {
         entity_counts <- table(core_data$entity_type)
         entity_percentages <- 100 * entity_counts / core_count
         
         # Create base row
         row_data <- data.frame(
            gsp_id = gsp_id,
            cp_fit_score = round(cp_fit_score, 4),
            core_count = core_count,
            stringsAsFactors = FALSE
         )
         
         # Add percentage columns for each entity type
         for(entity_type in all_entity_types) {
            # Clean entity type name for column name (replace spaces/special chars with underscore)
            clean_name <- paste0("pct_", gsub("[^A-Za-z0-9]", "_", entity_type))
            
            if(entity_type %in% names(entity_percentages)) {
               row_data[[clean_name]] <- round(as.numeric(entity_percentages[entity_type]), 2)
            } else {
               row_data[[clean_name]] <- 0.00
            }
         }
         
         return(row_data)
      } else {
         # Handle networks with no core nodes
         row_data <- data.frame(
            gsp_id = gsp_id,
            cp_fit_score = round(cp_fit_score, 4),
            core_count = 0,
            stringsAsFactors = FALSE
         )
         
         # Add zero percentages for all entity types
         for(entity_type in all_entity_types) {
            clean_name <- paste0("pct_", gsub("[^A-Za-z0-9]", "_", entity_type))
            row_data[[clean_name]] <- 0.00
         }
         
         return(row_data)
      }
   }))
   
   # Sort by GSP ID
   network_dataset <- network_dataset[order(network_dataset$gsp_id), ]
   
   # Display summary information
   cat("Dimensions:", nrow(network_dataset), "networks x", ncol(network_dataset), "variables\n")
   cat("Entity types included:", length(all_entity_types), "\n\n")
   
   # Show first few rows
   cat("First 5 networks (showing first 8 columns):\n")
   print(network_dataset[1:min(5, nrow(network_dataset)), 1:min(8, ncol(network_dataset))], row.names = FALSE)
   
   # Show column names for reference
   cat("\n\nAll column names:\n")
   cat("Basic columns: gsp_id, cp_fit_score, core_count\n")
   cat("Entity type percentage columns:\n")
   entity_cols <- names(network_dataset)[grepl("^pct_", names(network_dataset))]
   for(i in seq(1, length(entity_cols), 3)) {
      end_idx <- min(i + 2, length(entity_cols))
      cat("  ", paste(entity_cols[i:end_idx], collapse = ", "), "\n")
   }
   
   # Basic summary statistics
   cat("\n\nSUMMARY STATISTICS:\n")
   cat("-------------------\n")
   cat("CP Fit Score - Mean:", round(mean(network_dataset$cp_fit_score, na.rm = TRUE), 3), 
       "Min:", round(min(network_dataset$cp_fit_score, na.rm = TRUE), 3),
       "Max:", round(max(network_dataset$cp_fit_score, na.rm = TRUE), 3), "\n")
   cat("Core Count - Mean:", round(mean(network_dataset$core_count, na.rm = TRUE), 1),
       "Min:", min(network_dataset$core_count, na.rm = TRUE),
       "Max:", max(network_dataset$core_count, na.rm = TRUE), "\n")
   
   # Save the dataset 
   output_file <- paste0(output_dir, "network_cp_entity_dataset.csv")
   write.csv(network_dataset, output_file, row.names = FALSE)
   cat("Dataset saved to:", output_file, "\n")
   
} 



#===============================================================================
# DEFINE CP LEVEL
#===============================================================================

# CP Fit Score Summary Statistics and Categorization Analysis
if(exists("network_dataset") && !is.null(network_dataset)) {
   
   cat("CP FIT SCORE SUMMARY STATISTICS\n")
   cat("================================\n")
   
   cp_scores <- network_dataset$cp_fit_score
   
   # Basic descriptive statistics
   cat("Basic Statistics:\n")
   cat("-----------------\n")
   cat("N (networks):", length(cp_scores), "\n")
   cat("Mean:", round(mean(cp_scores, na.rm = TRUE), 4), "\n")
   cat("Median:", round(median(cp_scores, na.rm = TRUE), 4), "\n")
   cat("Standard Deviation:", round(sd(cp_scores, na.rm = TRUE), 4), "\n")
   cat("Minimum:", round(min(cp_scores, na.rm = TRUE), 4), "\n")
   cat("Maximum:", round(max(cp_scores, na.rm = TRUE), 4), "\n")
   cat("Range:", round(max(cp_scores, na.rm = TRUE) - min(cp_scores, na.rm = TRUE), 4), "\n\n")
   
   # Percentiles for distribution-based approach
   cat("Distribution-based Percentiles:\n")
   cat("-------------------------------\n")
   percentiles <- quantile(cp_scores, probs = c(0, 0.25, 0.33, 0.5, 0.67, 0.75, 1), na.rm = TRUE)
   for(i in 1:length(percentiles)) {
      cat(sprintf("%dth percentile: %.4f\n", as.numeric(names(percentiles)[i]) * 100, percentiles[i]))
   }
   
   # Distribution-based tertiles
   tertiles <- quantile(cp_scores, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
   cat("\nDistribution-based Tertiles:\n")
   cat("----------------------------\n")
   cat("Low CP (0-33rd percentile):", round(tertiles[1], 4), "to", round(tertiles[2], 4), "\n")
   cat("Medium CP (34th-67th percentile):", round(tertiles[2], 4), "to", round(tertiles[3], 4), "\n")
   cat("High CP (68th-100th percentile):", round(tertiles[3], 4), "to", round(tertiles[4], 4), "\n")
   
   # Magnitude-based categories
   cat("\nMagnitude-based Categories:\n")
   cat("---------------------------\n")
   cat("Low CP (0-0.33): ", round(min(cp_scores), 4), "to 0.33\n")
   cat("Medium CP (0.34-0.66): 0.34 to 0.66\n")
   cat("High CP (0.67-1.0): 0.67 to", round(max(cp_scores), 4), "\n")
   
   # Compare the two approaches
   cat("\n\nCOMPARISON OF CATEGORIZATION APPROACHES\n")
   cat("========================================\n")
   
   # Distribution-based categories
   dist_categories <- cut(cp_scores, 
                          breaks = tertiles, 
                          labels = c("Low CP", "Medium CP", "High CP"),
                          include.lowest = TRUE)
   dist_table <- table(dist_categories, useNA = "ifany")
   
   # Magnitude-based categories
   mag_categories <- cut(cp_scores,
                         breaks = c(0, 0.33, 0.66, 1.0),
                         labels = c("Low CP", "Medium CP", "High CP"),
                         include.lowest = TRUE)
   mag_table <- table(mag_categories, useNA = "ifany")
   
   # Distribution-based results
   cat("DISTRIBUTION-BASED (Tertiles):\n")
   cat("-------------------------------\n")
   for(i in 1:length(dist_table)) {
      pct <- round(100 * dist_table[i] / sum(dist_table), 1)
      cat(sprintf("%-10s: %d networks (%.1f%%)\n", names(dist_table)[i], dist_table[i], pct))
   }
   
   cat("\nScore ranges for distribution-based:\n")
   cat("Low CP:", sprintf("%.4f to %.4f", tertiles[1], tertiles[2]), "\n")
   cat("Medium CP:", sprintf("%.4f to %.4f", tertiles[2], tertiles[3]), "\n")
   cat("High CP:", sprintf("%.4f to %.4f", tertiles[3], tertiles[4]), "\n")
   
   # Magnitude-based results
   cat("\nMAGNITUDE-BASED (Equal intervals):\n")
   cat("-----------------------------------\n")
   for(i in 1:length(mag_table)) {
      pct <- round(100 * mag_table[i] / sum(mag_table), 1)
      cat(sprintf("%-10s: %d networks (%.1f%%)\n", names(mag_table)[i], mag_table[i], pct))
   }
   
   # Check how many fall into each magnitude category
   cat("\nActual score distribution in magnitude categories:\n")
   low_mag <- sum(cp_scores >= 0 & cp_scores <= 0.33, na.rm = TRUE)
   med_mag <- sum(cp_scores > 0.33 & cp_scores <= 0.66, na.rm = TRUE)
   high_mag <- sum(cp_scores > 0.66 & cp_scores <= 1.0, na.rm = TRUE)
   
   cat("Low (0-0.33):", low_mag, "networks\n")
   cat("Medium (0.34-0.66):", med_mag, "networks\n") 
   cat("High (0.67-1.0):", high_mag, "networks\n")
   
}

      
# ==============================================================================
## CORE COMPOSITION CHANGES WITH CP LEVEL
# ==============================================================================
      # By Distribution
      cp_scores <- network_dataset$cp_fit_score
      tertiles <- quantile(cp_scores, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
      
      # Add CP strength categories to network dataset
      network_dataset$cp_strength <- cut(network_dataset$cp_fit_score,
                                         breaks = tertiles,
                                         labels = c("Low CP", "Medium CP", "High CP"),
                                         include.lowest = TRUE)
      
      # Create mapping of networks to their CP strength
      network_cp_mapping <- setNames(network_dataset$cp_strength, network_dataset$gsp_id)
      
      # Add CP strength to the core-periphery node data
      all_core_periphery_data$cp_strength_new <- network_cp_mapping[all_core_periphery_data$network]
      
      # Analyze core composition for each strength level
      core_only <- all_core_periphery_data[all_core_periphery_data$position == "Core", ]
      
      # Get top entity types overall for consistent comparison
      top_entities_overall <- names(sort(table(core_only$entity_type), decreasing = TRUE))[1:15]
      
      # Create composition matrix
      composition_by_strength <- matrix(0, nrow = length(top_entities_overall),
                                        ncol = 3)
      rownames(composition_by_strength) <- top_entities_overall
      colnames(composition_by_strength) <- c("Low CP", "Medium CP", "High CP")
      
      for(i in 1:3) {
         strength_level <- levels(network_dataset$cp_strength)[i]
         strength_core <- core_only[core_only$cp_strength_new == strength_level, ]
         
         if(nrow(strength_core) > 0) {
            entity_counts <- table(strength_core$entity_type)
            
            for(entity in top_entities_overall) {
               if(entity %in% names(entity_counts)) {
                  composition_by_strength[entity, i] <- 
                     round(100 * entity_counts[entity] / sum(entity_counts), 2)
               }
            }
         }
      }
      
      # Display composition matrix
      composition_df <- as.data.frame(composition_by_strength)
      composition_df$Entity_Type <- rownames(composition_df)
      composition_df <- composition_df[, c("Entity_Type", "Low CP", "Medium CP", "High CP")]
      
      # Calculate change from low to high CP
      composition_df$Change_Low_to_High <- composition_df$`High CP` - composition_df$`Low CP`
      
      print(composition_df, row.names = FALSE)
      
      
      
      ## IF CONTINUOUS, REGRESSION?
      # corr local, gsa, 
      # perdict prob of each node being core-pheriohery, baye model-
      # prob. of node of being in a core logic 
      # shann diversity, corr to cp
      model <- lm(cp_fit_score ~ pct_Basin + pct_City + pct_Company + pct_County + 
                     pct_Data_System + pct_District + pct_Federal_Gov + 
                     pct_Geographic_Unit + pct_Group + pct_Infrastructure + 
                     pct_Legal + pct_Local_Gov + pct_Local_GSA + 
                     pct_Natural_Feature + pct_NGO + pct_Nonsense + 
                     pct_Other_GSA + pct_Person + pct_Reference + 
                     pct_State_Gov + pct_Technical + pct_Water_Project, data = network_dataset)
      summary(model)
      names(network_dataset)
      
      
      
## More Visualized Graph Sept 30
      # Load necessary libraries
      library(dplyr)
      library(ggplot2)
      library(tidyr)
      library(gridExtra)
      
      # Prepare data - keep only needed variables and remove NAs
      plot_data <- data_meta %>%
         select(cp_fit_score, edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef) %>%
         drop_na()
      
      # ====== Option 1: Four separate plots in a grid ======
      p1 <- ggplot(plot_data, aes(x = cp_fit_score, y = edges_coef)) +
         geom_point(alpha = 0.6, color = "#E31A1C") +
         geom_smooth(method = "loess", color = "black", se = TRUE) +
         labs(title = "Edges Coefficient", x = "CP Fit Score", y = "Edges Coef") +
         theme_minimal()
      
      p2 <- ggplot(plot_data, aes(x = cp_fit_score, y = mutual_coef)) +
         geom_point(alpha = 0.6, color = "#1F78B4") +
         geom_smooth(method = "loess", color = "black", se = TRUE) +
         labs(title = "Mutual Coefficient", x = "CP Fit Score", y = "Mutual Coef") +
         theme_minimal()
      
      p3 <- ggplot(plot_data, aes(x = cp_fit_score, y = gwdegree_coef)) +
         geom_point(alpha = 0.6, color = "#33A02C") +
         geom_smooth(method = "loess", color = "black", se = TRUE) +
         labs(title = "GW Degree Coefficient", x = "CP Fit Score", y = "GW Degree Coef") +
         theme_minimal()
      
      p4 <- ggplot(plot_data, aes(x = cp_fit_score, y = gwdsp_coef)) +
         geom_point(alpha = 0.6, color = "#FF7F00") +
         geom_smooth(method = "loess", color = "black", se = TRUE) +
         labs(title = "GWDSP Coefficient", x = "CP Fit Score", y = "GWDSP Coef") +
         theme_minimal()
      
      # Arrange in 2x2 grid
      grid.arrange(p1, p2, p3, p4, ncol = 2, 
                   top = "LOLOG Coefficients vs CP Fit Score")
      
      # ====== Option 2: All coefficients on one plot (faceted) ======
      # Reshape data to long format
      plot_data_long <- plot_data %>%
         pivot_longer(cols = c(edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef),
                      names_to = "coefficient",
                      values_to = "value")
      
      # Create labels for facets
      plot_data_long <- plot_data_long %>%
         mutate(coefficient = factor(coefficient,
                                     levels = c("edges_coef", "mutual_coef", "gwdegree_coef", "gwdsp_coef"),
                                     labels = c("Edges", "Mutual", "GW Degree", "GWDSP")))
      
      faceted_plot <- ggplot(plot_data_long, aes(x = cp_fit_score, y = value)) +
         geom_point(alpha = 0.5, size = 2, aes(color = coefficient)) +
         geom_smooth(method = "loess", color = "black", se = TRUE, size = 1) +
         facet_wrap(~ coefficient, scales = "free_y", ncol = 2) +
         scale_color_manual(values = c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00")) +
         labs(title = "LOLOG Coefficients vs CP Fit Score",
              x = "CP Fit Score",
              y = "Coefficient Value") +
         theme_minimal() +
         theme(legend.position = "none",
               strip.text = element_text(face = "bold", size = 11),
               strip.background = element_rect(fill = "grey90", color = NA))
      
      print(faceted_plot)
      
      
      
      
      
      
####################################################################################
      # SHANNNON DIVERISTY
####################################################################################
      
      # Adjust column names based on your actual data structure
      entity_cols <- grep("^pct_", names(network_dataset), value = TRUE)
      
      # Extract the proportion matrix
      # Each row is a network, each column is an entity type proportion
      entity_proportions <- network_dataset %>%
         select(all_of(entity_cols))
      
      # ====== 2) Calculate Shannon diversity ======
      # Shannon diversity using natural logarithm (default)
      shannon_diversity <- diversity(entity_proportions, index = "shannon")
      
      # Add Shannon diversity back to your dataset
      network_dataset$shannon_diversity <- shannon_diversity
      
      # ====== 3) Calculate other diversity metrics ======
      # Species richness (number of entity types present in core)
      species_richness <- specnumber(entity_proportions)
      network_dataset$entity_richness <- species_richness
      
      # Pielou's evenness (J) - measures how evenly distributed entity types are
      # J = H / log(S), where H is Shannon diversity and S is richness
      network_dataset$pielou_evenness <- shannon_diversity / log(species_richness)
      
      # Simpson's diversity index
      simpson_diversity <- diversity(entity_proportions, index = "simpson")
      network_dataset$simpson_diversity <- simpson_diversity
      
      # Inverse Simpson
      inv_simpson <- diversity(entity_proportions, index = "invsimpson")
      network_dataset$inv_simpson <- inv_simpson
      
      # ====== 4) Summary statistics ======
      summary(network_dataset$shannon_diversity)
      summary(network_dataset$entity_richness)
      summary(network_dataset$pielou_evenness)

      
      
      model <- lm(cp_fit_score ~ shannon_diversity + entity_richness + pielou_evenness, 
                  data = network_dataset)
      summary(model)
      
      
      #Plot
      
      cp_shannon_plot <- ggplot(network_dataset, aes(x = cp_fit_score, y = shannon_diversity)) +
         geom_point(size = 3, alpha = 0.6, color = "#1F78B4") +
         geom_smooth(method = "loess", se = TRUE, color = "black", size = 1) +
         geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.8) +
         labs(title = "Core-Periphery Fit vs Shannon Diversity of Core Actors",
              x = "CP Fit Score",
              y = "Shannon Diversity Index") +
         theme_minimal() +
         theme(plot.title = element_text(face = "bold", size = 14),
               axis.title = element_text(size = 12))
      
      print(cp_shannon_plot)
      
      
      
      data_meta$V1 <- mds_df$V1
      data_meta$V2 <- mds_df$V2
      model1 <- lm(V1 ~ mult_gsas + exante_collab, data = data_meta)
      model2 <- lm(V2 ~ mult_gsas + exante_collab, data = data_meta)
      summary(model1)
      summary(model2)
      
      data_114 = data_meta[!data_meta$gsp_id %in% c(128, 124, 117), ]
      
      model3 <- lm(cp_fit_score ~ mult_gsas + exante_collab + gwsum + local_govs_per_10k_people + fract_of_area_in_habitat_log_scaled + Perc_Bach_Degree_Over25_scaled, data = data_meta)
      summary(model3)
      
     print(names(data_meta))

      
      # ====== Extract Top 10 Node Names by Entity Type Across All Networks ======
      
      library(dplyr)
      
      # Assuming you have all_nodelists already created from your code
      # Combine all nodelists with network identifier
      all_nodes_combined <- do.call(rbind, lapply(names(all_nodelists), function(name) {
         df <- all_nodelists[[name]]
         if("entity_type" %in% names(df) && "vertex.names" %in% names(df)) {
            data.frame(
               network = name,
               node_name = df$vertex.names,
               entity_type = df$entity_type,
               stringsAsFactors = FALSE
            )
         } else if("entity_type" %in% names(df) && "name" %in% names(df)) {
            data.frame(
               network = name,
               node_name = df$name,
               entity_type = df$entity_type,
               stringsAsFactors = FALSE
            )
         } else {
            NULL
         }
      }))
      
      # Check if data was successfully combined
      if(is.null(all_nodes_combined) || nrow(all_nodes_combined) == 0) {
         cat("ERROR: No node data found. Check column names in all_nodelists.\n")
         cat("Available columns in first nodelist:\n")
         print(names(all_nodelists[[1]]))
      } else {
         
         cat("Total nodes across all networks:", nrow(all_nodes_combined), "\n")
         cat("Unique entity types:", length(unique(all_nodes_combined$entity_type)), "\n\n")
         
         # Get all unique entity types
         entity_types <- sort(unique(all_nodes_combined$entity_type))
         
         # For each entity type, find top 10 most frequent node names
         cat("========================================================================\n")
         cat("TOP 10 MOST FREQUENT NODE NAMES BY ENTITY TYPE\n")
         cat("========================================================================\n\n")
         
         # Store results in a list
         top_nodes_list <- list()
         
         for(entity in entity_types) {
            cat("\n")
            cat(strrep("=", 70), "\n")
            cat("ENTITY TYPE:", entity, "\n")
            cat(strrep("=", 70), "\n")
            
            # Filter for this entity type
            entity_data <- all_nodes_combined[all_nodes_combined$entity_type == entity, ]
            
            # Count frequency of each node name
            node_counts <- table(entity_data$node_name)
            node_counts_sorted <- sort(node_counts, decreasing = TRUE)
            
            # Get top 10
            top_10 <- head(node_counts_sorted, 10)
            
            # Create summary dataframe
            top_10_df <- data.frame(
               Rank = 1:length(top_10),
               Node_Name = names(top_10),
               Frequency = as.numeric(top_10),
               Pct_of_Entity = round(100 * as.numeric(top_10) / nrow(entity_data), 2),
               stringsAsFactors = FALSE
            )
            
            # Store in list
            top_nodes_list[[entity]] <- top_10_df
            
            # Print table
            cat("\nTotal", entity, "nodes:", nrow(entity_data), "\n")
            cat("Unique", entity, "node names:", length(unique(entity_data$node_name)), "\n\n")
            print(top_10_df, row.names = FALSE)
            
            # Visual bar chart
            cat("\nFrequency Distribution:\n")
            max_freq <- max(top_10_df$Frequency)
            for(i in 1:nrow(top_10_df)) {
               bar_length <- round(30 * top_10_df$Frequency[i] / max_freq)
               cat(sprintf("%2d. %-30s %s %d (%.1f%%)\n",
                           i,
                           substr(top_10_df$Node_Name[i], 1, 30),
                           paste(rep("█", bar_length), collapse = ""),
                           top_10_df$Frequency[i],
                           top_10_df$Pct_of_Entity[i]))
            }
         }
         
         # ====== Summary Statistics ======
         cat("\n\n")
         cat(strrep("=", 70), "\n")
         cat("SUMMARY STATISTICS\n")
         cat(strrep("=", 70), "\n\n")
         
         summary_stats <- data.frame(
            Entity_Type = character(),
            Total_Nodes = numeric(),
            Unique_Names = numeric(),
            Top_Node = character(),
            Top_Node_Freq = numeric(),
            Top_Node_Pct = numeric(),
            stringsAsFactors = FALSE
         )
         
         for(entity in entity_types) {
            entity_data <- all_nodes_combined[all_nodes_combined$entity_type == entity, ]
            top_10_df <- top_nodes_list[[entity]]
            
            summary_stats <- rbind(summary_stats, data.frame(
               Entity_Type = entity,
               Total_Nodes = nrow(entity_data),
               Unique_Names = length(unique(entity_data$node_name)),
               Top_Node = top_10_df$Node_Name[1],
               Top_Node_Freq = top_10_df$Frequency[1],
               Top_Node_Pct = top_10_df$Pct_of_Entity[1],
               stringsAsFactors = FALSE
            ))
         }
         
         # Sort by total nodes
         summary_stats <- summary_stats[order(summary_stats$Total_Nodes, decreasing = TRUE), ]
         
         cat("Summary of Top Nodes by Entity Type:\n")
         print(summary_stats, row.names = FALSE)
         
         # ====== Save Results ======
         cat("\n\nSaving results...\n")
         
         # Save combined top 10 list
         output_file <- paste0(output_dir, "top_10_nodes_by_entity_type.csv")
         
         # Combine all top 10 dataframes with entity type column
         all_top_10 <- do.call(rbind, lapply(names(top_nodes_list), function(entity) {
            df <- top_nodes_list[[entity]]
            df$Entity_Type <- entity
            df[, c("Entity_Type", "Rank", "Node_Name", "Frequency", "Pct_of_Entity")]
         }))
         
         write.csv(all_top_10, output_file, row.names = FALSE)
         cat("Top 10 nodes saved to:", output_file, "\n")
         
         # Save summary statistics
         summary_file <- paste0(output_dir, "entity_type_summary_stats.csv")
         write.csv(summary_stats, summary_file, row.names = FALSE)
         cat("Summary statistics saved to:", summary_file, "\n")
         
         # ====== Optional: Top nodes that appear in most networks ======
         cat("\n\n")
         cat(strrep("=", 70), "\n")
         cat("BONUS: NODES APPEARING IN MOST NETWORKS (BY ENTITY TYPE)\n")
         cat(strrep("=", 70), "\n")
         
         for(entity in head(entity_types, 5)) {  # Show top 5 entity types only
            cat("\n", entity, ":\n")
            cat(strrep("-", 70), "\n")
            
            entity_data <- all_nodes_combined[all_nodes_combined$entity_type == entity, ]
            
            # Count number of unique networks each node appears in
            node_network_counts <- entity_data %>%
               group_by(node_name) %>%
               summarise(
                  n_networks = n_distinct(network),
                  total_appearances = n()
               ) %>%
               arrange(desc(n_networks), desc(total_appearances)) %>%
               head(10)
            
            print(as.data.frame(node_network_counts), row.names = FALSE)
         }
      }
      
      
# CP correlation with raw network statistics
      network_raw <- read.csv ("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/network_statistics/merged_dataset_GC.csv")
           network_raw$cp_fit_score <- network_dataset$cp_fit_score      

print(names(network_raw))      



# Select relevant columns
metrics_data <- network_raw %>%
   select(
      gsp_id,  # or your network identifier
      cp_fit_score,
      num_nodes,
      num_edges,
      avg_degree,
      avg_path_length,
      degree_centralization,
      transitivity_global,
      transitivity_local,
      modularity
   ) %>%
   drop_na()

# Reshape data to long format for faceting
metrics_long <- metrics_data %>%
   pivot_longer(
      cols = c(num_nodes, num_edges, avg_degree, avg_path_length,
               degree_centralization, transitivity_global, 
               transitivity_local, modularity),
      names_to = "metric",
      values_to = "value"
   )

# Create better labels for facets
metric_labels <- c(
   "num_nodes" = "Number of Nodes",
   "num_edges" = "Number of Edges",
   "avg_degree" = "Average Degree",
   "avg_path_length" = "Average Path Length",
   "degree_centralization" = "Degree Centralization",
   "transitivity_global" = "Global Transitivity",
   "transitivity_local" = "Local Transitivity",
   "modularity" = "Modularity"
)

metrics_long$metric <- factor(metrics_long$metric,
                              levels = names(metric_labels),
                              labels = metric_labels)

# ====== Create faceted plot ======
faceted_plot <- ggplot(metrics_long, aes(x = value, y = cp_fit_score)) +
   geom_point(alpha = 0.5, size = 2, color = "#1F78B4") +
   geom_smooth(method = "loess", se = TRUE, color = "black", size = 1) +
   facet_wrap(~ metric, scales = "free_x", ncol = 3) +
   labs(
      title = "Core-Periphery Fit Score vs Network Structural Metrics",
      subtitle = sprintf("Analysis of %d networks", nrow(metrics_data)),
      x = "Metric Value",
      y = "CP Fit Score"
   ) +
   theme_minimal() +
   theme(
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "grey90", color = NA),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 11)
   )

print(faceted_plot)

# ====== Calculate correlations for labels ======
correlation_labels <- data.frame(
   metric = character(),
   label = character(),
   x_pos = numeric(),
   y_pos = numeric(),
   stringsAsFactors = FALSE
)

metrics_to_test <- c("num_nodes", "num_edges", "avg_degree", "avg_path_length",
                     "degree_centralization", "transitivity_global", 
                     "transitivity_local", "modularity")

for(metric in metrics_to_test) {
   if(metric %in% names(metrics_data)) {
      test_result <- cor.test(metrics_data$cp_fit_score, metrics_data[[metric]], 
                              method = "pearson")
      
      sig_level <- case_when(
         test_result$p.value < 0.001 ~ "***",
         test_result$p.value < 0.01 ~ "**",
         test_result$p.value < 0.05 ~ "*",
         TRUE ~ ""
      )
      
      # Create label text
      label_text <- sprintf("r = %.3f%s", test_result$estimate, sig_level)
      
      # Calculate position (top-left of each panel)
      # We'll use Inf for positioning
      correlation_labels <- rbind(correlation_labels, data.frame(
         metric = metric_labels[metric],
         label = label_text,
         x_pos = -Inf,
         y_pos = Inf,
         stringsAsFactors = FALSE
      ))
   }
}

# ====== Alternative: 2x4 grid for cleaner layout with correlation labels ======
faceted_plot_2x4 <- ggplot(metrics_long, aes(x = value, y = cp_fit_score)) +
   geom_point(alpha = 0.5, size = 2, color = "#1F78B4") +
   geom_smooth(method = "loess", se = TRUE, color = "black", size = 1) +
   geom_text(data = correlation_labels, 
             aes(x = x_pos, y = y_pos, label = label),
             hjust = -0.1, vjust = 1.5, 
             size = 3.5, fontface = "bold",
             color = "red") +
   facet_wrap(~ metric, scales = "free_x", ncol = 4, nrow = 2) +
   labs(
      title = "Core-Periphery Fit Score vs Network Structural Metrics",
      subtitle = sprintf("Analysis of %d networks (*** p<0.001, ** p<0.01, * p<0.05)", nrow(metrics_data)),
      x = "Metric Value",
      y = "CP Fit Score"
   ) +
   theme_minimal() +
   theme(
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "grey90", color = NA),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 11)
   )

print(faceted_plot_2x4)



library(ggplot2)

# ====== Histogram of CP Fit Score Distribution ======

# Calculate summary statistics
mean_cp <- mean(network_raw$cp_fit_score, na.rm = TRUE)
median_cp <- median(network_raw$cp_fit_score, na.rm = TRUE)
sd_cp <- sd(network_raw$cp_fit_score, na.rm = TRUE)

# Create histogram
cp_histogram <- ggplot(network_raw, aes(x = cp_fit_score)) +
   geom_histogram(bins = 30, fill = "#1F78B4", color = "black", alpha = 0.7) +
   geom_vline(aes(xintercept = mean_cp), 
              color = "red", linetype = "dashed", size = 1) +
   geom_vline(aes(xintercept = median_cp), 
              color = "darkgreen", linetype = "dashed", size = 1) +
   annotate("text", x = mean_cp, y = Inf, 
            label = sprintf("Mean = %.3f", mean_cp),
            vjust = 2, hjust = -0.1, color = "red", fontface = "bold") +
   annotate("text", x = median_cp, y = Inf, 
            label = sprintf("Median = %.3f", median_cp),
            vjust = 4, hjust = -0.1, color = "darkgreen", fontface = "bold") +
   labs(
      title = "Distribution of Core-Periphery Fit Scores",
      subtitle = sprintf("N = %d networks | Mean = %.3f | SD = %.3f", 
                         nrow(network_raw), mean_cp, sd_cp),
      x = "CP Fit Score",
      y = "Frequency"
   ) +
   theme_minimal() +
   theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 12)
   )

print(cp_histogram)

# ====== Alternative: Density plot ======
cp_density <- ggplot(network_raw, aes(x = cp_fit_score)) +
   geom_density(fill = "#1F78B4", alpha = 0.5, color = "black", size = 1) +
   geom_vline(aes(xintercept = mean_cp), 
              color = "red", linetype = "dashed", size = 1) +
   geom_vline(aes(xintercept = median_cp), 
              color = "darkgreen", linetype = "dashed", size = 1) +
   labs(
      title = "Density Distribution of Core-Periphery Fit Scores",
      subtitle = sprintf("N = %d networks | Mean = %.3f | SD = %.3f", 
                         nrow(network_raw), mean_cp, sd_cp),
      x = "CP Fit Score",
      y = "Density"
   ) +
   theme_minimal() +
   theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 12)
   )

print(cp_density)

# ====== Combined histogram + density ======
cp_combined <- ggplot(network_raw, aes(x = cp_fit_score)) +
   geom_histogram(aes(y = ..density..), bins = 30, 
                  fill = "#1F78B4", color = "black", alpha = 0.5) +
   geom_density(color = "red", size = 1.5) +
   geom_vline(aes(xintercept = mean_cp), 
              color = "darkred", linetype = "dashed", size = 1) +
   annotate("text", x = mean_cp, y = Inf, 
            label = sprintf("Mean = %.3f", mean_cp),
            vjust = 1.5, hjust = -0.1, color = "darkred", fontface = "bold") +
   labs(
      title = "Distribution of Core-Periphery Fit Scores",
      subtitle = sprintf("N = %d networks | Mean = %.3f | Median = %.3f | SD = %.3f", 
                         nrow(network_raw), mean_cp, median_cp, sd_cp),
      x = "CP Fit Score",
      y = "Density"
   ) +
   theme_minimal() +
   theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 12)
   )

print(cp_combined)

# ====== Summary Statistics ======
cat("\n========================================================================\n")
cat("CP FIT SCORE DISTRIBUTION STATISTICS\n")
cat("========================================================================\n\n")

cat("Sample Size:", nrow(network_raw), "networks\n\n")

cat("Central Tendency:\n")
cat("  Mean:   ", sprintf("%.4f", mean_cp), "\n")
cat("  Median: ", sprintf("%.4f", median_cp), "\n")
cat("  Mode:   ", sprintf("%.4f", as.numeric(names(sort(table(round(network_raw$cp_fit_score, 2)), 
                                                        decreasing = TRUE)[1]))), "\n\n")

cat("Dispersion:\n")
cat("  SD:     ", sprintf("%.4f", sd_cp), "\n")
cat("  Variance:", sprintf("%.4f", var(network_raw$cp_fit_score, na.rm = TRUE)), "\n")
cat("  Range:  ", sprintf("%.4f to %.4f", 
                          min(network_raw$cp_fit_score, na.rm = TRUE),
                          max(network_raw$cp_fit_score, na.rm = TRUE)), "\n")
cat("  IQR:    ", sprintf("%.4f", IQR(network_raw$cp_fit_score, na.rm = TRUE)), "\n\n")

cat("Quartiles:\n")
quantiles <- quantile(network_raw$cp_fit_score, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
cat("  Min (0%):  ", sprintf("%.4f", quantiles[1]), "\n")
cat("  Q1 (25%):  ", sprintf("%.4f", quantiles[2]), "\n")
cat("  Q2 (50%):  ", sprintf("%.4f", quantiles[3]), "\n")
cat("  Q3 (75%):  ", sprintf("%.4f", quantiles[4]), "\n")
cat("  Max (100%):", sprintf("%.4f", quantiles[5]), "\n\n")

cat("Shape:\n")
cat("  Skewness:", sprintf("%.4f", moments::skewness(network_raw$cp_fit_score, na.rm = TRUE)), "\n")
cat("  Kurtosis:", sprintf("%.4f", moments::kurtosis(network_raw$cp_fit_score, na.rm = TRUE)), "\n\n")

# Shapiro-Wilk test for normality (if sample size < 5000)
if(nrow(network_raw) < 5000) {
   shapiro_test <- shapiro.test(network_raw$cp_fit_score)
   cat("Normality Test (Shapiro-Wilk):\n")
   cat("  W =", sprintf("%.4f", shapiro_test$statistic), "\n")
   cat("  p-value =", sprintf("%.4f", shapiro_test$p.value), "\n")
   if(shapiro_test$p.value < 0.05) {
      cat("  Result: Data significantly deviates from normal distribution\n")
   } else {
      cat("  Result: Data does not significantly deviate from normal distribution\n")
   }
}

# Save plot
# ggsave("cp_fit_score_histogram.png", cp_combined, width = 10, height = 6, dpi = 300)
