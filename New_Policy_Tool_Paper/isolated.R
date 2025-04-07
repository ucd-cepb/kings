# March 2024
# whole network vs non-isolated network vs great component
# collapse then weighted 
# correlation on network statistics
# CFA: Confirmatory Factor Analysis 
# policy instrument: allocation, trading, pumping restriction, taxes, effective incentives 

# Library
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

############################## import the network #########################
################### isolated graph #########################
file_paths <- list.files(path = "/Users/wendysong/Documents/Davis/PhD research/cleaned_extracts", pattern = "*.RDS", full.names = TRUE)
file_paths <- file_paths[!grepl("0089.RDS|0053.RDS", file_paths)]
# the try on box/git 
output_folder <- "New_Policy_Tool_Paper/Network_policy_tool_paper/PhD_research/network_policy/graphs"


for (i in 1:length(file_paths)) {
  
  file_name <- basename(file_paths[i])
  graph_title <- paste("Graph", gsub(".RDS", "", file_name))
  output_file <- file.path(output_folder, paste0( gsub(".RDS$", "", file_name), ".png"))
  
  
  data <- readRDS(file_paths[i])
  nodes <- data$nodelist
  edges <- data$edgelist
  
  # drop isolated node
  edge_list_filtered <- edges[, c("source", "target")]
  
  nodelist_filtered <- nodes[, c("entity_name", "entity_type", "num_appearances"), drop = FALSE]
  colnames(nodelist_filtered)[colnames(nodelist_filtered) == "entity_name"] <- "name"
 
  # non-isolated   
  na_edges <- edge_list_filtered[is.na(edge_list_filtered$source) | is.na(edge_list_filtered$target), ]
  edge_list_filtered <- na.omit(edge_list_filtered)
  
  all_nodes <- unique(c(edge_list_filtered$source, edge_list_filtered$target))
  nodelist_filtered <- nodelist_filtered[nodelist_filtered$name %in% all_nodes, ]
  
  g <- graph_from_data_frame(d = edge_list_filtered, vertices = nodelist_filtered, directed = TRUE)
  
  V(g)$color <- ifelse(V(g)$entity_type == "GPE", "#C3B1E1",  
                       ifelse(V(g)$entity_type == "ORG", "#A2D5C6",  
                              ifelse(V(g)$entity_type == "PERSON", "#FFCBA4",  
                                            "#D3D3D3")))  
    V(g)$size <- 3  
  
     ## export
   png(output_file)
  
   plot(g, 
       vertex.label = NA,          
       vertex.color = V(g)$color,  
       vertex.size = V(g)$size,    
       vertex.frame.color = NA,    
       edge.arrow.size = 0.05,     
       edge.curved = 0.1,         
       edge.color = "gray",       
       edge.width = 0.5,           
       main = graph_title) 
  
  legend("topleft", 
         legend = c("GPE", "ORG", "PERSON"), 
         col = c("#C3B1E1", "#A2D5C6", "#FFCBA4"),  
         pch = 19,                
         pt.cex = 1,              
         cex = 0.8,
         text.width = 0.5,
         bty = "n")              

  dev.off()
}


















############## non-isolated network statistics #################
#num_nodes, num_edges, avg_degree, avg_path_length, 
#centralization, transitivity(local, global), modularity

file_paths <- list.files(path = "/Users/wendysong/Documents/Davis/PhD research/cleaned_extracts", pattern = "*.RDS", full.names = TRUE)
file_paths <- file_paths[!grepl("0089.RDS|0053.RDS", file_paths)]
output_folder_statistics <- "/Users/wendysong/Documents/Davis/PhD research/network_policy/"
output_csv_path <- paste0(output_folder_statistics, "network_statistics_isolated.csv")
merged_dataset_path <- paste0(output_folder_statistics, "merged_dataset_isolated.csv")

## weighted demo
# edges_with_weights <- edge_list_filtered %>%
#  group_by(source, target) %>%
#  summarize(weight = n(), .groups = 'drop')
#print(edges_with_weights)
###################################################   pass

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
  
  # extract the gsp id
  file_name <- basename(file_paths[i])
  gsp_id <- as.numeric(stringr::str_extract(file_name, "\\d+"))
  
  ################# Weighted ######################
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
  
  # append
  all_network_stats <- rbind(all_network_stats, network_stats)
}

# Write the combined network statistics to CSV
write.csv(all_network_stats, file = output_csv_path, row.names = FALSE, append = TRUE)


# ------------------------ merge with policy instrument ----------------------------
policy_data <- read.csv("/Users/wendysong/Documents/Davis/PhD research/policy_by_plan.csv")
policy_data[, 2:6] <- lapply(policy_data[, 2:6], function(x) ifelse(x == 2, 1, ifelse(x == 1, 0.5, 0)))
# ???????? consider trading as saperate, pending
policy_data$policy_index <- rowSums(policy_data[, 2:6])
policy_data$maybe_total <- rowSums(policy_data[, 2:6] == 0.5)

network_statistics <- read.csv("/Users/wendysong/Documents/Davis/PhD research/network_policy/network_statistics_isolated.csv") 
merged_dataset <- merge(network_statistics, policy_data, by = "gsp_id", all.x = TRUE)
print(names(merged_dataset))       
write.csv(merged_dataset, file = merged_dataset_path, row.names = FALSE, append = TRUE)

# ------------------------ merge with meta_data --------------------------------------

meta_data <- read.csv("/Users/wendysong/Documents/Davis/PhD research/gsp_ids_with_metadata.csv")
print(names(meta_data))
# mult_gsas,priority,urbangw_af,percent_dac_by_pop_scaled,fract_of_area_in_habitat,maxdryspell_scaled,Agr_Share_Of_GDP_scaled,Perc_Bach_Degree_Over25_scaled, Perc_Bach_Degree_Over25_scaled,basin_population, Republican_Vote_Share_scaled
data_meta <- merged_dataset %>%
  left_join(meta_data %>% select(gsp_id,mult_gsas,priority,urbangw_af,percent_dac_by_pop_scaled,fract_of_area_in_habitat,
                                 maxdryspell_scaled,Agr_Share_Of_GDP_scaled,Perc_Bach_Degree_Over25_scaled, Perc_Bach_Degree_Over25_scaled,
                                 basin_population, Republican_Vote_Share_scaled, exante_collab
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





##### correlation #######

stats_for_corr <- data[, c("num_nodes", "num_edges", "avg_degree", 
                                        "avg_path_length", "degree_centralization", 
                                        "transitivity_global", "transitivity_local", 
                                        "modularity")]
correlation_matrix <- cor(stats_for_corr, use = "complete.obs")
cor_melt <- melt(correlation_matrix)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  
  geom_text(aes(label = round(value, 2)), size = 3) +  
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Correlation") +  
  theme_minimal() +  
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 6),  
    legend.title = element_text(size = 8),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 16, face = "bold")  
  ) +
  labs(title = "Isolated Heatmap(weighted)", x = "", y = "")


##---------------------- PCA  ---------------------
##### cluster #####
clustering_vars <- data %>%
  select(num_nodes, num_edges, avg_degree, 
         avg_path_length, degree_centralization, 
         transitivity_global, transitivity_local, 
         modularity)
clustering_vars_scaled <- scale(clustering_vars)
dist_matrix <- dist(clustering_vars_scaled)
hclust_result <- hclust(dist_matrix, method = "ward.D2")
labels <- data$gsp_id
plot(hclust_result, labels = labels, main = "Dendrogram with GSP IDs",
     xlab = "", sub = "", cex = 0.7)

# Calculate silhouette scores for different numbers of clusters
sil_width <- sapply(2:10, function(k) {
  cutree(hclust_result, k = k) %>%  # Cut the dendrogram into k clusters
    silhouette(dist_matrix) %>%
    summary() %>%
    .$avg.width
})
plot(2:10, sil_width, type = "b", xlab = "Number of Clusters", ylab = "Silhouette Width", main = "Silhouette Method")

# selected #of cluster
num_clusters <- 3
clusters <- cutree(hclust_result, k = num_clusters)
data$cluster <- as.factor(clusters)
cluster_summary <- data %>%
  group_by(cluster) %>%
  summarise(
    num_nodes = mean(num_nodes, na.rm = TRUE),
    num_edges = mean(num_edges, na.rm = TRUE),
    avg_degree = mean(avg_degree, na.rm = TRUE),
    avg_path_length = mean(avg_path_length, na.rm = TRUE),
    centralization = mean(degree_centralization, na.rm = TRUE),
    transitivity_global = mean(transitivity_global, na.rm = TRUE),
    transitivity_local = mean(transitivity_local, na.rm = TRUE),
    modularity = mean(modularity, na.rm = TRUE)
  )
print(cluster_summary)

## --- Testing for Significant Differences Between Clusters ---

test_vars <- names(clustering_vars)
# ANOVA (parametric test)
for (var in test_vars) {
  formula <- as.formula(paste(var, "~ cluster"))
  aov_result <- aov(formula, data = all_network_stats)
  cat("\nANOVA for", var, ":\n")
  print(summary(aov_result))
}


#######  regression
models <- lapply(c("Allocations","Trading", "Taxes.Fees", "Pumping.Restrictions", "Efficiency.Incentives"), function(y) {
  glm(as.formula(paste(y, "~ cluster")), 
      data = data, 
      family = binomial)
})
lapply(models, summary)
# ------------------------- probably not, no significant -----------------------

###### ----------------- PCA
pca_result <- prcomp(clustering_vars, scale = TRUE)

# Add PCA scores to the data
data$PC1 <- pca_result$x[, 1]
data$PC2 <- pca_result$x[, 2]
data$PC3 <- pca_result$x[, 3]


# 2D plot
ggplot(data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point(size = 3) +
  labs(title = "Clusters in 2D PCA Space", x = "PC1", y = "PC2", color = "Cluster")

# PCA Correlation
pca_scores <- pca_result$x
combined_data <- cbind(pca_scores, clustering_vars)
correlation_matrix <- cor(combined_data)

pc1_correlations <- correlation_matrix["PC1", colnames(clustering_vars)]
pc2_correlations <- correlation_matrix["PC2", colnames(clustering_vars)]
pc3_correlations <- correlation_matrix["PC3", colnames(clustering_vars)]

combined_correlations <- data.frame(
  PC1_Correlation = pc1_correlations,
  PC2_Correlation = pc2_correlations,
  PC3_Correlation = pc3_correlations
)
print(combined_correlations)



# Logistic regression for each dependent variable
models <- lapply(c("Allocations", "Trading","Taxes.Fees", "Pumping.Restrictions", "Efficiency.Incentives"), function(y) {
  glm(as.formula(paste(y, "~ PC1+PC2+PC3")), 
      data = data, 
      family = binomial)
})
lapply(models, summary)

meta_model <- lm(Pumping.Restrictions ~ mult_gsas + priority + urbangw_af +
                   percent_dac_by_pop_scaled + fract_of_area_in_habitat +
                   maxdryspell_scaled + Agr_Share_Of_GDP_scaled + 
                   Perc_Bach_Degree_Over25_scaled + basin_population + 
                   Republican_Vote_Share_scaled + exante_collab + PC1+PC2+PC3, data = data)
summary(meta_model)

meta_model <- glm(Efficiency.Incentives ~ mult_gsas + priority + urbangw_af +
                    percent_dac_by_pop_scaled + fract_of_area_in_habitat +
                    maxdryspell_scaled + Agr_Share_Of_GDP_scaled +
                    Perc_Bach_Degree_Over25_scaled + basin_population +
                    Republican_Vote_Share_scaled + exante_collab +
                    PC1 + PC2 + PC3,
                  data = data,
                  family = binomial(link = "logit"))

summary(meta_model)


## Confirmatory factor analysis 

model <- '
  Connectivity =~ avg_degree + avg_path_length
  Clustering =~ modularity + transitivity_global + transitivity_local

  # Structural model (regress dependent variables on latent factors)
  Allocations ~ num_nodes + num_edges + Connectivity + degree_centralization + Clustering
  Trading ~ num_nodes + num_edges + Connectivity + degree_centralization + Clustering
  Taxes.Fees ~ num_nodes + num_edges + Connectivity + degree_centralization + Clustering
  Pumping.Restrictions ~ num_nodes + num_edges + Connectivity + degree_centralization + Clustering
  Efficiency.Incentives ~ num_nodes + num_edges + Connectivity + degree_centralization + Clustering
'
fit <- sem(model, data = data)
lavInspect(fit, "cov.lv")
summary(fit, fit.measures = TRUE, standardized = TRUE)









