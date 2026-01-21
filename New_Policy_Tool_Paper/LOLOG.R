library(statnet)
library(tidyverse)
library(data.table)
library(igraph)
library(broom)
library(ergm)
library(statnet)
library(sna)
library(lolog)
library(ggplot2)
library(patchwork)
library(corrplot)
library(dplyr)
library(factoextra)

' Data exported
write.csv(data_meta, file = output_file, row.names = FALSE, append = TRUE)'

# ln -s ~/Library/CloudStorage/Box-Box/Kings_Large_Files/data ~/Documents/GitHub/kings/
## 1. load the rds, summary network statistics
file_paths <- list.files(path = "data/network_policy_tool_paper/wendysong/cleaned_unfiltered_extracts", pattern = "*.RDS", full.names = TRUE)
file_paths <- file_paths[!grepl("0089.RDS|0053.RDS", file_paths)]
output_path <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/LOLOG/"
output_file <- paste0(output_path, "meta_data_GC.csv")

## long term, add to this function so you can specify the options (.e.g, how ot handle weights, whether to remove isolates)
rds2network <- function(network_file_path,isolates = F,loops = F,object = c('igraph','network')) {
   data <- readRDS(network_file_path)
   nodes <- data$nodelist
   edges <- data$edgelist
   ### added because loops make statnet unhappy
   ### igraph doesnt' notice/care
   if(!loops){
      edges <- edges[edges$source!=edges$target,]
   }
   # collapse and weighted
   edges_with_weights <- edges |>
      group_by(source, target) |>
      summarize(weight = n(), .groups = 'drop')
   nodelist_filtered <- nodes[, c("entity_name", "entity_type"), drop = FALSE]
   colnames(nodelist_filtered)[colnames(nodelist_filtered) == "entity_name"] <- "name"
   # drop any isolated
   if(!isolates){edges_with_weights <- na.omit(edges_with_weights)}
   all_nodes <- unique(c(edges_with_weights$source, edges_with_weights$target))
   nodelist_filtered <- nodelist_filtered[nodelist_filtered$name %in% all_nodes, ]
   if(object == 'igraph'){
      g <- graph_from_data_frame(d = edges_with_weights, vertices = nodelist_filtered, directed = TRUE)
   }
   if(object == 'network'){
      g <- network(edges_with_weights,
                   vertex.attr = nodelist_filtered,
                   directed = TRUE)
   }
   return(g)
}
all_networks <- lapply(file_paths, rds2network,
                       isolates = FALSE, loops = FALSE,
                       object   = "network")
names(all_networks) <- basename(file_paths)
all_giants <- lapply(all_networks,function(net){
   # Find connected components
   g <- intergraph::asIgraph(net)
   components_info <- components(g)
   # Identify the largest component
   largest_component_id <- which.max(components_info$csize)
   # Get the vertices belonging to the largest component
   vertices_in_giant_component <- V(g)[components_info$membership == largest_component_id]
   # Extract the subgraph (the giant component)
   giant_component_graph <- induced_subgraph(g, vertices_in_giant_component)
   # Get the edge IDs from the giant component graph
   #edge_ids_in_giant_component <- as_ids(E(giant_component_graph))
   intergraph::asNetwork(giant_component_graph)
})
library(parallel)
giant_list <- mclapply(all_giants,function(net)
{
   formula <- net ~ edges + mutual + gwdegree(1,'in') + gwdsp(0.5)
   mod <- lolog(formula,nsamp = 1e3)
   mod
},mc.cores = 8,mc.cleanup = T,mc.preschedule = F)



## Analysis 
?lolog

# LOLOG Results Analysis - Using Your Exact Structure
# Analysis for giant_component_lologs list with 117 networks

library(tidyverse)
library(broom)
library(ggplot2)
library(corrplot)
library(patchwork)

output_path <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/LOLOG/"




# 1. EXTRACT COEFFICIENTS FROM giant_component_lologs
# =============================================================================

network_statistics <- read_csv("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/network_statistics/network_statistics_GC.csv")

# Function to extract coefficients from giant_component_lologs structure
extract_giant_lologs_coeffs <- function(giant_lologs) {
   
   coeffs_df <- map_dfr(seq_along(giant_lologs), function(i) {
      model <- giant_lologs[[i]]
      network_name <- names(giant_lologs)[i]
      
      # Check if model exists and has theta
      if (!is.null(model) && "theta" %in% names(model)) {
         # Extract coefficients (theta values)
         theta_vals <- model$theta
         
         # Extract standard errors from vcov matrix if available
         if ("vcov" %in% names(model) && !is.null(model$vcov)) {
            se_vals <- sqrt(diag(model$vcov))
         } else {
            se_vals <- rep(NA, length(theta_vals))
         }
         
         # Calculate z-scores and p-values (assuming asymptotic normality)
         z_scores <- ifelse(!is.na(se_vals), theta_vals / se_vals, NA)
         p_values <- ifelse(!is.na(z_scores), 2 * (1 - pnorm(abs(z_scores))), NA)
         
         # FIXED: Convert 0007.RDS → 7 to match network_statistics format
         if (is.null(network_name)) network_name <- paste0("network_", i)
         gsp_id_clean <- gsub("\\.RDS$|\\.rds$", "", network_name)  # Remove extension
         gsp_id_clean <- as.character(as.numeric(gsp_id_clean))     # Remove leading zeros
         
         # Create data frame
         data.frame(
            gsp_id = gsp_id_clean,  # Now matches network_statistics format (e.g., "7")
            original_name = network_name,  # Keep original for reference (e.g., "0007.RDS")
            edges_coef = theta_vals[1],
            mutual_coef = theta_vals[2], 
            gwdegree_coef = theta_vals[3],
            gwdsp_coef = theta_vals[4],
            edges_se = se_vals[1],
            mutual_se = se_vals[2],
            gwdegree_se = se_vals[3], 
            gwdsp_se = se_vals[4],
            edges_pvalue = p_values[1],
            mutual_pvalue = p_values[2],
            gwdegree_pvalue = p_values[3],
            gwdsp_pvalue = p_values[4],
            # Additional model info
            obs_edges = model$obsStats[1],
            obs_mutual = model$obsStats[2],
            obs_gwdegree = model$obsStats[3],
            obs_gwdsp = model$obsStats[4],
            stringsAsFactors = FALSE
         )
      } else {
         # Handle failed/missing models
         if (is.null(network_name)) network_name <- paste0("network_", i)
         gsp_id_clean <- gsub("\\.RDS$|\\.rds$", "", network_name)
         gsp_id_clean <- as.character(as.numeric(gsp_id_clean))
         
         data.frame(
            gsp_id = gsp_id_clean,
            original_name = network_name,
            edges_coef = NA, mutual_coef = NA, gwdegree_coef = NA, gwdsp_coef = NA,
            edges_se = NA, mutual_se = NA, gwdegree_se = NA, gwdsp_se = NA,
            edges_pvalue = NA, mutual_pvalue = NA, gwdegree_pvalue = NA, gwdsp_pvalue = NA,
            obs_edges = NA, obs_mutual = NA, obs_gwdegree = NA, obs_gwdsp = NA,
            stringsAsFactors = FALSE
         )
      }
   })
   
   return(coeffs_df)
}

# Extract coefficients from giant_component_lologs
cat("Extracting coefficients from giant_component_lologs...\n")
lolog_coeffs <- extract_giant_lologs_coeffs(giant_component_lologs)

# Remove failed extractions
lolog_coeffs_clean <- lolog_coeffs[complete.cases(lolog_coeffs[c("edges_coef", "mutual_coef", "gwdegree_coef", "gwdsp_coef")]), ]

cat("Successfully extracted coefficients for", nrow(lolog_coeffs_clean), "networks\n")


# 2. MERGE WITH NETWORK_STATISTICS
# =============================================================================

# Merge giant_component_lologs coefficients with network_statistics by gsp_id
cat("\nMerging with network_statistics...\n")

# Check if network_statistics exists
if (!exists("network_statistics")) {
   stop("network_statistics dataframe not found. Please load it first.")
}

# Show matching examples for verification
cat("Sample matching:\n")
cat("giant_component_lologs names (first 5):", head(names(giant_component_lologs), 5), "\n")
cat("After removing .RDS:", head(gsub("\\.RDS$|\\.rds$", "", names(giant_component_lologs)), 5), "\n")
cat("After removing leading zeros:", head(as.character(as.numeric(gsub("\\.RDS$|\\.rds$", "", names(giant_component_lologs)))), 5), "\n")
cat("network_statistics gsp_id (first 5):", head(network_statistics$gsp_id, 5), "\n")

# Merge the datasets
merged_data <- merge(lolog_coeffs_clean, network_statistics, by = "gsp_id", all.x = TRUE)

# Remove rows without cp_fit_score
merged_clean <- merged_data[!is.na(merged_data$cp_fit_score), ]
cat("\nFinal analysis dataset:", nrow(merged_clean), "networks\n")

merged_clean <- merged_clean %>%
   arrange(as.numeric(gsp_id))

# 3. CORRELATION ANALYSIS
# =============================================================================

# Select variables for correlation analysis
correlation_vars <- merged_clean %>%
   select(edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef, cp_fit_score)

# Calculate correlation matrix
cor_matrix <- cor(correlation_vars, use = "complete.obs")

cat("\n=== CORRELATION MATRIX ===\n")
print(round(cor_matrix, 3))

# Extract specific correlations with cp_fit_score
cp_correlations <- data.frame(
   Coefficient = c("Edges", "Mutual", "GW-Degree", "GW-DSP"),
   Correlation_with_CP = cor_matrix[1:4, "cp_fit_score"],
   stringsAsFactors = FALSE
) %>%
   arrange(desc(abs(Correlation_with_CP)))

cat("\n=== CORRELATIONS WITH CORE-PERIPHERY FIT SCORE ===\n")
print(cp_correlations)

# 4. COEFFICIENT DISTRIBUTION
# =============================================================================


# CREATE INDIVIDUAL FREQUENCY PLOTS

# Plot 1: Edges coefficient
p1 <- ggplot(merged_clean, aes(x = edges_coef)) +
   geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
   labs(title = "Edges Coefficient Distribution",
        x = "Edges Coefficient",
        y = "Frequency") +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot 2: Mutual coefficient
p2 <- ggplot(merged_clean, aes(x = mutual_coef)) +
   geom_histogram(bins = 20, fill = "darkgreen", alpha = 0.7, color = "white") +
   labs(title = "Mutual Coefficient Distribution",
        x = "Mutual Coefficient", 
        y = "Frequency") +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot 3: GW-Degree coefficient
p3 <- ggplot(merged_clean, aes(x = gwdegree_coef)) +
   geom_histogram(bins = 20, fill = "orange", alpha = 0.7, color = "white") +
   labs(title = "GW-Degree Coefficient Distribution",
        x = "GW-Degree Coefficient",
        y = "Frequency") +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot 4: GW-DSP coefficient
p4 <- ggplot(merged_clean, aes(x = gwdsp_coef)) +
   geom_histogram(bins = 20, fill = "purple", alpha = 0.7, color = "white") +
   labs(title = "GW-DSP Coefficient Distribution",
        x = "GW-DSP Coefficient",
        y = "Frequency") +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Arrange plots in a 2x2 grid
combined_plot <- (p1 | p2) / (p3 | p4)

print(combined_plot)



## SEGMENT PLOTS INCLUDING SE
names(data)



library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1. Reshape: wide  ➜  long  -------------------------------------------
long_coef <- data %>%                                     
  select(gsp_id,
         edges_coef,   edges_se,
         mutual_coef,  mutual_se,
         gwdegree_coef, gwdegree_se,
         gwdsp_coef,   gwdsp_se) %>%                      
  pivot_longer(-gsp_id,
               names_to  = c("term", ".value"),
               names_pattern = "(.*)_(coef|se)") %>%       
  filter(!is.na(coef)) %>%                                
  group_by(term) %>% 
  arrange(coef, .by_group = TRUE) %>%
  mutate(rank = row_number(),                              
         lo   = coef - se,                                
         hi   = coef + se)  %>%                               
ungroup() %>% 
   mutate(term = factor(term, levels = c("edges", "mutual", "gwdegree", "gwdsp")))

# ---- 2. Segment plot -------------------------------------------------------
ggplot(long_coef,
       aes(y = rank)) +
  geom_segment(aes(x = lo, xend = hi, yend = rank),
               linewidth = 0.6, colour = "steelblue") +    
  geom_point(aes(x = coef), size = 1.2, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey50") +
  facet_wrap(~ term, scales = "free_x") +                  
  labs(title = "Coefficient Distribution Across 117 Networks",
       subtitle = "Bars show estimate ± 1 standard error",
       x = "Coefficient value", y = NULL) +
  theme_classic(base_size = 12) +
  theme(panel.spacing.x = unit(1.5, "lines"),
        strip.text = element_text(face = "bold"),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())


















#===============================================================================





# SUMMARY STATISTICS

summary_stats <- merged_clean %>%
   select(edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef) %>%
   summarise_all(list(
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
   )) %>%
   pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
   separate(stat, into = c("coefficient", "statistic"), sep = "_coef_") %>%
   pivot_wider(names_from = statistic, values_from = value)

cat("Summary Statistics for Coefficients:\n")
print(summary_stats)



# 5. REDUCED DIMENSION
# =============================================================================

## 5.1 CLUSTERING
# =============================================================================

library(cluster)
data <- merged_clean
names(data)

clustering_vars <- data %>%
   select(edges_coef, mutual_coef, gwdegree_coef, 
          gwdsp_coef)

clustering_vars_scaled <- scale(clustering_vars)
dist_matrix <- dist(clustering_vars_scaled)
hclust_result <- hclust(dist_matrix, method = "ward.D2")
labels <- data$gsp_id
plot(hclust_result, labels = labels, main = "Dendrogram with GSP IDs",
     xlab = "", sub = "", cex = 0.4)
rect.hclust(hclust_result, k = num_clusters, border = "red")


# Calculate silhouette scores for different numbers of clusters
sil_width <- sapply(2:10, function(k) {
   cutree(hclust_result, k = k) %>%  # Cut the dendrogram into k clusters
      silhouette(dist_matrix) %>%
      summary() %>%
      .$avg.width
})
plot(2:10, sil_width, type = "b", xlab = "Number of Clusters", ylab = "Silhouette Width", main = "Silhouette Method")

# selected # of cluster
num_clusters <- 3
clusters <- cutree(hclust_result, k = num_clusters)
data$cluster <- as.factor(clusters)
cluster_summary <- data %>%
   group_by(cluster) %>%
   summarise(
      edges = mean(edges_coef, na.rm = TRUE),
      mutual = mean(mutual_coef, na.rm = TRUE),
      gwdegree = mean(gwdegree_coef, na.rm = TRUE),
      gwdsp = mean(gwdsp_coef, na.rm = TRUE),
   )
print(cluster_summary)


model <- lm(cp_fit_score ~ cluster, data = data)
summary(model)


model <- lm(PC2 ~ mult_gsas + exante_collab, data = data_meta)
summary(model)

model 


## 5.2 PCA
# =============================================================================


pca_result <- prcomp(clustering_vars)
eigenvalues <- pca_result$sdev ^ 2

print(eigenvalues)

summary(pca_result)

# Add PCA scores to the data
data$PC1 <- pca_result$x[, 1]
data$PC2 <- pca_result$x[, 2]
#data$PC3 <- pca_result$x[, 3]

eigen_summary <- data.frame(
   PC         = paste0("PC", seq_along(eigenvalues)),
   Eigenvalue = eigenvalues,
)

# 2D plot of observations distribution - dimension reduction PC1, PC2
ggplot(data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
   geom_point(size = 3) +
   labs(title = "Clusters in 2D PCA Space", x = "PC1", y = "PC2", color = "Cluster")


# PCA Correlation
pca_scores <- pca_result$x
combined_data <- cbind(pca_scores, clustering_vars)
correlation_matrix <- cor(combined_data)

pc1_correlations <- correlation_matrix["PC1", colnames(clustering_vars)]
pc2_correlations <- correlation_matrix["PC2", colnames(clustering_vars)]
#pc3_correlations <- correlation_matrix["PC3", colnames(clustering_vars)]


combined_correlations <- data.frame(
   PC1_Correlation = pc1_correlations,
   PC2_Correlation = pc2_correlations
   #PC3_Correlation = pc3_correlations
)
print(combined_correlations)


model <- lm(cp_fit_score ~ PC1 + PC2, data = data)
summary(model)




# 5.3. COSINE SIMILARITY 
# =============================================================================

coefficient_data <- data %>%
   select(gsp_id, edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef) %>%
   filter(complete.cases(.)) 
coeff_matrix <- as.matrix(coefficient_data[, -1])  
rownames(coeff_matrix) <- coefficient_data$gsp_id

# Calculate cosine similarity matrix
cosine_similarity <- function(x, y) {
   dot_product <- sum(x * y)
   norm_x <- sqrt(sum(x^2))
   norm_y <- sqrt(sum(y^2))
   
   if (norm_x == 0 || norm_y == 0) {
      return(0)
   } else {
      return(dot_product / (norm_x * norm_y))
   }
}

calculate_cosine_matrix <- function(data_matrix) {
   n <- nrow(data_matrix)
   cosine_matrix <- matrix(0, nrow = n, ncol = n)
   rownames(cosine_matrix) <- rownames(data_matrix)
   colnames(cosine_matrix) <- rownames(data_matrix)
   
   for (i in 1:n) {
      for (j in 1:n) {
         cosine_matrix[i, j] <- cosine_similarity(data_matrix[i, ], data_matrix[j, ])
      }
   }
   return(cosine_matrix)
}

cosine_matrix <- calculate_cosine_matrix(coeff_matrix)

# Now GROUPING ....
## pending discussion
dist_matrix <- 1- cosine_matrix
hc <- hclust(as.dist(dist_matrix), method = "average")  
plot(hc, main = "Hierarchical Clustering Dendrogram",
     xlab = "Network GSP ID", ylab = "1 - Cosine Similarity",
     sub = "", cex = 0.6)
groups <- cutree(hc, k = 4)



# CONNECT AGENCIES ATTRIBUTES
#===============================================================================

# Merge dataset
meta_data <- read.csv("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/gsp_ids_with_metadata.csv")
print(names(meta_data))
# mult_gsas,priority,urbangw_af,percent_dac_by_pop_scaled,fract_of_area_in_habitat,maxdryspell_scaled,Agr_Share_Of_GDP_scaled,Perc_Bach_Degree_Over25_scaled, Perc_Bach_Degree_Over25_scaled,basin_population, Republican_Vote_Share_scaled
data_meta <- data %>%
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


# ====== 1) Prep PCA data  ======
df_clean <- data_meta %>%
   select(
      edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef,  # PCA features
      exante_collab,  mult_gsas,                           # coloring var
      gsp_id                                               
   ) %>%
   drop_na()

# Matrix for PCA
X   <- df_clean %>% select(edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef)
grp <- df_clean$exante_collab
grp <- factor(df_clean$exante_collab, levels = c(0, 1), labels = c("No", "Yes"))


grp1 <- df_clean$mult_gsas
grp1 <- factor(df_clean$mult_gsas, levels = c(0, 1), labels = c("No", "Yes"))

# ====== 2) PCA (STHDA recommends prcomp + scale.=TRUE) ======
res.pca <- prcomp(X, scale. = TRUE)

# ====== 3) Eigenvalues / variance explained ======
eigenvalues <- res.pca$sdev ^ 2
prop_var    <- eigenvalues / sum(eigenvalues)
cum_var     <- cumsum(prop_var)

eigen_table <- data.frame(
   PC                  = paste0("PC", seq_along(eigenvalues)),
   Eigenvalue          = eigenvalues,
   Prop_Variance       = prop_var,
   Cumulative_Variance = cum_var
)
print(eigen_table, row.names = FALSE, digits = 3)

# Optional scree (eigenvalues) plot
fviz_eig(res.pca, addlabels = TRUE)

# ====== 4) Individuals plot, colored by exante_collab ======
# exante_collab is categorical:
fviz_pca_ind(
   res.pca,
   geom        = "point",
   habillage   = grp1,                  # color by factor groups
   addEllipses = TRUE, ellipse.type = "confidence",
   palette     = c("#00AFBB", "#FC4E07"),  # two colors
   repel       = TRUE,
   legend.title = "mult_agencies"
)

?ellipse.type






# SUMMARY BY GSA ATTRIBUTES
# ==============================================================================
data <- data_meta
coeff_cols <- c("edges_coef","mutual_coef","gwdegree_coef","gwdsp_coef")

# Table by mult_gsa (0/1)
tbl_mult_gsa <- data %>%
   group_by(mult_gsas) %>%
   summarise(
      across(all_of(coeff_cols),
             list(mean = ~mean(., na.rm = TRUE),
                  se   = ~sd(.,   na.rm = TRUE) / sqrt(sum(!is.na(.)))),
             .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
   ) %>%
   pivot_longer(-mult_gsas,
                names_to = c("term", ".value"),
                names_pattern = "(.*)_(mean|se)") %>%
   mutate(`mean±SE` = sprintf("%.3f ± %.3f", mean, se)) %>%
   select(mult_gsas, term, `mean±SE`) %>%
   pivot_wider(names_from = term, values_from = `mean±SE`) %>%
   arrange(mult_gsas)

# Table by exante_collab (0/1)
tbl_exante_collab <- data %>%
   group_by(exante_collab) %>%
   summarise(
      across(all_of(coeff_cols),
             list(mean = ~mean(., na.rm = TRUE),
                  se   = ~sd(.,   na.rm = TRUE) / sqrt(sum(!is.na(.)))),
             .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
   ) %>%
   pivot_longer(-exante_collab,
                names_to = c("term", ".value"),
                names_pattern = "(.*)_(mean|se)") %>%
   mutate(`mean±SE` = sprintf("%.3f ± %.3f", mean, se)) %>%
   select(exante_collab, term, `mean±SE`) %>%
   pivot_wider(names_from = term, values_from = `mean±SE`) %>%
   arrange(exante_collab)

print(tbl_mult_gsa)
print(tbl_exante_collab)




### MULTIDEMENSIONAL 
##============================================================================== 

library(dplyr)
library(cluster)
library(ggpubr)
library(ggrepel)

data_meta <- read.csv("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/LOLOG/meta_data_GC.csv")

# ====== Data Preparation ======
data_meta <- data_meta %>%
   mutate(mult_gsas = ifelse(mult_gsas == TRUE, 1, 0))
data_meta <- data_meta %>% 
   mutate(exante_collab = ifelse(exante_collab == TRUE, 1, 0))

# ====== 1) Prep MDS data ======
df_clean <- data_meta %>%
   select(
      edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef, cp_fit_score, # MDS features
      exante_collab, mult_gsas,                            # grouping variables
      gsp_id                                               
   ) 

# Matrix for MDS (following GeeksforGeeks pattern)
X <- df_clean %>% select(edges_coef, mutual_coef, gwdegree_coef, gwdsp_coef)

# ====== 2) Perform MDS analysis ======
mds_result <- cmdscale(dist(X))

# ====== 3) Form clusters using K-means clustering ======
kmeans_clusters <- kmeans(mds_result, centers = 3)$cluster

# ====== 4) Add cluster information to the MDS results ======
mds_df <- as.data.frame(mds_result)
mds_df$groups <- as.factor(kmeans_clusters)
mds_df$mult_gsas <- as.factor(ifelse(df_clean$mult_gsas == 1, "Yes", "No"))
mds_df$exante_collab <- as.factor(ifelse(df_clean$exante_collab == 1, "Yes", "No"))
mds_df$gsp_id <- df_clean$gsp_id
mds_df$cp_fit_score <- df_clean$cp_fit_score

# ====== 5) Plot using ggscatter with K-means clusters ======
ggscatter(mds_df, x = "V1", y = "V2",
          color = "groups",
          palette = "jco",
          size = 3,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
   geom_text_repel(aes(label = gsp_id), box.padding = 0.5)

# ====== 6) Plot colored by mult_gsas ======
ggscatter(mds_df, x = "V1", y = "V2",
          color = "mult_gsas",
          palette = c("#00AFBB", "#FC4E07"),
          size = 3,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "MDS Data - Multiple GSAs",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
   geom_text_repel(aes(label = gsp_id), box.padding = 0.5)

# ====== 7) Plot colored by exante_collab ======
ggscatter(mds_df, x = "V1", y = "V2",
          color = "exante_collab",
          palette = c("#00AFBB", "#FC4E07"),
          size = 3,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "MDS Data - Ex Ante Collaboration",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
   geom_text_repel(aes(label = gsp_id), box.padding = 0.5)


# ====== Understanding MDS Dimensions ======
# Calculate correlations between MDS dimensions and original variables
correlations <- cor(mds_df[, c("V1", "V2", "cp_fit_score")], df_clean[, c("edges_coef", "mutual_coef", "gwdegree_coef", "gwdsp_coef")])
print("Correlations between MDS dimensions and original coefficients:")
print(round(correlations, 3))

# Plot V1 vs V2
v1_v2_cor <- cor(mds_df$V2, mds_df$V1)
cat(sprintf("\nCorrelation between MDS Dim 1 and MDS Dim 2: r = %+.3f\n", v1_v2_cor))

v1_v2_plot <- ggplot(mds_df, aes(x = V2, y = V1)) +
   geom_smooth(method = "loess", color = "black", se = TRUE, size = 1) +
   labs(title = "MDS Dimension 1 vs Dimension 2",
        subtitle = sprintf("Correlation: r = %.3f", v1_v2_cor),
        x = "MDS Dimension 2",
        y = "MDS Dimension 1") +
   theme_minimal() +
   theme(panel.grid.minor = element_blank())

print(v1_v2_plot)

###############################################################################
# Create plot with two lines
###############################################################################

# Reshape data for plotting both dimensions as lines
mds_long <- mds_df %>%
   select(V1, V2, cp_fit_score) %>%
   pivot_longer(cols = c(V1, V2), names_to = "Dimension", values_to = "MDS_Value")

# Calculate correlations for subtitle
cor_v1_cp <- cor(mds_df$V1, mds_df$cp_fit_score)
cor_v2_cp <- cor(mds_df$V2, mds_df$cp_fit_score)

cat(sprintf("\nCorrelation between CP Fit Score and MDS Dim 1: r = %+.3f\n", cor_v1_cp))
cat(sprintf("Correlation between CP Fit Score and MDS Dim 2: r = %+.3f\n", cor_v2_cp))

# Create plot with two lines
cp_mds_plot <- ggplot(mds_long, aes(x = MDS_Value, y = cp_fit_score, color = Dimension)) +
   geom_point(alpha = 0.5, size = 2) +
   geom_smooth(method = "loess", se = TRUE, size = 1.2) +
   scale_color_manual(values = c("V1" = "#E31A1C", "V2" = "#1F78B4"),
                      labels = c("V1" = "MDS Dimension 1", "V2" = "MDS Dimension 2")) +
   labs(title = "CP Fit Score vs MDS Dimensions",
        subtitle = sprintf("Correlations: Dim 1 (r = %.3f), Dim 2 (r = %.3f)", cor_v1_cp, cor_v2_cp),
        x = "MDS Dimension Value",
        y = "CP Fit Score") +
   theme_minimal() +
   theme(legend.position = "right")

print(cp_mds_plot)

# Create biplot
biplot <- ggplot(mds_df, aes(x = V1, y = V2)) +
   geom_point(aes(color = groups), size = 3, alpha = 0.7) +
   geom_segment(data = arrow_data, 
                aes(x = 0, y = 0, xend = x, yend = y),
                arrow = arrow(length = unit(0.3, "cm")), 
                color = "red", size = 1) +
   geom_text(data = arrow_data, 
             aes(x = x * 1.1, y = y * 1.1, label = variable),
             color = "red", size = 3, fontface = "bold") +
   scale_color_manual(values = c("#E31A1C", "#1F78B4", "#33A02C")) +
   labs(title = "MDS Biplot: Points and Variable Loadings",
        subtitle = "Arrows show direction of original variables in MDS space",
        x = "MDS Dimension 1", 
        y = "MDS Dimension 2",
        color = "K-means\nClusters") +
   theme_minimal() +
   theme(panel.grid.minor = element_blank())

print(biplot)

# ====== 10) Compare clusters with existing groupings ======
cluster_comparison <- table(kmeans_clusters, df_clean$mult_gsas)
print("\nK-means clusters vs mult_gsas:")
print(cluster_comparison)

cluster_comparison2 <- table(kmeans_clusters, df_clean$exante_collab)
print("K-means clusters vs exante_collab:")
print(cluster_comparison2)