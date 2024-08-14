library(ggraph)
library(dotenv)
library(igraph)
library(tidyverse)
library(migraph)
library(randomForest)
library(knitr)
library(broom)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/Policy_Instruments_Paper/cleaned_extracts_PIP")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

gsp_meta <- read_csv(paste0(Sys.getenv("BOX_PATH"), "/Structural_Topic_Model_Paper/gsp_ids_with_metadata.csv")) %>% 
   select(c(priority, gwsum, GSP.ID)) %>%
   mutate(
      priority_High = if_else(priority == 'High', 1, 0),
      priority_Medium = if_else(priority == 'Medium', 1, 0),
      priority_Low = if_else(priority == 'Low', 1, 0),
      priority_Very_Low = if_else(priority == 'Very Low', 1, 0),
      gwsum_0 = if_else(gwsum == 0, 1, 0),
      gwsum_1 = if_else(gwsum == 1, 1, 0),
      gwsum_2 = if_else(gwsum == 2, 1, 0),
      gwsum_3 = if_else(gwsum == 3, 1, 0)
   )%>% 
   select(-c(priority, gwsum))

replace_letters <- function(x) { 
   x <- gsub('Y', 1, x)
   x <- gsub('M', 0, x)
   x <- gsub('N', 0, x)
   return(x)
}

policies <- read_csv("Policy_Instruments_Paper/Data/policy_instruments_bruno.csv") 

pol_cols <- c('allocations', 'taxes', 'pumping_restrictions',  
              'efficiency_incentives') 

policies_clean <- policies %>% 
   select(c('GSP_ID', 'Allocations', 'Taxes/Fees', 
            'Pumping Restrictions', 'Efficiency Incentives')
   ) %>%
   set_names(c('GSP_ID', 'allocations', 'taxes', 
               'pumping_restrictions', 'efficiency_incentives')
   ) %>%
   mutate(across(everything(), ~ replace_letters(.)),
          across(everything(), ~ as.numeric(.))
   ) %>% 
   mutate(any = ifelse(rowSums(select(., all_of(pol_cols))) > 0, 1, 0),
          all = rowSums(select(., all_of(pol_cols)))/4,
          carrot = (allocations + efficiency_incentives)/2,
          stick = (pumping_restrictions + taxes)/2,
          all_w = (2*allocations + 3*taxes + 4*pumping_restrictions + efficiency_incentives)/10,
          strict = (pumping_restrictions + taxes - allocations - efficiency_incentives+2)/4
   )

gsp_summary <- data.frame()
nets <- list()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- gsp_ids[g]
   nets[[gsp_id]] <- net
   
   net <- set_vertex_attr(net,
                          'core',
                          value = node_core(net))
   corenet <- induced_subgraph(net, V(net)[core == 1])
   
   orgnet <- induced_subgraph(net, V(net)[!is.na(org_type)])
   orgnet_noi <- delete.vertices(orgnet, which(degree(orgnet) == 0))
   
   orgnet_gsa_deg <- sum(igraph::degree(orgnet_noi, 
                                        v=V(orgnet_noi)[GSA == 1], 
                                        mode = 'all'))
   
   corenet_gsa_deg <- sum(igraph::degree(corenet, 
                                         v=V(corenet)[GSA == 1], 
                                         mode = 'all'))
   
   net_gsa_deg <- sum(igraph::degree(net,
                                     v=V(net)[GSA == 1],
                                     mode = 'all'))
   
   gsp_stats <- data.frame(
      gsp_id = as.numeric(gsp_id),
      n= vcount(net),
      m = ecount(net),
      net_density = migraph::network_density(net),
      net_diameter = migraph::network_diameter(net),
      net_components = migraph::network_components(net),
      net_degree = migraph::network_degree(net),
      net_betweenness = migraph::network_betweenness(net),
      net_eigenvector = migraph::network_eigenvector(net),
      net_core = migraph::network_core(net),
      net_fact = migraph::network_factions(net),
      net_reciprocity = migraph::network_reciprocity(net),
      net_transitivity = migraph::network_transitivity(net),
      net_assortativity = migraph::network_assortativity(net),
      net_tri = sum(igraph::triangles(net)),
      net_eccentricity = max(igraph::eccentricity(net)),
      net_gsa_deg = net_gsa_deg / ecount(net),
      
      # cnet_density = migraph::network_density(corenet),
      # cnet_diameter = migraph::network_diameter(corenet),
      # cnet_components = migraph::network_components(corenet),
      # cnet_degree = migraph::network_degree(corenet),
      # cnet_betweenness = migraph::network_betweenness(corenet),
      # cnet_eigenvector = migraph::network_eigenvector(corenet),
      # cnet_fact = migraph::network_factions(corenet),
      # cnet_reciprocity = migraph::network_reciprocity(corenet),
      # cnet_transitivity = migraph::network_transitivity(corenet),
      # cnet_assortativity = migraph::network_assortativity(corenet),
      # cnet_tri = sum(igraph::triangles(corenet)),
      # cnet_eccentricity = max(igraph::eccentricity(corenet)),
      # cnet_gsa_deg =  corenet_gsa_deg / ecount(corenet),
      
      orgnet_density = migraph::network_density(orgnet_noi),
      orgnet_diameter = migraph::network_diameter(orgnet_noi),
      orgnet_components = migraph::network_components(orgnet_noi),
      orgnet_degree = migraph::network_degree(orgnet_noi),
      orgnet_betweenness = migraph::network_betweenness(orgnet_noi),
      orgnet_eigenvector = migraph::network_eigenvector(orgnet_noi),
      orgnet_fact = migraph::network_factions(orgnet_noi),
      orgnet_reciprocity = migraph::network_reciprocity(orgnet_noi),
      orgnet_transitivity = migraph::network_transitivity(orgnet_noi),
      orgnet_assortativity = migraph::network_assortativity(orgnet_noi),
      orgnet_tri = sum(igraph::triangles(orgnet_noi)),
      orgnet_eccentricity = max(igraph::eccentricity(orgnet_noi)),
      orgnet_gsa_deg = orgnet_gsa_deg / ecount(orgnet_noi),
      
      org_types = length(unique(V(net)$org_type))
   )
   gsp_summary <- rbind(gsp_summary,gsp_stats)
   
   # remove isolates
   
   # net_noi <- delete.vertices(net, which(degree(net) == 0))
   
   # graph <- ggraph(net_noi, layout = 'fr') +
   #    geom_edge_link() +
   #    geom_node_point(aes(color = org_type, size = degree)) +
   #    theme_void() +
   #    ggtitle(paste0("GSP ", gsp_id))+
   #    theme_graph(background='white')+
   #    scale_color_discrete(name = "Organization Type") +
   #    scale_size_continuous(name = "Degree")
   
   # ggsave(paste0("Policy_Instruments_Paper/Graphs/gsp_", gsp_id, ".png"),
   #        graph,
   #        width = 9,
   #        height = 9,
   #        units = 'in')
}

gsp_summary <- tibble(gsp_summary)

merged <- gsp_summary %>% 
   left_join(gsp_meta, by = c("gsp_id" = "GSP.ID")) %>%
   right_join(policies_clean, by = c("gsp_id" = "GSP_ID")) %>% 
   mutate(across(everything(), ~ as.numeric(.)))

dep_vars <- names(merged)[2:31]; dep_vars
indep_vars <- names(merged)[40:49]; indep_vars

mod_results <- list()
mod_results_sig <- list()

# Fit multivariate models for each variable in dep_vars including the base_vars


# Function to rename and select columns
rename_and_select <- function(df, name) {
   df %>%
      rename(!!name := estimate) %>%
      select(var, !!sym(name))
}


for (ivar in indep_vars) {
   ivar_df <- data.frame(var = character(), 
                         estimate = numeric(),
                         p_val = numeric())

   for (dvar in dep_vars) {
      formula <- reformulate(dvar, response = ivar)
      model <- glm(formula, data = merged, family = binomial)
      model_summary <- summary(model)
      row_add <- data.frame(var = dvar,
                            estimate = model_summary$coefficients[2,1],
                            p_val = model_summary$coefficients[2,4])
      ivar_df <- rbind(ivar_df, row_add)
   }
   
   ivar_df_sig <- ivar_df %>% 
      mutate(estimate = ifelse(p_val < 0.05, estimate, NA)) %>% 
      select(-p_val)
   
   
   mod_results[[ivar]] <- ivar_df
   mod_results_sig[[ivar]] <- ivar_df_sig
}

# Apply the function and cbind the results
mod_results_sig <- mod_results_sig %>%
   imap(rename_and_select) %>%
   reduce(left_join, by = c("var"))

mod_results_sig_pes <- mod_results_sig

mod_results_sig_op
mod_results_sig_neut
mod_results_sig_pes


dep_vars <- names(merged)[31:44]; dep_vars

merged_rf <- merged %>% 
   mutate(across(c(allocations,
                   taxes,
                   pumping_restrictions,
                   efficiency_incentives,
                   any,
                   all,
                   carrot,
                   stick,
                   all_w), ~ as.factor(.)))

formula <- as.formula(paste('any', "~", paste(dep_vars, collapse = " + ")))

# Fit the random forest model using the constructed formula
rfmod <- randomForest(formula, data = merged_rf, 
                      ntree = 5000, 
                      mtry = 10,
                      importance = TRUE); rfmod
summary(rfmod)

