library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(knitr)
library(broom)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/Policy_Instruments_Paper/cleaned_extracts_PIP")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

policies <- read_csv("Policy_Instruments_Paper/Data/policy_instruments_bruno.csv")

replace_letters <- function(x) { 
   x <- gsub('Y', 1, x)
   x <- gsub('M', 0.5, x)
   x <- gsub('N', 0, x)
   return(x)
}

letter_cols <- c('Allocations', 'Trading', 'Taxes/Fees', 'Pumping Restrictions', 
                 'Efficiency Incentives') 

policies_clean <- policies %>% 
   select(GSP_ID, all_of(letter_cols)) %>%
   mutate(across(all_of(letter_cols), ~ replace_letters(.)),
          across(all_of(letter_cols), ~ as.numeric(.))) %>% 
   mutate(any = ifelse(rowSums(select(., all_of(letter_cols))) > 0, 1, 0))

colnames(policies_clean) <- c('GSP_ID', 'allocations', 'trading', 'taxes_fees', 
                              'pumping_restrictions', 'efficiency_incentives', 'any')

gsp_summary <- data.frame()
nets <- list()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   
   net <- set_vertex_attr(net,
                          'core',
                          value = node_core(net))
   
   gsp_id <- gsp_ids[g]
   nets[[gsp_id]] <- net
   
   snet <- induced_subgraph(net, V(net)[GSA == 1])
   corenet <- induced_subgraph(net, V(net)[core == 1])
   govnet <- V(net)[V(net)$org_type %in% c('CA_Gov', 'Loc_Gov', 'GSA', 'NL_Gov', 'Spec_Dist')]
   
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
      net_cent_bet = igraph::centr_betw(net)$centralization,
      net_cent_deg = igraph::centr_degree(net)$centralization,
      net_cent_eig = igraph::centr_eigen(net)$centralization,
      
      cnet_density = migraph::network_density(corenet),
      cnet_diameter = migraph::network_diameter(corenet),
      cnet_components = migraph::network_components(corenet),
      cnet_degree = migraph::network_degree(corenet),
      cnet_betweenness = migraph::network_betweenness(corenet),
      cnet_eigenvector = migraph::network_eigenvector(corenet),
      cnet_fact = migraph::network_factions(corenet),
      cnet_reciprocity = migraph::network_reciprocity(corenet),
      cnet_transitivity = migraph::network_transitivity(corenet),
      cnet_assortativity = migraph::network_assortativity(corenet),
      cnet_tri = sum(igraph::triangles(corenet)),
      cnet_eccentricity = max(igraph::eccentricity(corenet)),
      cnet_cent_clo = igraph::centr_clo(corenet)$centralization,
      
      org_types = length(unique(V(net)$org_type)),
      gsa_deg = sum(degree(net, v=govnet, mode='out')) / ecount(net)
   )
   gsp_summary <- rbind(gsp_summary,gsp_stats)
   
   # remove isolates
   gnet <- delete.vertices(net, which(degree(net) == 0))
   
   # graph <- ggraph(corenet, layout = 'fr') +
   #    geom_edge_link() +
   #    geom_node_point(aes(color = org_type, size = degree, shape = as.factor(GSA))) +
   #    theme_void() +
   #    ggtitle(paste0("GSP ", gsp_id))+
   #    theme_graph(background='white')
   
   # ggsave(paste0("Policy_Instruments_Paper/Graphs/gsp_", gsp_id, ".png"),
   #        graph,
   #        width = 9,
   #        height = 9,
   #        units = 'in')
}

gsp_summary <- tibble(gsp_summary)
gsp_summary

merged <- gsp_summary %>% 
   left_join(policies_clean, by = c("gsp_id" = "GSP_ID")) %>% 
   mutate(across(everything(), ~ as.numeric(.)))

summary(merged)

dep_vars <- names(merged)[2:34]; dep_vars

# mods across all dependent variables
for (var in dep_vars){
   model <- glm(reformulate(var, response = 'allocations'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in dep_vars){
   model <- glm(reformulate(var, response = 'trading'), 
                data = merged %>% filter(allocations==1), 
                family = binomial)
   print(summary(model))}

for (var in dep_vars){
   model <- glm(reformulate(var, response = 'taxes_fees'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in dep_vars){
   model <- glm(reformulate(var, response = 'pumping_restrictions'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in dep_vars){
   model <- glm(reformulate(var, response = 'efficiency_incentives'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in dep_vars){
   model <- glm(reformulate(var, response = 'any'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

