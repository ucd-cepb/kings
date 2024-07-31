library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(ggcorrplot)
library(knitr)
library(broom)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/Policy_Instruments_Paper/cleaned_extracts_PIP")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

policies <- read_csv("Policy_Instruments_Paper/Data/policy_instruments_bruno.csv")

replace_letters <- function(x) {
   x <- gsub('Y', 1, x)
   x <- gsub('M', 1, x)
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

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- gsp_ids[g]
   
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
      net_reciprocity = migraph::network_reciprocity(net),
      net_transitivity = migraph::network_transitivity(net),
      net_assortativity = migraph::network_assortativity(net),
      net_tri = sum(igraph::triangles(net)),
      net_eccentricity = max(igraph::eccentricity(net))
   )
   gsp_summary <- rbind(gsp_summary,gsp_stats)
   
   igraph::as_data_frame(net, what = 'vertices') %>% 
      filter(org_type == 'GSA' & GSA == 0)
   
   # remove isolates
   gnet <- delete.vertices(net, which(degree(net) == 0))
   
   graph <- ggraph(gnet, layout = 'fr') +
      geom_edge_link() +
      geom_node_point(aes(color = org_type, size = degree, shape = as.factor(GSA))) +
      theme_void() +
      ggtitle(paste0("GSP ", gsp_id))+
      theme_graph(background='white')
   
   ggsave(paste0("Policy_Instruments_Paper/Graphs/gsp_", gsp_id, ".png"), graph)
}

gsp_summary <- tibble(gsp_summary)
gsp_summary

merged <- gsp_summary %>% 
   left_join(policies_clean, by = c("gsp_id" = "GSP_ID")) %>% 
   mutate(across(everything(), ~ as.numeric(.)))

summary(merged)

# mods across all dependent variables
for (var in names(merged)[4:16]){
   model <- glm(reformulate(var, response = 'allocations'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in names(merged)[4:16]){
   model <- glm(reformulate(var, response = 'trading'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in names(merged)[4:16]){
   model <- glm(reformulate(var, response = 'taxes_fees'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in names(merged)[4:16]){
   model <- glm(reformulate(var, response = 'pumping_restrictions'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in names(merged)[4:16]){
   model <- glm(reformulate(var, response = 'efficiency_incentives'), 
                data = merged, 
                family = binomial)
   print(summary(model))}

for (var in names(merged)[4:16]){
   model <- glm(reformulate(var, response = 'any'), 
                data = merged, 
                family = binomial)
   print(summary(model))}
