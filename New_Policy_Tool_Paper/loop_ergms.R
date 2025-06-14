# June 2025
# for Tyler ERGMs
# Network Cleaning, Network Statistics, Core_periphery
# data = Network statistics + cp_fit_score + policy instruments + SES + GSA + Other policy instrument index(total_maybe, sum policy index)

library(statnet)
library(tidyverse)
library(data.table)
# ln -s ~/Library/CloudStorage/Box-Box/Kings_Large_Files/data ~/Documents/GitHub/kings/
## 1. load the rds, summary network statistics

file_paths <- list.files(path = "data/Network_policy_tool_paper/wendysong/cleaned_unfiltered_extracts", pattern = "*.RDS", full.names = TRUE)
file_paths <- file_paths[!grepl("0089.RDS|0053.RDS", file_paths)]


## long term, add to this function so you can specify the options (.e.g, how ot handle weights, wehter to remove isolates)
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
library(igraph)


all_networks <- lapply(file_paths[1:10],rds2network,isolates = F,object = 'network',loops = F)

names(all_networks) <- file_paths[1:10]
### this runs count statistic summary on network objects
stats_list <- lapply(all_networks, function(x) {
   summary(x~edges + mutual + transitive)
   }

   
### this fits ERGM...
### not that ERGM fitting can get really out of hand quickly
### exogeneous predictors are fast, but structure count terms beyond edges slow things down
### and bigger risk is that same specification won't work for all graphs
### current code assumes same specification, but we can iterate on this
   
model_list <- lapply(seq_along(all_networks), function(x) {
      print(basename(names(all_networks))[x])
      ergm(all_networks[[x]]~edges + mutual + gwidegree(0.25,fixed = T),
           eval.loglik = F,control = control.ergm(MCMC.interval = 1000,
                                                  MCMLE.conv.min.pval = 0.25,
                                                  MCMC.burnin = 10000,
                                                  MCMC.samplesize = 1000))
   }
)

library(broom)

model_estimates <- mapply(function(x,y) tidy(x) |> mutate(network = y),
                          x = model_list,y = basename(names(all_networks)),SIMPLIFY = F)

estimate_dt <- rbindlist(model_estimates,use.names = T)




