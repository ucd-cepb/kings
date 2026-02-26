# June 2025
# for Tyler ERGMs
# Network Cleaning, Network Statistics, Core_periphery
# data = Network statistics + cp_fit_score + policy instruments + SES + GSA + Other policy instrument index(total_maybe, sum policy index)

library(statnet)
library(tidyverse)
library(data.table)
library(broom)
library(igraph)

# ln -s ~/Library/CloudStorage/Box-Box/Kings_Large_Files/data ~/Documents/GitHub/kings/
## 1. load the rds, summary network statistics

file_paths <- list.files(path = "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/cleaned_unfiltered_extracts", pattern = "*.RDS", full.names = TRUE)
file_paths <- file_paths[!grepl("0089.RDS|0053.RDS", file_paths)]


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


all_networks <- lapply(file_paths[1:10],rds2network,isolates = F,object = 'network',loops = F)

names(all_networks) <- file_paths[1:10]
### this runs count statistic summary on network objects

stats_list <- lapply(all_networks, function(x) {
   summary(x~edges + mutual + transitive) })

   
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


model_estimates <- mapply(function(x,y) tidy(x) |> mutate(network = y),
                          x = model_list,y = basename(names(all_networks)),SIMPLIFY = F)

estimate_dt <- rbindlist(model_estimates,use.names = T)


# Do ERGM on 7, 87, 127 
file_0007 <- file_paths[basename(file_paths) == "0007.RDS"]
network_0007 <- rds2network(file_0007, isolates = FALSE, object = 'network', loops = FALSE)
summary(network_0007 ~ edges + mutual + transitive)

model_0007 <- ergm(network_0007 ~ edges + mutual +  gwidegree(0,fixed = T), #+ gwesp(0.3,fixed = TRUE),   
                   eval.loglik = FALSE,
                   control = control.ergm(
                      MCMC.interval = 1000,
                      MCMLE.conv.min.pval = 0.25,
                      MCMC.burnin = 10000,
                      MCMC.samplesize = 1000
                   ))


'model_0007 <- ergm(network_0007 ~ edges + mutual +  gwidegree(0.25,fixed = T) + gwesp(0.1,fixed = TRUE),  
                   eval.loglik = FALSE,
                   control = control.ergm(
                      MCMC.interval = 5000,
                      MCMLE.conv.min.pval = 0.25,
                      MCMC.burnin = 30000,
                      MCMC.samplesize = 5000,
                      MCMLE.density.guard = 100
                   ))
?? MCMLE.density.guard'

estimate_0007 <- tidy(model_0007) %>%
   mutate(network = "0007.RDS")

print(estimate_0007)

## ERGM Fail, now try LOLOG
file_0065 <- file_paths[basename(file_paths) == "0065.RDS"]
network_0065 <- rds2network(file_0065, isolates = FALSE, object = 'network', loops = FALSE)

### Tyler code
library(parallel)
library(doParallel)
# Set up parallel processing
n_cores <- detectCores() - 1
n_cores <- 31
# Create cluster
cl <- makeCluster(n_cores)
registerDoParallel(cl)


lg <- lolog(network_0065 ~ edges() +
               mutual() +
               gwdegree(0.2, direction="in") +
               gwesp(0.2),nsamp = 10e3,
            cluster = cl)
summary(lg)
# Check gof on (in + out) degree distribution
lg_gof <- gofit(lg, network_0065~ degree(0:50) +  esp(0:25),nsim = 5e3)
plot(lg_gof)


### baseline check
lg_base <- lolog(network_0065 ~ edges() +
                    mutual(),
                 cluster = cl)
summary(lg_base)



### 2D decay mining
gwdegree_decays <- c(0.5, 1.0, 1.5, 2.0)
gwesp_decays <- c(0.5, 1.0, 1.5, 2.0)


results <- list()

for (deg in gwdegree_decays) {
   for (esp in gwesp_decays) {
      cat(sprintf("Trying gwdegree=%.2f, gwesp=%.2f\n", deg, esp))
      key <- sprintf("gwdegree_%.2f_gwesp_%.2f", deg, esp)
      result <- tryCatch({
         fit <- lolog(network_0065 ~ edges() +
                         mutual() +
                         gwdegree(deg, direction = "in") +
                         gwesp(esp),
                      nsamp = 10000,
                      cluster = cl)
         cat(sprintf("✓ Success at gwdegree=%.2f, gwesp=%.2f\n", deg, esp))
         list(success = TRUE, summary = summary(fit))
      }, error = function(e) {
         cat(sprintf("✗ Failed at gwdegree=%.2f, gwesp=%.2f → %s\n", deg, esp, e$message))
         list(success = FALSE, error = e$message)
      })
      results[[key]] <- result
      cat("\n")
   }
}

result065 <- results
successful <- Filter(function(x) x$success, result065)
names(successful)

# extract coef of one particular key
key <- "gwdegree_1.00_gwesp_0.50" ## mark 1
result065[[key]]$summary

## Trouble shooting: Degree distribution 
deg_dist <- table(sna::degree(network_0065, gmode = "digraph", cmode = "indegree"))
barplot(deg_dist, main="In-Degree Distribution", xlab="In-Degree", ylab="Number of Nodes")

