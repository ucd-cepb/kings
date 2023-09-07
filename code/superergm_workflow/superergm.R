library(igraph)
library(pbapply)
library(statnet)
library(Rglpk)
library(broom)
library(data.table)
library(dplyr)
library(Bergm)
library(bayesplot)

super <- readRDS("data_output/supernetwork_weighted")
super <- subgraph(super, V(super)[
   vertex_attr(super,"entity_type") %in% c("ORG","PERSON")])
supersimpl <- igraph::simplify(super, remove.multiple = F, remove.loops = T)

supersimpl <- get.data.frame(supersimpl, what="both")
supersimpl <- network(x=supersimpl$edges[,1:2], directed = T,
                  hyper = F, loops = F, multiple = F, 
                  bipartiate = F, vertices = supersimpl$vertices,
                  matrix.type = "edgelist")

#TODO subset actual entities from list of water agencies or by hand or something
#before running ergm on the supersimpl network, because otherwise
#there are too many nodes, which leads to vector memory issues
#TODO include node scope, eg federal, state, etc. by sending a list of entities with
#known scopes through the "disambiguate" function from textNet so the entity names associated with 
#their respective entity scope matches the format of the entities in the network
set.seed(80)
superergm <- Bergm::bergm(supersimpl~ edges + isolates +
                             gwidegree(decay=0.25, fixed = T) + 
                             gwesp(decay=0.15, fixed = T,cutoff=30) + 
                             nodemix("entity_type") + nodecov("num_GSPs_in"),
                          prior.mean = c(0, 0, 0, 0, 0, 0, 0, 0),
                          prior.sigma = diag(10,8),
                          gamma = .6, #changes the acceptance rate
                          burn.in = 1e3,
                          aux.iters = 2500,
                          main.iters = 3000,
                          nchains = 6, 
                          set.seed = 700,
                          verbose = F)
saveRDS(superergm, "data_output/superergm")



