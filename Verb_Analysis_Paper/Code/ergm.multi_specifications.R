
library(statnet)
library(ergm.multi)

net_list <- readRDS('data/Verb_Analysis_Paper/layered_nets.RDS')

m0 <- ergm(Networks(net_list[1:3]) ~ 
              N(~L(~edges, ~`1`)) +
              N(~L(~edges, ~`2`)) +
              N(~L(~edges, ~`3`))
)



# Uncombine each multilayer network back to its 3 layers
all_layers <- list()
for(i in 1:3) {
   layers <- uncombine_network(net_list[[i]], split.vattr = ".LayerID")
   for(j in 1:3) {
      layers[[j]] %v% "NetworkID" <- i
      layers[[j]] %v% "LayerID" <- paste(i,j,sep = '_')
      layers <- unname(layers)
   }
   all_layers <- c(all_layers, layers)
}
# Then use Layer() as above
combined <- Layer(all_layers)
# Use regular ergm with L() operators - NO Networks()!
m0 <- ergm(combined ~ 
              L(~edges, ~`1`) +      # edges in layer 1 across all networks
              L(~edges, ~`2`) +      # edges in layer 2
              L(~edges, ~`3`),
           constraints = ~blockdiag("NetworkID")
)



# Extract all individual layer networks from your multilayer networks
all_layers <- list()
layer_names <- c("layer1", "layer2", "layer3")
idx <- 1

for(i in seq_along(net_list)) {  # Using first 3 networks
   print(i)
   # Get the three layers from network i
   layers <- uncombine_network(net_list[[i]], split.vattr = ".LayerID")
   for(j in seq_along(layer_names)) {
      # Add metadata to track which network and which layer
      layers[[j]] %n% "OriginalNetwork" <- i
      layers[[j]] %n% "OriginalLayer" <- j
      all_layers[[idx]] <- layers[[j]]
      names(all_layers)[idx] <- paste0("net", i, "_", layer_names[j])
      idx <- idx + 1
   }
}

# Now create ONE big multilayer network with net x layer "layers" (networks × 3 layers)
big_multilayer <- Layer(all_layers)

# Use Layer Logic to model within vs. cross effects
m0 <- ergm(big_multilayer ~ 
              # Edges within each original layer
              L(~edges, ~`net1_layer1`) +
              L(~edges, ~`net1_layer2`) +
              # ... etc
              
              # Cross-layer within same network
              L(~edges, ~`net1_layer1` & `net1_layer2`)
)





# block-diagonal structure
block <- combine_networks(net_list[1:3])




block %v% ".BlockID" <- paste(block %v% ".NetworkID", block %v% ".LayerID", sep = "_")

ergm(block~N(~edges))
ergm(block~N(~L(~edges,~`1`)))

# The .NetworkID is now nested - flatten it
# Extract the network ID (first level)
net_ids <- sapply(get.vertex.attribute(block, ".NetworkID", unlist=FALSE), 
                  function(x) if(is.list(x)) x[[1]] else x)
# Set a proper flat block ID
block %v% "NetBlock" <- net_ids
# Now use this attribute for the block constraint
m0 <- ergm(Networks(block) ~ 
              N(~L(~edges, ~`1`)) + 
              N(~L(~edges, ~`2`)) + 
              N(~L(~edges, ~`3`)),
           constraints = ~blockdiag("NetBlock")
)


m0 <- ergm(Networks(block) ~ 
                 N(~L(~edges, ~`1`)) + 
                 N(~L(~edges, ~`2`)) + 
                 N(~L(~edges, ~`3`))
)
   

N(~L(~edges, ~`1`)) + N(edges.age.at()~L(~edges, ~`1`)) + 
   N(~L(~edges, ~`2`)) +
   N(~L(~edges, ~`3`))
# Check if blocks are contiguous
table(block %v% ".NetworkID")
head(block %v% ".NetworkID", 30)




table(get.vertex.attribute(block, ".NetworkID"))
table(get.vertex.attribute(block, ".LayerID"),get.vertex.attribute(block, ".NetworkID"))
?combine_networks
combined$

length(net_list)

f0 <- Networks(combined) ~ 
   N(~L(~edges, ~`1`)) + 
   N(~L(~edges, ~`2`)) +
   N(~L(~edges, ~`3`))
m0 <- ergm(Networks(combined) ~ 
                 N(~L(~edges, ~`1`)) + 
                 N(~L(~edges, ~`2`)) +
                 N(~L(~edges, ~`3`)))
f1 <- update.formula(f0, ~.+)

              
              
#l1 <- rgraph(n = 100,tprob = 0.05,mode = 'graph')
#l2 <- rgraph(n = 100,tprob = 0.05,mode = 'graph')
#l3 <- rgraph(n = 100,tprob = 0.05,mode = 'graph')

#net_list <- lapply(list(l1,l2,l3),as.network)

llist <- lapply(dt[1:10],function(x) {
   Layer(x,c('L1' = "1", 
             'L2' = "2", 
             'L3' = "3"))})

llist[[2]]
#mnet <- ergm.multi::combine_networks(net_list,blockID.vattr = 'layer')
control = control.ergm(main.method = "Stochastic-Approximation",MPLE.maxit = 1e3,MPLE.covariance.sim.burnin = 500,
                       MPLE.covariance.samplesize = 200,MPLE.nonident = 'warning',init.method = 'MPLE')



m0list <- lapply(llist,function(x) {
   f0 <- x ~ #density by layer
      L(~edges, ~`1`) + L(~edges, ~`2`) + L(~edges, ~`3`)
   ergm(f0,control = control)
   })

ergm(llist[[1]] ~ L(~edges, ~`1`) + L(~edges, ~`2`) + L(~edges, ~`3`))

m0 <- ergm(f0,control = control)
f1 <- update.formula(f0, ~ . + 
  # pairings of ties between layers (undirected)
  L(~edges, ~`1`&`2`) +   
  L(~edges, ~`1`&`3`) +   
  L(~edges, ~`2`&`3`),control = control)
m1 <- ergm(f1,control = control)
### NOTE:: IF DIRECTED, AND WE ACTUALLY WANT A-->B IN LAYER 1 AND B-->A IN LAYER 2
#L(~edges, ~(`1`&t(`2`))|(`2`&t(`1`))) +  
#L(~edges, ~(`1`&t(`3`))|(`3`&t(`1`))) +  
#L(~edges, ~(`2`&t(`3`))|(`3`&t(`2`)))
### also this does within-layer reciprocity if directed
#L(~mutual, ~`1`) +
#L(~mutual, ~`2`) +
#L(~mutual, ~`3`)


### NOTE THAT I WOULD USUALLY START WITH DECAY = 0 AND HEAT IT UP
### BUT DECAY = 0 CREATES NaNs here
### what this should be doing is edgewise shared partners for a an edge in layer X build by ties in any the other layers
### so like, layer 1 tie incident on A--B where there is a layer 2 or layer 3 tie from A--C and B--C
f2 <- update.formula(f1, ~ . + 
  gwespL(L.base = ~`1`, Ls.path = list(~`2`|`3`, ~`2`|`3`), decay = 1,fixed = T) +
  gwespL(L.base = ~`2`, Ls.path = list(~`1`|`3`, ~`1`|`3`), decay = 1,fixed = T) +
  gwespL(L.base = ~`3`, Ls.path = list(~`1`|`2`, ~`1`|`2`), decay = 1,fixed = T)
  )
m2 <- ergm(f2,control = control)


