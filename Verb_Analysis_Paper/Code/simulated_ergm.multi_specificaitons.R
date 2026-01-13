
library(statnet)
library(ergm.multi)

l1 <- rgraph(n = 100,tprob = 0.05,mode = 'graph')
l2 <- rgraph(n = 100,tprob = 0.05,mode = 'graph')
l3 <- rgraph(n = 100,tprob = 0.05,mode = 'graph')

net_list <- lapply(list(l1,l2,l3),as.network)
mnet <- Layer(net_list)
#mnet <- ergm.multi::combine_networks(net_list,blockID.vattr = 'layer')

control = control.ergm(main.method = "Stochastic-Approximation",MPLE.maxit = 1e3,MPLE.covariance.sim.burnin = 500,
                       MPLE.covariance.samplesize = 200,MPLE.nonident = 'warning',init.method = 'MPLE')
f0 <- mnet ~
  #density by layer
  L(~edges, ~`1`) + L(~edges, ~`2`) + L(~edges, ~`3`)

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


