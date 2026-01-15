
library(statnet)
library(ergm.multi)
layered_nets <- readRDS("data/Verb_Analysis_Paper/layered_nets.RDS")

#not going to use this table to specify different models for different plans
#summdf <- readRDS("data/Verb_Analysis_Paper/reciprocal_edge_summary_by_plan.RDS")

#the 35th plan is short, use this as test case
#k = 35
mod0_saved = 'data/Verb_Analysis_Paper/model_0_results.RDS'
mod1_saved = 'data/Verb_Analysis_Paper/model_1_results.RDS'
mod2_saved = 'data/Verb_Analysis_Paper/model_2_results.RDS'
if(file.exists(mod0_saved)){m0s <- readRDS(mod0_saved)}else{m0s <- vector(mode = "list", length = length(layered_nets))}
if(file.exists(mod1_saved)){m1s <- readRDS(mod1_saved)}else{m1s <- vector(mode = "list", length = length(layered_nets))}
if(file.exists(mod2_saved)){m2s <- readRDS(mod2_saved)}else{m2s <- vector(mode = "list", length = length(layered_nets))}

#control = control.ergm(main.method = "Stochastic-Approximation",MPLE.maxit = 1e3,MPLE.covariance.sim.burnin = 500,
#                       MPLE.covariance.samplesize = 200,MPLE.nonident = 'warning',init.method = 'MPLE')
control = control.ergm(parallel = 96,MPLE.samplesize = 100e3,MCMC.burnin = 100e3,MCMC.interval = 5e3,MCMC.samplesize = 100e3,
                       #### relaxing tolerance
                       MCMLE.conv.min.pval = 0.2)

f0 <- layered_nets[[k]] ~
   #density by layer
   L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)
f1 <- update.formula(f0, ~ . + 
                        # pairings of ties between layers (undirected)
                        L(~edges, ~`info`& ( `money` | `neither`)) +  
                        L(~edges, ~`money`& (`info` | `neither`)),control = control)
f2 <- update.formula(f1, ~ . + 
                        gwespL(L.base = ~`info`, Ls.path = list(~`money`|`neither`|`info`, ~`money`|`neither`|`info`), decay = 1,fixed = T) +
                        gwespL(L.base = ~`money`, Ls.path = list(~`info`|`neither`|`money`, ~`info`|`neither`|`money`), decay = 1,fixed = T)  )


if(all(sapply(m0s,class)=='ergm')){print('all base models have finished')}else{
   for(k in 1:length(layered_nets)){
      if(is.null(m0s[[k]]))
         print(k)
         {
      #mnet <- ergm.multi::combine_networks(net_list,blockID.vattr = 'layer')
         m0s[[k]] <- ergm(f0,control = control)
         saveRDS(m0s,mod0_saved)
         }
   }
}   
   
if(all(sapply(m1s,class)=='ergm')){print('all reciprocity models have finished')}else{  
   #if(summdf[k,"info_and_neither"] > 0 | summdf[k,"money_and_info"] > 0){} ALWAYS TRUE WITH OUR DATA
   #not going to invoke logic for reciprocity terms after all
   #if(summdf[k, "money_and_info"] > 0 | summdf[k, "money_and_neither"] > 0){
   #ok to add recip_money
   #}
   for(k in 1:length(layered_nets)){
      if(is.null(m1s[[k]])){
         print(k)
         m1s[[k]] <- tryCatch(ergm(f1,control = control),error = function(e) NULL)
         saveRDS(m1s,mod1_saved)
      }
   }
}
      ### NOTE THAT I WOULD USUALLY START WITH DECAY = 0 AND HEAT IT UP
      ### BUT DECAY = 0 CREATES NaNs here
      ### what this should be doing is edgewise shared partners for a an edge in layer X build by ties in any the other layers
      ### so like, layer 1 tie incident on A--B where there is a layer 2 or layer 3 tie from A--C and B--C
if(all(sapply(m2s,class)=='ergm')){print('all transitivity models have finished')}else{  
   for(k in 1:length(layered_nets)){
      if(is.null(m2s[[k]])){
         print(k)
         m2s[[k]] <- tryCatch(ergm(f2,control = control),error = function(e) NULL)
         saveRDS(m2s,mod2_saved)
      }
   }
}




