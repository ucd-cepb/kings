layered_nets <- readRDS("data/Verb_Analysis_Paper/layered_nets.RDS")
library(statnet)
library(ergm.multi)

filekey <- read.csv("filekey.csv")

edgesnodes <- list.files(path = filekey[filekey$var_name=="disambiged_unfiltered_extracts_superpaper",]$filepath, full.names = T)
gspids <- stringr::str_extract(edgesnodes,'[0-9]{1,}')
gspids <- gspids[!gspids%in% c("0053", "0089")]

#not going to use this table to specify different models for different plans
#summdf <- readRDS("data/Verb_Analysis_Paper/reciprocal_edge_summary_by_plan.RDS")

#the 35th plan is short, use this as test case
#k = 35

m0s <- vector(mode = "list", length = 117)
m1s <- vector(mode = "list", length = 117)
m2s <- vector(mode = "list", length = 117)

for(k in 1:length(layered_nets)){
   #mnet <- ergm.multi::combine_networks(net_list,blockID.vattr = 'layer')
   
   control = control.ergm(main.method = "Stochastic-Approximation",MPLE.maxit = 1e3,MPLE.covariance.sim.burnin = 500,
                          MPLE.covariance.samplesize = 200,MPLE.nonident = 'warning',init.method = 'MPLE')
   f0 <- layered_nets[[k]] ~
      #density by layer
      L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)
   
   m0s[[k]] <- ergm(f0,control = control)
   saveRDS(m0s[[k]], paste0("m0models_",gspids[k],".RDS"))
   #if(summdf[k,"info_and_neither"] > 0 | summdf[k,"money_and_info"] > 0){} ALWAYS TRUE WITH OUR DATA
   
   #not going to invoke logic for reciprocity terms after all
   #if(summdf[k, "money_and_info"] > 0 | summdf[k, "money_and_neither"] > 0){
      #ok to add recip_money
   #}
   
   f1 <- update.formula(f0, ~ . + 
                           # pairings of ties between layers (undirected)
                           L(~edges, ~`info`& ( `money` | `neither`)) +  
                           L(~edges, ~`money`& (`info` | `neither`)),control = control)
   m1s[[k]] <- ergm(f1,control = control)
   saveRDS(m1s[[k]], paste0("m1models_",gspids[k],".RDS"))
   
   
   ### NOTE THAT I WOULD USUALLY START WITH DECAY = 0 AND HEAT IT UP
   ### BUT DECAY = 0 CREATES NaNs here
   ### what this should be doing is edgewise shared partners for a an edge in layer X build by ties in any the other layers
   ### so like, layer 1 tie incident on A--B where there is a layer 2 or layer 3 tie from A--C and B--C
   f2 <- update.formula(f1, ~ . + 
                           gwespL(L.base = ~`info`, Ls.path = list(~`money`|`neither`|`info`, ~`money`|`neither`|`info`), decay = 1,fixed = T) +
                           gwespL(L.base = ~`money`, Ls.path = list(~`info`|`neither`|`money`, ~`info`|`neither`|`money`), decay = 1,fixed = T)  )
   m2s[[k]] <- ergm(f2,control = control)
   
   summary(m2s[[k]])
   saveRDS(m2s[[k]], paste0("m2models_",gspids[k],".RDS"))
}


saveRDS(m2s, "sample_of_fullmodels.RDS")
saveRDS(m1s, "sample_of_m1_models.RDS")
saveRDS(m0s, "sample_of_m0_models.RDS")

conflist <- vector(mode = "list", length = 3)
library(data.table)
for(k in 1:3){
   conftable <- data.frame(confint(sample_of_fullmodels[[k]]))
   conflist[[k]] <- data.frame(
      term = names(sample_of_fullmodels[[k]]$coefficients),
      coefficients = sample_of_fullmodels[[k]]$coefficients, 
              low_CI_bound = conftable$X2.5..,
              high_CI_bound = conftable$X97.5..,
              gspid = gspids[k])
   
   
   
}
fullconflist <- rbindlist(conflist)

library(ggplot2)
for(j in 1:length(unique(fullconflist$term))){
   termlist <- fullconflist[term==unique(fullconflist$term)[j],]
   termlist$index <- 1:nrow(termlist)
   g <- ggplot(termlist, aes(x = index, y = coefficients)) + 
      geom_point() + geom_errorbar(aes(ymin = low_CI_bound, ymax = high_CI_bound), width = 0.1) + labs(title = unique(fullconflist$term)[j]) + theme_classic()
   ggsave(paste0("coefficientplot_", j, ".png"), g)
}
 