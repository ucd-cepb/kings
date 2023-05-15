library(igraph)
library(pbapply)
library(statnet)
flist <- list.files('data/directed_network_objects/networks_collapsed_to_weighted/',full.names = T)

nlist <- pblapply(flist,function(x) {
   g=readRDS(x);n <- intergraph::asNetwork(g);n})

erg_list <- pblapply(seq_along(nlist), function(n) {#print(n);
tryCatch(ergm(nlist[[n]]~ edges + 
                # factor('entity_type',levels = 1:3) + 
               #nodemix("entity_type",levels2=c(-1,-5,-9)),verbose = F),
         nodemix("entity_type"),verbose = F),
         error = function(e) NULL)},cl = 8)

coef_list <- lapply(seq_along(erg_list),function(x) 
   tidy(erg_list[[x]],conf.level = 0.95,conf.int = T) %>% mutate(net = x))

coef_dt <- rbindlist(coef_list,use.names = T)

library(ggthemes)

mix_dt<-coef_dt[grepl('mix',term),]
mix_dt$term <- str_remove_all(mix_dt$term,'mix\\.entity_type\\.')
mix_dt$term <- str_replace_all(mix_dt$term,"\\.",'-')
mixcomp <- dcast(mix_dt,net ~ term, value.var = 'estimate')


library(GGally)

intercept_pairs <- ggpairs(factcomp[,-c('net')],
       # columnLabels = c("Miles","Displacement","Horsepower",
       #                  "Rear axle ratio","Weight","1/4 mile time"),
       upper = list(continuous = wrap('points', size = 0.5,pch = 19,alpha = 0.6)),
       lower = NULL,#list(NA),#combo = wrap("facethist", bins = 30)),
       diag = list(continuous = wrap("barDiag")),
        title = "Scatterplot matrix of tie probabilities by entity type, ERGM") +
    theme_bw()

ggsave(file = 'figures/mix_estimate.png',plot = intercept_pairs,units = 'in',dpi = 450,height = 7,width= 7)





erg_list <- pblapply(seq_along(nlist), function(n) {#print(n);
   tryCatch(ergm(nlist[[n]]~ #edges + 
                    factor('entity_type',levels = 1:3) + 
                    nodemix("entity_type",levels2=c(-1,-5,-9)),verbose = F),
            error = function(e) NULL)},cl = 8)

coef_list <- lapply(seq_along(erg_list),function(x) 
   tidy(erg_list[[x]],conf.level = 0.95,conf.int = T) %>% mutate(net = x))

coef_dt <- rbindlist(coef_list,use.names = T)

library(ggthemes)

facttemp <- coef_dt[grepl('fact',term),]
facttemp$term <- str_remove_all(facttemp$term,'.*nodefactor\\.')
facttemp$term <- str_replace_all(facttemp$term,"entity_type\\.",'')
factcomp <- dcast(facttemp,net ~ term, value.var = 'estimate')
library(GGally)

intercept_pairs <- ggpairs(factcomp[,-c('net')],
                           # columnLabels = c("Miles","Displacement","Horsepower",
                           #                  "Rear axle ratio","Weight","1/4 mile time"),
                           upper = list(continuous = wrap('points', size = 0.5,pch = 19,alpha = 0.6)),
                           lower = NULL,#list(NA),#combo = wrap("facethist", bins = 30)),
                           diag = list(continuous = wrap("barDiag")),
                           title = "Scatterplot matrix of entity-specific tie probabilities") +
   theme_bw()

ggsave(file = 'figures/intercep_estimate.png',plot = intercept_pairs,units = 'in',dpi = 450,height = 7,width= 7)



