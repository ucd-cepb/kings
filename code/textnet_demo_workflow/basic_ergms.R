library(igraph)
library(pbapply)
library(statnet)
library(Rglpk)
library(broom)
library(data.table)
library(dplyr)
library(Bergm)
library(bayesplot)

edges_and_nodes <- list.files(path = "cleaned_extracts", full.names = T)
gspids <- substr(edges_and_nodes, 18,21)
gspids <- gspids[c(1:38,40:67,69:119)]
#box option
#flist <- list.files('data/output_large_files/weighted_directed_graphs/',full.names = T)
#local option
#flist <- list.files("data_output",pattern="to_weighted_graph",full.names=T)
super <- readRDS("data_output/supernetwork_weighted")
super <- subgraph(super, V(super)[
   vertex_attr(super,"entity_type") %in% c("ORG","PERSON")])
supersimpl <- igraph::simplify(super, remove.multiple = F, remove.loops = T)

supersimpl <- get.data.frame(supersimpl, what="both")
supersimpl <- network(x=supersimpl$edges[,1:2], directed = T,
                  hyper = F, loops = F, multiple = F, 
                  bipartiate = F, vertices = supersimpl$vertices,
                  matrix.type = "edgelist")

flist <- flist[c(1:38,40:67,69:length(flist))]

super <- get.data.frame(super, what="vertices")
super <- super[,c("name","num_GSPs_in")]

nlist <- pblapply(flist,function(x) {
   g=readRDS(x)
   g <- subgraph(g, V(g)[
      vertex_attr(g,"entity_type") %in% c("ORG","PERSON")])
   g<- igraph::simplify(g, remove.multiple = F, remove.loops = T)
   
   agency_df <- get.data.frame(g, what = "both")
   agency_df$vertices <- left_join(agency_df$vertices, super)
   n <- network(x=agency_df$edges[,1:2], directed = T,
                  hyper = F, loops = F, multiple = F, 
                  bipartiate = F, vertices = agency_df$vertices,
                  matrix.type = "edgelist")
   return(n)
   })

summs <- pblapply(seq_along(nlist), function(n) {
   print(n)
   as.data.table(t(summary(nlist[[n]] ~ edges + gwidegree(decay=0.25, fixed = T) +
           gwesp(decay=0.25, fixed = T, cutoff=30) +
           #nodecov("num_GSPs_in") +
           nodemix("entity_type"))))
})
summsbind <- rbindlist(summs)

bergm_function <- function(model){
   M.prior <- c(0, 0, 0, 0, 0, 0, 0)
   
   S.prior <- diag(10,7) # matrix for sigma values 
   
   #sometimes bergm will thrown an error that says it has unused arguments, if that is the case need to restart R
   bergm.post <- Bergm::bergm(model,
                       prior.mean = M.prior,
                       prior.sigma = S.prior,
                       gamma = .6, #changes the acceptance rate
                       burn.in = 1e3,
                       aux.iters = 2500,
                       main.iters = 3000,
                       nchains = 8, 
                       set.seed = 700) 
   return(bergm.post)
}

for(n in 1:length(nlist)){
   print(n)
   # Model ----
   m <- nlist[[n]] ~ edges + 
      isolates +
      gwidegree(decay=0.25, fixed = T) + 
      gwesp(decay=0.15, fixed = T,cutoff=30) + 
      nodemix("entity_type")
   
   m <- bergm_function(m)
   
   saveRDS(m, file = paste0("data_output/bergm_",gspids[[n]]))
   
}

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





#is not updated after this point:####




m1_erg_list <- pblapply(seq_along(nlist), function(n) {
   print(n)
tryCatch(ergm(nlist[[n]]~ edges + gwidegree(decay=0.25,fixed=T) +
                 gwesp(decay = 0.25, fixed=T, cutoff=30) +
                #nodecov('num_GSPs_in')+
         nodemix("entity_type"),verbose = F,
         eval.loglik = F,
         constraints = ~edges,
         control = control.ergm(seed=1,MCMC.samplesize=2048,
                                MCMC.interval=4096,
                                MCMLE.density.guard = exp(4))),
         error = function(e) NULL)},cl = 8)

coef_list <- lapply(seq_along(m1_erg_list),function(x) 
   tidy(m1_erg_list[[x]],conf.level = 0.95,conf.int = T) %>% mutate(net = x))

coef_dt <- rbindlist(coef_list,use.names = T)

saveRDS(list("models"=m1_erg_list, "coefs"=coef_dt),"data_output/m1_ergm_lists")






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



