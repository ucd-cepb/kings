library(igraph)
library(pbapply)
library(statnet)
library(Rglpk)
library(broom)
library(data.table)
library(dplyr)
library(Bergm)
library(bayesplot)
filekey <- read.csv("filekey.csv")
edges_and_nodes <- list.files(path = filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath, full.names = T)
gspids <- stringr::str_extract(edges_and_nodes,'[0-9]{1,}')
gspids <- gspids[c(1:38,40:67,69:119)]
#box option
#flist <- list.files('data/output_large_files/weighted_directed_graphs/',full.names = T)
#local option
#flist <- list.files("data/output_large_files",pattern="to_weighted_graph",full.names=T)
super <- readRDS(filekey[filekey$var_name=="supernetwork_weighted_govnetpaper",]$filepath)
super <- subgraph(super, V(super)[
   vertex_attr(super,"entity_type") %in% c("ORG","PERSON")])
supersimpl <- igraph::simplify(super, remove.multiple = F, remove.loops = T)

supersimpl <- get.data.frame(supersimpl, what="both")
supersimpl <- network(x=supersimpl$edges[,1:2], directed = T,
                  hyper = F, loops = F, multiple = F, 
                  bipartiate = F, vertices = supersimpl$vertices,
                  matrix.type = "edgelist")

flist <- flist[c(1:38,40:67,69:119)]

super <- get.data.frame(super, what="vertices")
super <- super[,c("name","num_GSPs_in")]

nlist <- pblapply(flist,function(x) {
   print(x)
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

gspids[which(summsbind$mix.entity_type.PERSON.PERSON==0)]

bergm_function <- function(model){

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
   
   M.prior <- c(0, 0, 0, 0, 0)
      
   S.prior <- diag(10,5) # matrix for sigma values 
      
      
    m <- nlist[[n]] ~ edges + 
      isolates +
      gwidegree(decay=0.25, fixed = T) + 
      gwesp(decay=0.15, fixed = T,cutoff=30) + 
      nodefactor("entity_type")
   
   m <- bergm_function(m)
   
   saveRDS(m, file = paste0(filekey[filekey$var_name=="bergms_govnetpaper",]$filepath,gspids[[n]]))
   
}

used_gspids <- gspids[!gspids %in% c("0053","0089")]

m1_erg_list <- lapply(1:length(used_gspids), function(n) readRDS(
   paste0(filekey[filekey$var_name=="bergms_govnetpaper",]$filepath,used_gspids[[n]])))

saveRDS(m1_erg_list,filekey[filekey$var_name=="all_bergms_govnetpaper",]$filepath)

coef_dt <- rbindlist(lapply(1:length(m1_erg_list),
                     function(n) as.data.table(t(colMeans(m1_erg_list[[n]]$Theta)))))
colnames(coef_dt) <- c("edges","isolates","gwidegree","gwesp","person")

library(ggthemes)
library(statnet)
library(GGally)
cdtmelt <- coef_dt
cdtmelt$rownum <- 1:nrow(cdtmelt)
cdtmelt <- pivot_longer(cdtmelt, cols = !rownum, names_to = "var",values_to = "estimate")
cdtmelt$var <- as.factor(cdtmelt$var)

boxplot(coef_dt)

ergm_boxplot <- ggplot(cdtmelt, aes(x=var,y=estimate)) +
geom_boxplot()+
   theme_bw() 
ggsave(file = paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,"/ergm_boxplot.png"),plot = ergm_boxplot,units = 'in',dpi = 450,height = 5,width= 7)


library(coda)

#adapted from summary.bergm
Theta <- vector(mode="list",length=length(used_gspids))
Theta <- lapply(1:length(m1_erg_list), function(i) as.mcmc(m1_erg_list[[i]]$Theta))
quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)
varquant <- lapply(1:length(Theta), function(i) as.data.table(cbind(t(apply(Theta[[i]], 2, quantile, quantiles)),
                                                            c("edges","isolates","gwidegree","gwesp","person")),
                                                    ))

vq <- rbindlist(varquant)

coef_dt$edgesLow <- as.numeric(vq$`2.5%`[vq$V6=="edges"])
coef_dt$edgesHi <- as.numeric(vq$`97.5%`[vq$V6=="edges"])
coef_dt$isolatesLow <- as.numeric(vq$`2.5%`[vq$V6=="isolates"])
coef_dt$isolatesHi <- as.numeric(vq$`97.5%`[vq$V6=="isolates"])
coef_dt$gwidegreeLow <- as.numeric(vq$`2.5%`[vq$V6=="gwidegree"])
coef_dt$gwidegreeHi <- as.numeric(vq$`97.5%`[vq$V6=="gwidegree"])
coef_dt$gwespLow <- as.numeric(vq$`2.5%`[vq$V6=="gwesp"])
coef_dt$gwespHi <- as.numeric(vq$`97.5%`[vq$V6=="gwesp"])
coef_dt$personLow <- as.numeric(vq$`2.5%`[vq$V6=="person"])
coef_dt$personHi <- as.numeric(vq$`97.5%`[vq$V6=="person"])

network_properties <- readRDS(paste0(filekey[filekey$var_name=="network_properties_folder_govnetpaper",]$filepath,"/gov_dir_weight_no_gpe_network_properties"))
network_properties <- network_properties[!network_properties$gsp_id %in% c("0053","0089"),]

coef_dt$num_nodes <- as.numeric(network_properties$num_nodes)
#gwesp
gwesp_nodes <- ggplot(coef_dt, aes(num_nodes, gwesp)) +
   geom_point() +
   theme_bw() +
   geom_errorbar(aes(ymin = gwespLow, ymax = gwespHi))
ggsave(file = paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,"/gwesp_nodes.png"),plot = gwesp_nodes,units = 'in',dpi = 450,height = 5,width= 7)

#gwdegree
gwdegree_nodes <- ggplot(coef_dt, aes(num_nodes, gwidegree)) +
   geom_point() +
   theme_bw() +
   geom_errorbar(aes(ymin = gwidegreeLow, ymax = gwidegreeHi))
ggsave(file = paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,"/gwdegree_nodes.png"),plot = gwdegree_nodes,units = 'in',dpi = 450,height = 5,width= 7)

#re: this, I'm thinking something like 2 panels, 
#side by side, each panel orders x-axis by network size (# of nodes) (or whatever)
#, y axis is parameter estimate, but each estimate is plotted as a vertical bar, 
#bar is interval. one panel for gwidegree, one panel for gwesp


#not updated after this point


intercept_pairs <- ggpairs(factcomp[,-c('net')],
       # columnLabels = c("Miles","Displacement","Horsepower",
       #                  "Rear axle ratio","Weight","1/4 mile time"),
       upper = list(continuous = wrap('points', size = 0.5,pch = 19,alpha = 0.6)),
       lower = NULL,#list(NA),#combo = wrap("facethist", bins = 30)),
       diag = list(continuous = wrap("barDiag")),
        title = "Scatterplot matrix of tie probabilities by entity type, ERGM") +
    theme_bw()

ggsave(file = paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,"/mix_estimate.png"),plot = intercept_pairs,units = 'in',dpi = 450,height = 7,width= 7)





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

ggsave(file = paste0(filekey[filekey$var_name=="govnetpaper_figures",]$filepath,"/intercep_estimate.png"),plot = intercept_pairs,units = 'in',dpi = 450,height = 7,width= 7)



