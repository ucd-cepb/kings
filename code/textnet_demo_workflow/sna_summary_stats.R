library(igraph)
library(pbapply)
flist <- list.files('data/directed_network_objects/networks_collapsed_to_weighted/',full.names = T)

nlist <- pblapply(flist,function(x) {
   g=readRDS(x);n <- intergraph::asNetwork(g);
   und_g <- as.undirected(g, mode = "collapse")
   lc <- igraph::cluster_louvain(und_g)
   data.table(id = basename(x),
                              edges = gsize(g),
                              nodes = gorder(g),
                              mean_edgeweight = mean(E(g)$weight),
                              connectedness = sna::connectedness(n),
                              centralization = sna::centralization(n,sna::degree),
                              transitivity = sna::gtrans(n, mode = "digraph", use.adjacency=F),
                              modularity = modularity(und_g,lc$membership,E(und_g)$weight)
                              )
   },cl = 8)


library(data.table)
library(stringr)
net_stats <- rbindlist(nlist)
net_stats$gsp_id <- str_extract(net_stats$id,'[0-9]{1,}')
docs <- readRDS('data/textnet_data/gsp_docs_w_meta')
page_count <- docs[,.N,by=.(gsp_id)][order(-N),]

comp <- merge(net_stats,page_count)
library(tidyverse)
base <- ggplot(data = comp,aes(x = N)) + 
   scale_x_continuous(name = '# pages') + 
   theme_bw() 
g1 <- base + 
   geom_point(aes(y = nodes),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# nodes') + 
   ggtitle('# nodes')
g2 <- base + 
   geom_point(aes(y = nodes/N),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# nodes/page') + 
   ggtitle('# nodes per page')
g3 <- base + 
   geom_point(aes(y = edges),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# edges') + 
   ggtitle('# edges')
g4 <- base + 
   geom_point(aes(y = edges/N),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# edges/page') + 
   ggtitle('# edges per page')

library(gridExtra)
grom <- grid.arrange(g1,g2,g3,g4,ncol = 2)
ggsave(grom,file = 'figures/size_vs_pages.png',dpi = 450,units = 'in',height = 7,width = 7)

base2 <- ggplot(data = comp,aes(x = N)) + 
   scale_x_continuous(name = '# pages') + 
   theme_bw() 
g5 <- base2 + 
   geom_point(aes(y = centralization),pch = 21,alpha = 0.5) + 
   scale_y_continuous('centralization score') + 
   ggtitle('centralization')
g6 <- base2 + 
   geom_point(aes(y = transitivity),pch = 21,alpha = 0.5) + 
   scale_y_continuous('transitivity score') + 
   ggtitle('transitivity')

grom2 <- (grid.arrange(g5,g6,ncol = 2))
ggsave(grom2,file = 'figures/central_trans_vs_pages.png',dpi = 450,units = 'in',height = 3.5,width = 7)


library(htmlTable)
library(vtable)
sumtable(data = net_stats[,-c('gsp_id','id')],add.median = T,out = 'htmlreturn',file = 'figures/table2_sumstats.html',)
sum_res <- summary(net_stats[,-c('gsp_id','id')])

data.table(sum_res)
