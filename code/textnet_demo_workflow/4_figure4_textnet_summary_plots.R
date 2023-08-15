library(igraph)
library(pbapply)
library(data.table)
library(stringr)

network_stats <- readRDS("data_output/gov_dir_weight_no_gpe_network_properties")

gsp_meta <- readRDS("data_output/gsp_docs_w_meta")
gsp_meta <- gsp_meta[!is_comment & !is_reference,]
gsp_meta <- gsp_meta[,c(.SD,.N),by=gsp_id]
gsp_mini <- unique(gsp_meta[,c(1,14,16,19:27)])
gsp_mini <- gsp_mini[!gsp_mini$gsp_id %in% c("0089","0053"),]
network_stats <- network_stats[!network_stats$gsp_id %in% c("0089","0053"),]
#for meta
network_stats <- as_tibble(network_stats)
comp <- merge(gsp_mini, network_stats)
comp$num_nodes <- as.numeric(comp$num_nodes)
comp$num_edges <- as.numeric(comp$num_edges)
comp$reciprocity <- as.numeric(comp$reciprocity)
comp$centralization <- as.numeric(comp$centralization)
comp$transitivity <- as.numeric(comp$transitivity)
comp$connectedness <- as.numeric(comp$connectedness)
comp$modularity <- as.numeric(comp$modularity)

cor.test(comp$N, comp$transitivity)
cor.test(comp$N, comp$centralization)
cor.test(comp$N, comp$modularity)
cor.test(comp$N, comp$connectedness)
cor.test(comp$N, comp$reciprocity)
library(tidyverse)
base <- ggplot(data = comp,aes(x = N)) + 
   scale_x_continuous(name = '# pages') + 
   theme_bw() 
g1 <- base + 
   geom_point(aes(y = num_nodes),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# nodes') + 
   ggtitle('# nodes')
g2 <- base + 
   geom_point(aes(y = num_nodes/N),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# nodes/page') + 
   ggtitle('# nodes per page')
g3 <- base + 
   geom_point(aes(y = num_edges),pch = 21,alpha = 0.5) + 
   scale_y_continuous('# edges') + 
   ggtitle('# edges')
g4 <- base + 
   geom_point(aes(y = num_edges/N),pch = 21,alpha = 0.5) + 
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
g7 <- base2 + 
   geom_point(aes(y = modularity),pch = 21,alpha = 0.5) + 
   scale_y_continuous('modularity score') + 
   ggtitle('modularity')
g8 <- base2 + 
   geom_point(aes(y = reciprocity),pch = 21,alpha = 0.5) + 
   scale_y_continuous('reciprocity score') + 
   ggtitle('reciprocity')
g9 <- base2 + 
   geom_point(aes(y = connectedness),pch = 21,alpha = 0.5) + 
   scale_y_continuous('connectedness score') + 
   ggtitle('connectedness')

grom2 <- (grid.arrange(g5,g6, g7, g8, g9,ncol = 2))
ggsave(grom2,file = 'figures/net_stats_vs_pages.png',dpi = 450,units = 'in',height = 9,width = 7)

