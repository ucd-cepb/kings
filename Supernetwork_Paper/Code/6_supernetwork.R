library(ggplot2)
library(GGally)
library(ggraph)
library(data.table)
filekey <- read.csv("filekey.csv")

edges_and_nodes <- list.files(path = filekey[filekey$var_name=="disambiged_unfiltered_extracts_superpaper",]$filepath, full.names = T)
gspids <- stringr::str_extract(edges_and_nodes,'[0-9]{1,}')

#excluding the garbled PDF ("0089") and the duplicate ("0053")
graphs <- c(1:38, 40:67,69:length(gspids))

supernodes <- vector(mode = "list", length = length(gspids)-2)
superedges <- vector(mode = "list", length = length(gspids)-2)
for(m in graphs){
   print(m)
   single_ig <- readRDS(paste0(filekey[filekey$var_name=="full_directed_unfiltered_graphs_superpaper",]$filepath,gspids[m]))
   single_ig <- single_ig[[1]]
   sidf <- get.data.frame(single_ig, what = "both")
   supernodes[[m]] <- sidf$vertices
   superedges[[m]] <- sidf$edges
}

#getting rid of duplicates and keeping most common node attribute (entity_type)
superedgesdt <- rbindlist(superedges)
supernodesdt <- rbindlist(supernodes)
supernodesdt <- supernodesdt[order(-degr),]
supernodesdt <- supernodesdt[,num_GSPs_in := .N, by="name"]
supernodesdt <- supernodesdt[!duplicated(supernodesdt, by="name"),]

supernetwork <- igraph::graph_from_data_frame(superedgesdt,
                                          vertices = supernodesdt,
                                          directed = T)

vcount(supernetwork)
ecount(supernetwork)

saveRDS(supernetwork, filekey[filekey$var_name=="supernetwork_full_superpaper",]$filepath)
colnames(superedgesdt)[colnames(superedgesdt)=="from"] <- "source"
colnames(superedgesdt)[colnames(superedgesdt)=="to"] <- "target"

weighted_graph <- textNet::export_to_network(textnet_extract = list("nodelist" = supernodesdt, "edgelist" = superedgesdt), 
                                                export_format = "igraph", 
                                                keep_isolates = T, collapse_edges = T, self_loops = T)
   
   #uses original edges to calculate degree
   strength <- sort(igraph::strength(weighted_graph[[1]]),decreasing = T)
   topstrength <- names(strength[1:7])
   weighted_graph[[1]] <- igraph::set_vertex_attr(weighted_graph[[1]], "labels", 
                                             value = ifelse(igraph::get.vertex.attribute(weighted_graph[[1]],"name") %in% topstrength, 
                                                            igraph::get.vertex.attribute(weighted_graph[[1]],"name"), NA))
   
  
saveRDS(weighted_graph, filekey[filekey$var_name=="supernetwork_weighted_superpaper",]$filepath)

#code imported from plot_gov_nets.R
weighted_graph_no_isolates <- textNet::export_to_network(textnet_extract = list("nodelist" = supernodesdt, "edgelist" = superedgesdt), 
                                                      export_format = "igraph", 
                                                      keep_isolates = F, collapse_edges = T, self_loops = F)

#order of these layers matters
weighted_plot_noisolates <- ggraph(weighted_graph_no_isolates[[1]], layout = 'fr')+
   #ggraph::scale_edge_colour_gradient(high = viridis::cividis(5)[1], low = viridis::cividis(5)[4])+
   geom_edge_fan(aes(alpha = weight),
                 end_cap = circle(1,"mm"),
                 color = "#333333",
                 width = 0.2,
                 arrow = arrow(angle=15,length=unit(0.07,"inches"),ends = "last",type = "closed"))+
   #tol_high-contrast color scheme  
   scale_color_manual(values = c("#DDAA33","#004488","#BB5566"))+
   geom_node_point(aes(color = entity_type), size = 1,
                   alpha = 0.8)+
   #geom_node_text(aes(label = labels),
   #               size = 3, repel = T, color = "black")+
   theme_void()


ggsave(paste0("supernetwork.png"), plot = weighted_plot_noisolates, device = "png",
       path = filekey[filekey$var_name=="supernetwork_figures",]$filepath, width = 4020, height = 1890, dpi = 300,
       units = "px", bg = "white")

