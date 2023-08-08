library(ggplot2)
library(GGally)
library(ggraph)
library(data.table)

edges_and_nodes <- list.files(path = "cleaned_extracts", full.names = T)
gspids <- substr(edges_and_nodes, 18,21)

#excluding the garbled PDF ("0089")
graphs <- c(1:67,69:length(gspids))

supernodes <- vector(mode = "list", length = length(gspids)-1)
superedges <- vector(mode = "list", length = length(gspids)-1)
for(m in graphs){
   print(m)
   single_ig <- readRDS(paste0("data_output/full_directed_graph_",gspids[m]))
   sidf <- get.data.frame(single_ig, what = "both")
   supernodes[[m]] <- sidf$vertices
   superedges[[m]] <- sidf$edges
}

#getting rid of duplicates and keeping most common node attribute (entity_type)
superedgesdt <- rbindlist(superedges)
supernodesdt <- rbindlist(supernodes)
supernodesdt <- supernodesdt[order(-degr),]
supernodesdt <- supernodesdt[!duplicated(supernodesdt, by="name"),]

supernetwork <- igraph::graph_from_data_frame(superedgesdt,
                                          vertices = supernodesdt,
                                          directed = T)

vcount(supernetwork)
ecount(supernetwork)

saveRDS(supernetwork, "data_output/supernetwork_full")

weighted_graph <- supernetwork

weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_id")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_tense")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_name")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_lemma")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "parent_verb_id")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "neg")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "doc_sent_verb")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "doc_sent_parent")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "helper_lemma")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "helper_token")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_verb")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_helper_lemma")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_helper_token")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "edgeiscomplete")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "has_hedge")
weighted_graph <- igraph::delete_edge_attr(weighted_graph, "is_future")

igraph::E(weighted_graph)$weight <- 1
weighted_graph <- igraph::simplify(weighted_graph, edge.attr.comb=list(weight="sum"), remove.loops = F)

#uses original edges to calculate degree
degs <- sort(igraph::degree(supernetwork),decreasing = T)
topdegs <- names(degs[1:7])
weighted_graph <- igraph::set_vertex_attr(weighted_graph, "labels", 
                                          value = ifelse(igraph::get.vertex.attribute(weighted_graph,"name") %in% topdegs, 
                                                         igraph::get.vertex.attribute(weighted_graph,"name"), NA))

saveRDS(weighted_graph, "data_output/supernetwork_weighted")

#code imported from plot_gov_nets.R
weighted_graph_no_loops <- igraph::simplify(weighted_graph, remove.multiple = F, remove.loops = T)

isolates = which(igraph::degree(weighted_graph_no_loops)==0)
weighted_graph_noisolates = igraph::delete.vertices(weighted_graph_no_loops, isolates)

#order of these layers matters
weighted_plot_noisolates <- ggraph(weighted_graph_noisolates, layout = 'fr')+
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
       path = "figures", width = 4020, height = 1890, dpi = 300,
       units = "px", bg = "white")

