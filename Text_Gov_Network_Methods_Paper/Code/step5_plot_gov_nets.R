library(ggraph)
filekey <- read.csv("filekey.csv")

edges_and_nodes <- list.files(path = filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath, full.names = T)
gspids <- substr(edges_and_nodes, 18,21)

#only plots orgs and people
for(m in 1:length(edges_and_nodes)){
   weighted_graph <- readRDS(paste0(filekey[filekey$var_name=="weighted_netw_govnetpaper",]$filepath,gspids[m]))
   weighted_graph <- subgraph(weighted_graph, V(weighted_graph)[
      vertex_attr(weighted_graph,"entity_type") %in% c("ORG","PERSON")])
   
   #now remove loops so that isolates with a self-loop are not plotted
   weighted_graph_no_loops <- igraph::simplify(weighted_graph, remove.multiple = F, remove.loops = T)
   
   isolates = which(igraph::degree(weighted_graph_no_loops)==0)
   weighted_graph_noisolates = igraph::delete.vertices(weighted_graph_no_loops, isolates)
   
   #order of these layers matters
   weighted_plot_noisolates <- ggraph(weighted_graph_noisolates, layout = 'fr')+
      #ggraph::scale_edge_colour_gradient(high = viridis::cividis(5)[1], low = viridis::cividis(5)[4])+
      geom_edge_fan(aes(alpha = weight),
                    end_cap = circle(1,"mm"),
                    color = "#000000",
                    width = 0.3,
                    arrow = arrow(angle=15,length=unit(0.07,"inches"),ends = "last",type = "closed"))+
      #tol_high-contrast color scheme  "#DDAA33" is the GPE color
      scale_color_manual(values = c("#004488","#BB5566"))+
      geom_node_point(aes(color = entity_type), size = 1,
                      alpha = 0.8)+
      #geom_node_text(aes(label = labels),
      #               size = 3, repel = T, color = "black")+
      theme_void()
   
   
   ggsave(paste0("directed_gov_net_toweighted_isoremoved_nogpe",gspids[m],".png"), plot = weighted_plot_noisolates, device = "png",
          path = filekey[filekey$var_name=="govnetpaper_figures",]$filepath, width = 4020, height = 1890, dpi = 300,
          units = "px", bg = "white")
   
}
