library(ggplot2)
library(GGally)
library(ggraph)
library(data.table)
library(igraph)
filekey <- read.csv("filekey.csv")

use_filtered_parsefiles <- F


edges_and_nodes <- list.files(path = filekey[filekey$var_name=="disambiged_unfiltered_extracts_superpaper",]$filepath, full.names = T)
gspids <- stringr::str_extract(edges_and_nodes,'[0-9]{1,}')


#excluding the garbled PDF ("0089") and the duplicate ("0053")
graphs <- c(1:38, 40:67,69:length(gspids))
supernodes <- vector(mode = "list", length = length(gspids)-2)
superedges <- vector(mode = "list", length = length(gspids)-2)
for(m in graphs){
   print(m)
   if(use_filtered_parsefiles == T){
      single_ig <- readRDS(paste0(filekey[filekey$var_name=="full_directed_filtered_graphs_superpaper",]$filepath,gspids[m]))
      
   }else{
      single_ig <- readRDS(paste0(filekey[filekey$var_name=="full_directed_unfiltered_graphs_superpaper",]$filepath,gspids[m]))
   }
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

#removing edges where the lemma is not an English word
#usually this is because of Spanish segments or acronyms being the verb (incorrect parsing)
superedgesdt <- superedgesdt[tolower(superedgesdt$head_verb_lemma) %in% textNet::eng_words,]

#TODO now degr is not a real thing so get rid of it

#removing self-loops: source and target must be different
superedgesdt <- superedgesdt[superedgesdt$from != superedgesdt$to,]

supernetwork <- igraph::graph_from_data_frame(superedgesdt,
                                          vertices = supernodesdt,
                                          directed = T)

vcount(supernetwork)
ecount(supernetwork)

if(use_filtered_parsefiles == T){
   saveRDS(supernetwork, filekey[filekey$var_name=="supernetwork_filtered_full_superpaper",]$filepath)
   
}else{
   saveRDS(supernetwork, filekey[filekey$var_name=="supernetwork_unfiltered_full_superpaper",]$filepath)
   
}
colnames(superedgesdt)[colnames(superedgesdt)=="from"] <- "source"
colnames(superedgesdt)[colnames(superedgesdt)=="to"] <- "target"

#we set self-loops to false because there are a lot of isolates
weighted_graph <- textNet::export_to_network(textnet_extract = list("nodelist" = supernodesdt, "edgelist" = superedgesdt), 
                                                export_format = "igraph", 
                                                keep_isolates = T, collapse_edges = T, self_loops = F)
   
#uses original edges to calculate degree (aka strength of weighted graph)
strength <- sort(igraph::strength(weighted_graph[[1]]),decreasing = T)
topstrength <- names(strength[1:7])
weighted_graph[[1]] <- igraph::set_vertex_attr(weighted_graph[[1]], "labels", 
                                          value = ifelse(igraph::get.vertex.attribute(weighted_graph[[1]],"name") %in% topstrength, 
                                                         igraph::get.vertex.attribute(weighted_graph[[1]],"name"), NA))
   
if(use_filtered_parsefiles == T){
   saveRDS(weighted_graph, filekey[filekey$var_name=="supernetwork_filtered_weighted_superpaper",]$filepath)
}else{
   saveRDS(weighted_graph, filekey[filekey$var_name=="supernetwork_unfiltered_weighted_superpaper",]$filepath)
   
}

   isolates = which(igraph::degree(weighted_graph[[1]])==0)
   weighted_graph_no_isolates = igraph::delete_vertices(weighted_graph[[1]], isolates)  
   
   #reorder vertices from highest to lowest degree
   weighted_graph_noiso_ordered <- permute(weighted_graph_no_isolates, rank(V(weighted_graph_no_isolates)$degr, ties.method = "first"))
   
   #order of these layers matters
weighted_plot_noisolates <- ggraph(weighted_graph_noiso_ordered, layout = 'drl')+
   #ggraph::scale_edge_colour_gradient(high = viridis::cividis(5)[1], low = viridis::cividis(5)[4])+
   geom_edge_fan(aes(alpha = weight),
                 end_cap = circle(1,"mm"),
                 color = "#333333",
                 width = 0.2,
                 arrow = arrow(angle=15,length=unit(0.07,"inches"),ends = "last",type = "closed"))+
   #tol_high-contrast color scheme  
   scale_color_viridis_b()+
   geom_node_point(aes(color = num_GSPs_in), alpha = 0.4, size = 1)+
   #geom_node_text(aes(label = labels),
   #               size = 3, repel = T, color = "black")+
   theme_void()

if(use_filtered_parsefiles == T){
   ggsave(paste0("supernetwork_filtered.png"), plot = weighted_plot_noisolates, device = "png",
          path = filekey[filekey$var_name=="supernetwork_figures",]$filepath, width = 4020, height = 1890, dpi = 300,
          units = "px", bg = "white")
   
}else{
   ggsave(paste0("supernetwork_unfiltered_20250522_numGSPs.png"), plot = weighted_plot_noisolates, device = "png",
          path = filekey[filekey$var_name=="supernetwork_figures",]$filepath, width = 4020, height = 1890, dpi = 300,
          units = "px", bg = "white")
   
}

periph = which(igraph::degree(weighted_graph_noiso_ordered)<10)
weighted_graph_tenplus = igraph::delete_vertices(weighted_graph_noiso_ordered, periph)  
#this has created some isolates
periph = which(igraph::degree(weighted_graph_tenplus)<2)
weighted_graph_tenplus = igraph::delete_vertices(weighted_graph_tenplus, periph)  


weighted_plot_tenplus <- ggraph(weighted_graph_tenplus, layout = 'stress')+
   #ggraph::scale_edge_colour_gradient(high = viridis::cividis(5)[1], low = viridis::cividis(5)[4])+
   geom_edge_fan(aes(alpha = weight),
      end_cap = circle(1,"mm"),
      color = "#333333",
      width = 0.2,
      arrow = NULL) +
         #arrow(angle=15,length=unit(0.07,"inches"),ends = "last",type = "closed"))+
   #tol_high-contrast color scheme  
   scale_color_viridis_b()+
   geom_node_point(aes(color = num_GSPs_in), alpha = 0.4, size = 1)+
   #geom_node_text(aes(label = labels),
   #               size = 3, repel = T, color = "black")+
   theme_void()

ggsave(paste0("supernetwork_unfiltered_20250522_tenplus.png"), plot = weighted_plot_tenplus, device = "png",
       path = filekey[filekey$var_name=="supernetwork_figures",]$filepath, width = 4020, height = 1890, dpi = 300,
       units = "px", bg = "white")

weighted_graph_noiso_ordered <- igraph::set_vertex_attr(weighted_graph_noiso_ordered, "final_degr", value = igraph::degree(weighted_graph_noiso_ordered))
weighted_graph_noiso_ordered <- igraph::set_vertex_attr(weighted_graph_noiso_ordered, "final_str", value = igraph::strength(weighted_graph_noiso_ordered))

mydf <- get.data.frame(weighted_graph_noiso_ordered, what = "both")
View(mydf[[1]])
plot(log(mydf[[1]]$final_str), log(mydf[[1]]$num_GSPs_in))
