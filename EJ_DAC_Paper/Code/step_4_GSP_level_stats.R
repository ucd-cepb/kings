
# graphing an example network
gsp_list_test <- net_process(paste0(network_fp, "/",extract_list[3]))
gsp_graph_test <- net_graph(gsp_list_test)

# compare 'eigenvector' based on groups defined by 'DAC'
mean(V(gsp_graph_test)$eigenvector[V(gsp_graph_test)$DAC == 0], na.rm = TRUE)
mean(V(gsp_graph_test)$eigenvector[V(gsp_graph_test)$DAC == 1], na.rm = TRUE)

isolates_test <- which(degree(gsp_graph_test) == 0)
graph_2_test <- delete.vertices(gsp_graph_test, isolates_test)

ggraph(gsp_graph_test, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) + 
   geom_node_point(aes(size = num_appearances, colour = DAC)) + 
   theme_graph()