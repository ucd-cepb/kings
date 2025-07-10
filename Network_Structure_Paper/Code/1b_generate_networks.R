
library(tidyverse)
library(ellmer)

source("Network_Structure_Paper/Code/1a_generate_networks_functions.R")


na_nodes_to_be_tagged <- data.frame(
   gsp_id = integer(),
   entity_name = character(),
   num_appearances = integer()
)

all_nodes <- data.frame(
   entity_name = character(),
   gsp_id = integer(),
   num_appearances = integer(),
   entity_type = character()
)

# Apply functions to all networks
for (g in seq_along(gsp_ids)) {
   
   gsp_id <- paste0("gsp_", gsp_ids[g])
   
   gsp_list <- net_process(file = paste0(network_fp, "/", extract_list[g]),
                           gsp_id = gsp_ids[g])
   
   gsp_graph <- net_graph(gsp_list, gsp_id = gsp_ids[g])
   
   saveRDS(object = gsp_graph$network_graph,
           file = paste0(Sys.getenv("BOX_PATH"),
                         "/network_structure_by_plan/networks_fully_labeled",
                         "/", extract_list[g]))
   
   # store nodes to view
   na_nodes <- igraph::as_data_frame(gsp_graph$igraph, what='vertices') %>%
      tibble() %>% 
      filter(is.na(entity_type)) %>%
      select(name, num_appearances, degree) %>% 
      mutate(gsp_id = gsp_ids[g])
   
   sub_all_nodes <- igraph::as_data_frame(gsp_graph$igraph, what='vertices') %>%
      tibble() %>% 
      select(name, num_appearances, entity_type) %>% 
      mutate(gsp_id = gsp_ids[g])
   
   na_nodes_to_be_tagged <- rbind(na_nodes_to_be_tagged, na_nodes)
   
   all_nodes <- rbind(all_nodes, sub_all_nodes)
   
   ggraph::ggraph(gsp_graph$igraph, layout = 'fr') +
      geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
      geom_node_point(aes(color = entity_type, size=degree)) +
      theme_void() +
      ggtitle(paste0("GSP: ", gsp_id))
   
   ggsave(paste0('Network_Structure_Paper/Out/gsp_graphs/', gsp_id, '.png'),
          width = 9, height = 9, dpi = 300)
   
   print(paste0("Finished GSP ", gsp_id))
}

na_nodes_to_be_tagged_final <- na_nodes_to_be_tagged %>%
   group_by(name) %>%
   summarise(
      num_appearances = sum(num_appearances),
      num_gsps = length(unique(gsp_id)),
      mean_degree = round(mean(degree), 0),
   ) %>%
   arrange(desc(num_appearances))

all_nodes_final <- all_nodes %>%
   group_by(name) %>%
   summarise(
      num_appearances_sum = sum(num_appearances),
      num_appearances_mean = round(mean(num_appearances), 0),
      num_types = n_distinct(entity_type),
      entity_type = first(entity_type)
   ) %>% 
   arrange(desc(num_appearances_sum))

View(na_nodes_to_be_tagged_final %>% filter(str_length(name) > 3 & (mean_degree > 10 | num_gsps > 2 | num_appearances > 2)))


write.csv(na_nodes_to_be_tagged_final, 
          file = 'Network_Structure_Paper/Out/na_nodes_to_be_tagged.csv', 
          row.names = FALSE)

write.csv(all_nodes_final,
          file = 'Network_Structure_Paper/Out/all_nodes_final.csv', 
          row.names = FALSE)

# test code for one network

idt <- 40
gsp_idt <- gsp_ids[idt]

glt <- net_process(file = paste0(network_fp, "/",extract_list[idt]),
                   gsp_id = gsp_idt)

ggt <- net_graph(glt, gsp_id = gsp_idt)

plot_graph <- delete_vertices(ggt$igraph, which(igraph::degree(ggt$igraph) == 0))

# add degree to plot_graph

plot_graph <- set_vertex_attr(plot_graph,
                              'degree',
                              value = igraph::degree(plot_graph))

igraph::as_data_frame(plot_graph, what='vertices') %>% 
   as_tibble %>% 
   # filter(is.na(org_type)) %>% 
   arrange(desc(degree))

ggraph::ggraph(plot_graph, layout = 'fr') +
   geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
   geom_node_point(aes(color = entity_type, size=degree)) +
   # geom_node_text(aes(label = name), repel = TRUE) +
   theme_void() +
   # theme(legend.position = "none") +
   ggtitle(paste0("GSP: ", gsp_idt)) 
