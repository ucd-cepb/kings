
library(tidyverse)
library(ellmer)

source("Network_Structure_Paper/Code/1a_generate_networks_functions.R")

all_nodes <- data.frame(
   name = character(),
   num_appearances = integer(),
   entity_type = character(),
   AI_TAGGED = double(),
   degree = integer(),
   gsp_id = integer()
)

gsp_ids_test <- head(gsp_ids, 3)

# Apply functions to all networks
for (g in seq_along(gsp_ids_test)) {
   
   gsp_id <- paste0("gsp_", gsp_ids[g])

   # Track token usage before net_process
   tu_before <- token_usage()

   gsp_list <- net_process(file = paste0(network_fp, "/", extract_list[g]),
                           gsp_id = gsp_ids[g],
                           use_ai_tagging = TRUE)
   
   gsp_graph <- net_graph(gsp_list, gsp_id = gsp_ids[g])
   
   saveRDS(object = gsp_graph$network_graph,
           file = paste0(Sys.getenv("BOX_PATH"),
                         "/network_structure_by_plan/networks_fully_labeled",
                         "/", extract_list[g]))
   
   sub_all_nodes <- igraph::as_data_frame(gsp_graph$igraph, what='vertices') %>%
      tibble() %>% 
      select(name, num_appearances, entity_type, AI_TAGGED, degree) %>% 
      mutate(gsp_id = gsp_ids[g])
   
   all_nodes <- rbind(all_nodes, sub_all_nodes)
   
   # Use the new plot_graph function for plotting
   p <- plot_graph(gsp_graph$igraph, title = paste0("GSP: ", gsp_id))
   ggsave(p, filename = paste0('Network_Structure_Paper/Out/gsp_graphs/', gsp_id, '.png'),
          width = 12, height = 12, dpi = 300)
   
   # Track token usage after net_process
   tu_after <- token_usage()
   
   # Calculate token usage difference for this GSP
   tu_diff <- tu_after
   tu_diff$input <- tu_after$input - tu_before$input
   tu_diff$output <- tu_after$output - tu_before$output
   # If price is character (e.g., "$0.34"), convert to numeric for diff
   price_num_before <- suppressWarnings(as.numeric(gsub("[$]", "", tu_before$price)))
   price_num_after <- suppressWarnings(as.numeric(gsub("[$]", "", tu_after$price)))
   tu_diff$price <- if (!is.na(price_num_before) && !is.na(price_num_after)) {
     sprintf("$%.2f", price_num_after - price_num_before)
   } else {
     NA_character_
   }
   print(paste0("Finished GSP ", gsp_id, 
                " | Token usage (input/output/price): ", 
                tu_diff$input, "/", tu_diff$output, "/", tu_diff$price))
}

# write.csv(all_nodes,
#           file = 'Network_Structure_Paper/Out/all_nodes_raw.csv',
#           row.names = FALSE)

all_nodes_final <- all_nodes %>%
   group_by(name) %>%
   summarise(
      num_appearances_sum = sum(num_appearances),
      num_appearances_mean = round(mean(num_appearances), 0),
      num_types = n_distinct(entity_type),
      entity_type = first(entity_type)
   ) %>% 
   arrange(desc(num_appearances_sum))

# write.csv(all_nodes_final,
#           file = 'Network_Structure_Paper/Out/all_nodes_cleaned.csv', 
#           row.names = FALSE)





