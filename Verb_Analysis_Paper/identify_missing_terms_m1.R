#purpose of this function is to determine if there are missing categories of ties
#related to specific ergm terms
identify_missing_terms <- function(k){
   vert_ids_to_names <- network::get.vertex.attribute(layered_nets[[k]], "vertex.names")
   
   my_edgelist <- network::as.edgelist(layered_nets[[k]], attrname = c("is_money", "is_info"), output = "tibble")
   my_layers <- get.vertex.attribute(layered_nets[[k]], ".LayerName")
   my_edgelist <- my_edgelist %>% mutate(
      Layer_tail = my_layers[.tail],
      Layer_head = my_layers[.head],
      tailname = vert_ids_to_names[.tail],
      headname = vert_ids_to_names[.head]
      
   )
   #make sure headname and tailname are alphabetical for matching purposes
   my_edgelist <- my_edgelist %>% mutate(
      z_name = pmax(headname, tailname),
      a_name = pmin(headname, tailname)
      
   )
   summtable <- my_edgelist %>% group_by(a_name, z_name) %>% 
      summarize(  
                   money_and_info = any(Layer_tail == "money") & any(Layer_tail == "info"),
                   money_and_neither = any(Layer_tail == "money") & any(Layer_tail == "neither"),
                   info_and_neither = any(Layer_tail == "info") & any(Layer_tail == "info"),
                   money_money = sum(Layer_tail == "money") > 1,
                   info_info = sum(Layer_tail == "info") > 1,
                   neither_neither = sum(Layer_tail == "neither") > 1,
                   num_in_group = n()) %>%
      ungroup() %>% 
      select(money_and_info, money_and_neither, info_and_neither,
             money_money, info_info, neither_neither) %>% colSums()
   
   #print(paste0("There are no shared ties between layers: ", names(summtable)[summtable == 0]))
   return(summtable)
}


summarytables <- vector(mode = "list", length = 117)
for(i in 1:length(layered_nets)){
   summarytables[[i]] <- identify_missing_terms(i)
}

summdf <- setNames(data.frame(matrix(ncol = 6, nrow = 117)), names(summarytables[[1]]))
for(i in 1:length(layered_nets)){
   summdf[i,] <- summarytables[[i]]
}

View(summdf)
saveRDS(summdf, "data/Verb_Analysis_Paper/reciprocal_edge_summary_by_plan.RDS")
