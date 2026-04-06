#purpose of this function is to determine if there are missing categories of ties
#related to specific ergm terms
vert_ids_to_names <- network::get.vertex.attribute(layered_nets[[k]], "vertex.names")

my_edgelist <- network::as.edgelist(layered_nets[[k]], attrname = c("is_money", "is_info"), output = "tibble")
my_edgelist <- my_edgelist %>% mutate(
   tailname = vert_ids_to_names[.tail],
   headname = vert_ids_to_names[.head]
   
)
summtable <- my_edgelist %>% group_by(tailname, headname) %>% 
   summarize(money_and_info = any(is_money == 1) & any(is_info == 1),
             money_and_neither = any(is_money == 1) & any(is_money == 0 & is_info == 0),
             info_and_neither = any(is_info == 1) & any(is_money == 0 & is_info == 0)) %>% ungroup() %>% 
   select(money_and_info, money_and_neither, info_and_neither) %>% colSums()

print(paste0("There are no shared ties between layers: ", names(summtable)[summtable == 0]))
