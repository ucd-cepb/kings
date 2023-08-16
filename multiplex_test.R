
edgelist <- setDT(list(
   "source" = c("Glenn_County","Glenn_County","San_Francisco","Georgia"),
   "target" = c("Texas_Instr","Texas_Instr","Baylor","Austin"),
   "lemma" = c("bought","bought","is","indicated")))
nodelist <- setDT(list(
   "node_name" = c("Rocky","Alameda","Glenn_County","San_Francisco","Georgia",
                   "Texas_Instr","Waco","Baylor","Austin"),
   "entity_type" = c("Person","GPE","GPE","GPE","GPE",
                     "ORG","GPE","GPE","GPE")
))
test_graph <- igraph::graph_from_data_frame(
   edgelist, vertices = nodelist, directed = T)
vcount(test_graph)
ecount(test_graph)
tgdf <- get.data.frame(test_graph, what = "both")

edgelist2 <- setDT(list(
   "source" = c("Glenn_County","Glenn_County","San_Francisco","Georgia"),
   "target" = c("Texas_Instr","Texas_Instr","Baylor","Austin"),
   "lemma" = c("went","sold","is","indicated")))
nodelist2 <- setDT(list(
   "node_name" = c("Rocky","Alameda","Glenn_County","San_Francisco","Georgia",
                   "Texas_Instr","Waco","Baylor","Austin"),
   "entity_type" = c("Person","Person","Person","GPE","GPE",
                     "ORG","GPE","GPE","GPE")
))
test_graph2 <- igraph::graph_from_data_frame(
   edgelist2, vertices = nodelist2, directed = T)
vcount(test_graph2)
ecount(test_graph2)
tgdf2 <- get.data.frame(test_graph2, what = "both")

combined <- igraph::graph_from_data_frame(rbind(tgdf$edges,tgdf2$edges),
                  vertices = rbind(tgdf$vertices, tgdf$vertices),
                  directed = T)

vcount(combined)
ecount(combined)
edge_attr_names(combined)
