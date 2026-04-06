library(ggraph)
flobusiness <- set.vertex.attribute(flobusiness, "vertexnames", get.vertex.attribute(flobusiness, "vertex.names"))

list.vertex.attributes(flomarriage)
flomarriage <- set.vertex.attribute(flomarriage, "vertexnames", get.vertex.attribute(flomarriage, "vertex.names"))
ggraph(flobusiness) + geom_node_label(aes(label = vertexnames), repel = T, color = "red") + geom_edge_link()

ggraph(flomarriage) + geom_node_label(aes(label = vertexnames), repel = T, color = "blue") + geom_edge_link()

library(ergm.multi)
flo <- flomarriage | flobusiness
flo[,, names.eval="marriage"] <- as.matrix(flomarriage)
flo[,, names.eval="business"] <- as.matrix(flobusiness)
flo # edge attributes
flo <- Layer(flo, c(m="marriage", b="business"))
ergm(flo ~ L(~edges, ~m)+L(~edges, ~b))



