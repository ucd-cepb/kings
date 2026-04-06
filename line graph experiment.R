
lg <- vector(mode = "list", length = 117)
for(i in 1:length(nets)){
   lg[[i]] <- igraph::make_line_graph(nets[[i]])
   
}

for(i in 1:length(nets)){
   myattr <- get.edge.attribute(nets[[i]])
   for(j in seq_along(myattr)){
      lg[[i]] <- set_vertex_attr(lg[[i]], name = names(myattr[j]), value = myattr[[j]])
   }
}

for(i in 1:length(nets)){
   myattr <- get.vertex.attribute(nets[[i]])
   for(j in seq_along(myattr)){
      lg[[i]] <- set_edge_attr(lg[[i]], name = names(myattr[j]), value = myattr[[j]])
   }
}

#make three edge attributes

#this ergm hasn't been tested yet
library(ergm)
modelexample <- ergm(lg[[i]] ~ edges + gwesp + edgecov(money_flow) + 
                        kstar(2) + nodecov(betw_centr) + 
                        edgecov(info_flow) )
