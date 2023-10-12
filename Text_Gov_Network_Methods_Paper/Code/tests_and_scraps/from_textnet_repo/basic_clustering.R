library(igraph)
library(pbapply)
library(statnet)
#symbolic link option below:
#flist <- list.files('data/output_large_files/weighted_directed_graphs/',full.names = T)
#local link option below:
#flist <- list.files('data_output/',pattern = "to_weighted_graph", full.names = T)
flist <- flist[c(1:38,40:67,69:length(flist))]

nlist <- pblapply(flist,function(x) {
   igr <- readRDS(x)
   igr <- subgraph(igr, V(igr)[
      vertex_attr(igr,"entity_type") %in% c("ORG","PERSON")])
   
   agency_df <- get.data.frame(igr, what = "both")
   net <- network(x=agency_df$edges[,1:2], directed = T,
                  hyper = F, loops = T, multiple = T, 
                  bipartiate = F, vertices = agency_df$vertices,
                  matrix.type = "edgelist")
   return(net)
   })
no_numbers<-lapply(nlist,function(x) get.inducedSubgraph(x,v = which(!grepl('^[0-9]',x %v% 'vertex.names'))))
no_lowers <-lapply(no_numbers,function(x) get.inducedSubgraph(x,v = which(!grepl('^[a-z]',x %v% 'vertex.names'))))

clters <- rbindlist(lapply(seq_along(no_lowers),function(i){
n <- get.inducedSubgraph(no_lowers[[i]],v = which(no_lowers[[i]] %v% 'entity_type' == 'ORG'))
ni <- intergraph::asIgraph(n)
data.table(orgs = gsize(ni),clusters = max(cluster_louvain(as.undirected(ni,'collapse'))$membership))
}))
clters$clusters


sapply(nlist,network.size)-sapply(no_lowers,network.size)

test <- data.table(name = no_lowers[[21]] %v% 'vertex.names',
           mention = no_lowers[[21]] %v% 'num_appearances')
head(test,30)

test[mention==1,]$name


test[grepl('^[a-z]',name),]

V(no_numbers[[1]])
gsize(decompose(ni)[[1]])


plot(decompose(ni)[[1]])
ni
ggplot(clters,aes(x = orgs,y = clusters)) + geom_point()

cluster_louvain(as.undirected(x,'collapse'))




g_noisolist <- pblapply(flist,function(x) {
   G <- readRDS(x)
Isolated = which(igraph::degree(G)==0)
G2 = igraph::delete.vertices(G, Isolated)
G2})

clusters<-lapply(g_noisolist,function(x){
   cluster_louvain(as.undirected(x,'collapse'))
})



cs <- data.table(net = 1:114,clusters = sapply(clusters,function(x) length(unique(x$membership))),do.call(rbind,lapply(glist,function(x) rbind(table(V(x)$entity_type)))))

csd<-melt(cs,id.vars = c('net','clusters'))



ggplot(csd) + facet_wrap(~variable,ncol = 2) + 
   theme_bw() + 
   geom_point(aes(x = value,y = clusters))



frbind(table(V(glist[[1]])$entity_type),
      table(V(glist[[2]])$entity_type))

table(V(glist[[1]])$entity_type)



t(as.data.table(table(V(glist[[1]])$entity_type)))




nlist <- pblapply(flist,function(x) {
   g=readRDS(x);n <- intergraph::asNetwork(g);n})




cluster_louvain(as.undirected(glist[[1]],'collapse'))
?cluster_louvain
