library(statnet)
library(coop)
library(data.table)

bad <- c('0053','0089')
meta <- readRDS('Multipurpose_Files/gsp_docs_w_meta')
meta <- meta |> dplyr::select(-page_num,-text)
meta <- meta[!duplicated(meta$gsp_id),]
meta$Republican_Vote_Share <- as.numeric(meta$Republican_Vote_Share)
meta$Agr_Share_Of_GDP <- as.numeric(meta$Agr_Share_Of_GDP)


gs_edge <- fread('Network_Innovation_Paper/data_products/all_gsa_edges.csv')
gs_edge$gsp_id <- formatC(gs_edge$gsp_id,width = 4,flag = '0')
gs_melt <- melt(gs_edge,id.vars = c('gsp_id','gsa'))

dict <- fread('Network_Innovation_Paper/data_products/node_dictionary.csv')
dict <- dict |> filter(entity_type %in% c('City','Company','County','District','Federal_Gov','Local_Gov','Local_GSA','NGO','Other_GSA','State_Gov','Group'))
company_melt <- gs_melt[gs_melt$variable %in% dict$name[dict$entity_type=='Company'],]
ngo_melt <- gs_melt[gs_melt$variable %in% dict$name[dict$entity_type=='NGO'],]
group_melt <- gs_melt[gs_melt$variable %in% dict$name[dict$entity_type=='Group'],]

group_df <- dcast(group_melt,gsp_id ~ variable, value.var = 'value',fun.aggregate = sum,na.rm = T,fill = 0)
group_mat <- as.matrix(group_df[,-1])
rownames(group_mat) <- group_df$gsp_id
group_mat <- group_mat[,{colSums(group_mat>0)/nrow(group_mat)}<.10]
gsp_group_mat <- tcrossprod(group_mat)


ngo_df <- dcast(ngo_melt,gsp_id ~ variable, value.var = 'value',fun.aggregate = sum,na.rm = T,fill = 0)
ngo_mat <- as.matrix(ngo_df[,-1])
rownames(ngo_mat) <- ngo_df$gsp_id
ngo_mat <- ngo_mat[,{colSums(ngo_mat>0)/nrow(ngo_mat)}<.10]
gsp_ngo_mat <- tcrossprod(ngo_mat)

company_df <- dcast(company_melt,gsp_id ~ variable, value.var = 'value',fun.aggregate = sum,na.rm = T,fill = 0)
company_mat <- as.matrix(company_df[,-1])
rownames(company_mat) <- company_df$gsp_id
company_mat <- company_mat[,{colSums(company_mat>0)/nrow(company_mat)}<.10]
gsp_company_mat <- tcrossprod(company_mat)


##### make reference similarity network #####
ref_dyads <- readRDS('Network_Innovation_Paper/data_products/gsp_reference_pairs.rds')
ref_dyads <- ref_dyads[grepl('^v1',V2),]
ref_dyads$V1 <- str_extract(ref_dyads$V1,'[0-9]{4}')
ref_dyads$V2 <- str_extract(ref_dyads$V2,'[0-9]{4}')
ref_cosine <- coop::cosine(as.matrix(unclass(table(ref_dyads$V1,ref_dyads$V2))))
ref_cosine <- ref_cosine[!rownames(ref_cosine) %in% bad,!colnames(ref_cosine) %in% bad]

n <- nrow(ref_cosine)
ref_net <- network.initialize(n, directed=FALSE)
network.vertex.names(ref_net) <- rownames(ref_cosine)
ref_net[,] <- 1
vertex_names <- network.vertex.names(ref_net)
ref_cosine_ordered <- ref_cosine[vertex_names, vertex_names]
edge_list <- as.edgelist(ref_net)
cosim_values <- ref_cosine_ordered[edge_list]
set.edge.attribute(ref_net, "cosim", cosim_values)


#### make knowledge graph similarity network ####
knowledge_df <- read.csv('Network_Innovation_Paper/data_products/triple_similarity.csv')
knowledge_df <- data.table(knowledge_df)
# Get all unique nodes
all_nodes <- unique(c(knowledge_df$X, knowledge_df$X.1))
# Create symmetric pairwise data (add reverse pairs)
knowledge_symmetric <- rbind(
  knowledge_df[,.(node1 = X, node2 = X.1, similarity = cached_contextual_semantic)],
  knowledge_df[,.(node1 = X.1, node2 = X, similarity = cached_contextual_semantic)]
)
knowledge_symmetric$node1 <- str_extract(knowledge_symmetric$node1,'[0-9]{4}')
knowledge_symmetric$node2 <- str_extract(knowledge_symmetric$node2,'[0-9]{4}')
knowledge_symmetric <- knowledge_symmetric[!node1 %in% bad & !node2 %in% bad,]
# Cast to square matrix
kn_cast <- dcast(knowledge_symmetric, node1 ~ node2, value.var = 'similarity', fill = NA)
kmat <- as.matrix(kn_cast[,-1])
rownames(kmat) <- kn_cast$node1
# Set diagonal to 1 (self-similarity)
diag(kmat) <- 1
n <- nrow(kmat)
kn_net <- network.initialize(n, directed=FALSE)
network.vertex.names(kn_net) <- rownames(kmat)
kn_net[,] <- 1
# Set edge attributes using vectorized approach
vertex_names <- network.vertex.names(kn_net)
kmat_ordered <- kmat[vertex_names, vertex_names]
edge_list <- as.edgelist(kn_net)
cosim_values <- kmat_ordered[edge_list]
set.edge.attribute(kn_net, "cosim", cosim_values)

jac_dyads <- readRDS('Network_Innovation_Paper/data_products/project_jaccard_results/project_section_jaccard_scores_20250903.rds')
#jac_dyads <- jac_dyads[grepl('^v1',b)&grepl('^v1',a),]
#jac_dyads$a <- str_extract(jac_dyads$a,'[0-9]{4}')
#jac_dyads$b <- str_extract(jac_dyads$b,'[0-9]{4}')
jac_cast <- dcast(jac_dyads, a ~ b, value.var = 'score',fill = NA)
jac_mat <- as.matrix(jac_cast[,-1])
rownames(jac_mat) <- jac_cast$a

jac_mat <- jac_mat[!rownames(jac_mat) %in% bad,!colnames(jac_mat) %in% bad]
n <- nrow(jac_mat)
jac_net <- network.initialize(n, directed=FALSE)
network.vertex.names(jac_net) <- rownames(jac_mat)
jac_net[,] <- 1
vertex_names <- network.vertex.names(jac_net)
jac_ordered <- jac_mat[vertex_names, vertex_names]
edge_list <- as.edgelist(jac_net)
jaccard_values <- jac_ordered[edge_list]
set.edge.attribute(jac_net,"jaccard",jaccard_values)








library(sf) 
library(spdep)
# Read the CSV file containing basin ids
basin_ids <- fread('EJ_DAC_Paper/Data/gsp_basin_ids.csv')
basin_ids$gsp_id <- formatC(basin_ids$gsp_id,width = 4,flag= '0')
# Disable S2 geometry
sf::sf_use_s2(FALSE)
# Read the GSP shapefile
gsp_bounds <- st_read("Multipurpose_Files/GSP_Submitted")
gsp_bounds <- sf::st_make_valid(gsp_bounds)
gsp_bounds$GSP.ID <- formatC(as.numeric(gsp_bounds$GSP.ID),width = 4,flag = '0')
#gsp_bounds$gsp_id <- formatC(as.numeric(gsp_bounds$GSP.ID), width = 4, flag = '0')
gsp_bounds <- gsp_bounds |> arrange(GSP.ID)
# Make sure GSP.ID is formatted correctly with 4 digits
neighbors_list <- poly2nb(gsp_bounds, queen = F,useC = T,row.names = gsp_bounds$GSP.ID)
neighbors_matrix <- nb2mat(neighbors_list,style = "B",zero.policy = T)
colnames(neighbors_matrix) <- rownames(neighbors_matrix)

ref_nb <- neighbors_matrix[network.vertex.names(ref_net),network.vertex.names(ref_net)]
kn_nb <- neighbors_matrix[network.vertex.names(kn_net),network.vertex.names(kn_net)]
jac_nb <- neighbors_matrix[network.vertex.names(jac_net),network.vertex.names(jac_net)]



ref_companyentities <- gsp_company_mat[network.vertex.names(ref_net),network.vertex.names(ref_net)]
ref_ngoentities <- gsp_ngo_mat[network.vertex.names(ref_net),network.vertex.names(ref_net)]
ref_groupentities <- gsp_group_mat[network.vertex.names(ref_net),network.vertex.names(ref_net)]
ref_totalentities <- ref_companyentities + ref_ngoentities + ref_groupentities 

kn_companyentities <- gsp_company_mat[network.vertex.names(kn_net),network.vertex.names(kn_net)]
kn_ngoentities <- gsp_ngo_mat[network.vertex.names(kn_net),network.vertex.names(kn_net)]
kn_groupentities <- gsp_group_mat[network.vertex.names(kn_net),network.vertex.names(kn_net)]
kn_totalentities <- kn_companyentities + kn_ngoentities + kn_groupentities 

jac_companyentities <- gsp_company_mat[network.vertex.names(jac_net),network.vertex.names(jac_net)]
jac_ngoentities <- gsp_ngo_mat[network.vertex.names(jac_net),network.vertex.names(jac_net)]
jac_groupentities <- gsp_group_mat[network.vertex.names(jac_net),network.vertex.names(jac_net)]
jac_totalentities <- jac_companyentities + jac_ngoentities + jac_groupentities 


ref_net %v% 'joint_agency' <- meta$exante_collab[match(network.vertex.names(ref_net),meta$gsp_id)]
ref_net %v% 'mult_gsa' <- meta$mult_gsas[match(network.vertex.names(ref_net),meta$gsp_id)]
ref_net %v% 'priority' <- meta$priority_category[match(network.vertex.names(ref_net),meta$gsp_id)]
ref_net %v% "Republican_Vote_Share" <- meta$Republican_Vote_Share[match(network.vertex.names(ref_net),meta$gsp_id)]
ref_net %v% "Agr_Share_Of_GDP" <- meta$Agr_Share_Of_GDP[match(network.vertex.names(ref_net),meta$gsp_id)]

kn_net %v% 'joint_agency' <- meta$exante_collab[match(network.vertex.names(kn_net),meta$gsp_id)]
kn_net %v% 'mult_gsa' <- meta$mult_gsas[match(network.vertex.names(kn_net),meta$gsp_id)]
kn_net %v% 'priority' <- meta$priority_category[match(network.vertex.names(kn_net),meta$gsp_id)]
kn_net %v% "Republican_Vote_Share" <- meta$Republican_Vote_Share[match(network.vertex.names(kn_net),meta$gsp_id)]
kn_net %v% "Agr_Share_Of_GDP" <- meta$Agr_Share_Of_GDP[match(network.vertex.names(kn_net),meta$gsp_id)]

jac_net %v% 'joint_agency' <- meta$exante_collab[match(network.vertex.names(jac_net),meta$gsp_id)]
jac_net %v% 'mult_gsa' <- meta$mult_gsas[match(network.vertex.names(jac_net),meta$gsp_id)]
jac_net %v% 'priority' <- meta$priority_category[match(network.vertex.names(jac_net),meta$gsp_id)]
jac_net %v% "Republican_Vote_Share" <- meta$Republican_Vote_Share[match(network.vertex.names(jac_net),meta$gsp_id)]
jac_net %v% "Agr_Share_Of_GDP" <- meta$Agr_Share_Of_GDP[match(network.vertex.names(jac_net),meta$gsp_id)]






mod0_ref_net <- ergm(ref_net ~ sum,response = 'cosim',reference=~Unif(min(get.edge.attribute(ref_net,'cosim')), 1),control = control.ergm(parallel = 8,MCMC.samplesize = 1e5))
mod0_kn_net <- ergm(kn_net ~ sum,response = 'cosim',reference=~Unif(min(get.edge.attribute(kn_net,'cosim')), 1),control = control.ergm(parallel = 8,MCMC.samplesize = 1e5))
mod0_jc_net <- ergm(jac_net ~ sum, response = 'jaccard',reference=~Unif(min(get.edge.attribute(jac_net,'jaccard')), 1),control = control.ergm(parallel = 8,MCMC.samplesize = 1e5))


mod1_ref_net <- ergm(ref_net ~ sum + transitiveweights(),response = 'cosim',reference=~Unif(min(get.edge.attribute(ref_net,'cosim')), 1),control = control.ergm(parallel = 8,MCMC.samplesize = 1e3))
mod1_kn_net <- ergm(kn_net ~ sum + transitiveweights(),response = 'cosim',reference=~Unif(min(get.edge.attribute(kn_net,'cosim')), 1),control = control.ergm(parallel = 8,MCMC.samplesize = 1e5))
mod1_jc_net <- ergm(jac_net ~ sum + transitiveweights(),response = 'jaccard',reference=~Unif(min(get.edge.attribute(jac_net,'jaccard')),1),control = control.ergm(parallel = 8,MCMC.samplesize = 1e5))

dir.create( 'Network_Innovation_Paper/data_products/rds_placeholders/')
saveRDS(list(ref_net,ref_nb,ref_totalentities,ref_companyentities,ref_groupentities,ref_ngoentities),file = 'Network_Innovation_Paper/data_products/rds_placeholders/ref_object.rds')
saveRDS(list(jac_net,jac_nb,jac_totalentities,jac_companyentities,jac_groupentities,jac_ngoentities),file = 'Network_Innovation_Paper/data_products/rds_placeholders/jac_object.rds')
saveRDS(list(kn_net,kn_nb,kn_totalentities,kn_companyentities,kn_groupentities,kn_ngoentities),file = 'Network_Innovation_Paper/data_products/rds_placeholders/kn_object.rds')


mod1_ref_net <- ergm(ref_net ~ sum + triangles + nodefactor('mult_gsa') + 
                        nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                        edgecov(ref_nb) + edgecov(ref_totalentities),response = 'cosim',reference=~Unif(min(get.edge.attribute(ref_net,'cosim')), 1),estimate = 'CD',control = control.ergm(parallel = 8,MCMC.samplesize = 1e5))
mod1_kn_net <- ergm(kn_net ~ sum + triangles + nodefactor('mult_gsa') + 
                       nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') + 
                       edgecov(kn_nb) + edgecov(kn_totalentities),response = 'cosim',reference=~Unif(min(get.edge.attribute(kn_net,'cosim')), 1),estimate = 'CD',,control = control.ergm(parallel = 8,MCMC.samplesize = 1e5))
mod1_jc_net <- ergm(jac_net ~ sum + triangles + nodefactor('mult_gsa') + 
                       nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                       edgecov(jac_nb) +  edgecov(jac_totalentities), response = 'jaccard',reference=~Unif(min(get.edge.attribute(jac_net,'jaccard')),1),estimate = 'CD',control = control.ergm(parallel = 8,MCMC.samplesize = 1e5))

library(texreg)
texreg::screenreg(list(mod1_ref_net,mod1_kn_net,mod1_jc_net),custom.model.names = c('references','knowledge.triples','5-grams'))


summary(mod1_jc_net)
gof(mod1_ref_net)
summary(get.edge.attribute(kn_net,'cosim'))



"Republican_Vote_Share_scaled" 


table(meta$gwsum,meta$priority_category)
table(meta$mult_gsas,meta$exante_collab)

   

ergm::search.ergmTerms(keywords = 'valued')



gsp <- <- readRDS('Network_Innovation_Paper/data_products/page_metadata.RDS')




summary(jac_dyads$score)

head(tt)

ref_cosine[diag(ref_cosine)] <- NA
ref_cosine_dummy <- (ref_cosine>=0) + 0
ref_net <- as.network(ref_cosine_dummy,matrix.type = 'adjacency',ignore.eval = F,names.eval = 'cosim',directed = F)
set.edge.attribute(x = ref_net, attrname = "cosim", e = , value = ref_cosine)

ref_cosine
ref_net

summary(get.edge.attribute(ref_net,'cosim'))

             
             
             control = control.ergm(init.method = "zeros",
                                    MCMLE.maxit = 10))

sum(ref_net != 0)


ref_net

hist(ref_cosine[ref_cosine > 0]) 
summary(c(ref_cosine))



summary(c(ref_cosine))


head(ref_cosine)
ref_dyads[,.N,by=(V2)]
 <- coop::cosine(as.matrix(table(ref_dyads$V2,ref_dyads$V1)))


library(proxy)
binary_matrix <- as.matrix(table(df$entity1, df$entity2))
binary_matrix[binary_matrix > 0] <- 1
sim_matrix <- proxy::simil(binary_matrix, method = "cosine")