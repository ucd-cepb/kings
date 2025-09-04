library(statnet)
library(coop)
library(data.table)
library(tidyverse)

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

# Calculate 0.9 quantile threshold for reference network
diag(ref_cosine) <- NA  # Exclude diagonal
ref_threshold <- quantile(ref_cosine, 0.9, na.rm = TRUE)
cat("Reference network 0.9 quantile threshold:", ref_threshold, "\n")

# Create binary adjacency matrix based on threshold
ref_binary <- (ref_cosine >= ref_threshold) * 1
diag(ref_binary) <- 0  # Set diagonal to 0
ref_net <- network(ref_binary, directed = FALSE)


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

# Calculate 0.9 quantile threshold for knowledge network
diag(kmat) <- NA  # Exclude diagonal for threshold calculation
kn_threshold <- quantile(kmat, 0.9, na.rm = TRUE)
cat("Knowledge network 0.9 quantile threshold:", kn_threshold, "\n")

# Create binary adjacency matrix based on threshold
kn_binary <- (kmat >= kn_threshold) * 1
diag(kn_binary) <- 0  # Set diagonal to 0
kn_net <- network(kn_binary, directed = FALSE)

jac_dyads <- readRDS('Network_Innovation_Paper/data_products/project_jaccard_results/project_section_jaccard_scores_20250903.rds')
#jac_dyads <- jac_dyads[grepl('^v1',b)&grepl('^v1',a),]
#jac_dyads$a <- str_extract(jac_dyads$a,'[0-9]{4}')
#jac_dyads$b <- str_extract(jac_dyads$b,'[0-9]{4}')
jac_cast <- dcast(jac_dyads, a ~ b, value.var = 'score',fill = NA)
jac_mat <- as.matrix(jac_cast[,-1])
rownames(jac_mat) <- jac_cast$a

jac_mat <- jac_mat[!rownames(jac_mat) %in% bad,!colnames(jac_mat) %in% bad]

# Calculate 0.9 quantile threshold for Jaccard network
diag(jac_mat) <- NA  # Exclude diagonal for threshold calculation
jac_threshold <- quantile(jac_mat, 0.9, na.rm = TRUE)
cat("Jaccard network 0.9 quantile threshold:", jac_threshold, "\n")

# Create binary adjacency matrix based on threshold
jac_binary <- (jac_mat >= jac_threshold) * 1
diag(jac_binary) <- 0  # Set diagonal to 0
jac_net <- network(jac_binary, directed = FALSE)


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


mod0_ref_net <- ergm(ref_net ~ edges + twopath + gwdegree(1,fixed = T) + gwdsp(1,fixed = T), control = control.ergm(parallel = 8, MCMC.samplesize = 1e3))
mod0_kn_net <- ergm(kn_net ~ edges + twopath + gwdegree(1,fixed = T) + gwdsp(1,fixed = T), control = control.ergm(parallel = 8, MCMC.samplesize = 1e3))
mod0_jc_net <- ergm(jac_net ~ edges + twopath + gwdegree(1,fixed = T) + gwdsp(1,fixed = T), control = control.ergm(parallel = 8, MCMC.samplesize = 1e3))


mod1_ref_net <- ergm(ref_net ~ edges + twopath + gwdegree(1,fixed = T) + gwdsp(1,fixed = T) + nodefactor('mult_gsa') + 
                        nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                        edgecov(ref_nb) + edgecov(ref_totalentities), control = control.ergm(parallel = 8, MCMC.samplesize = 1e5))
mod1_kn_net <- ergm(kn_net ~ edges + twopath + gwdegree(1,fixed = T) + gwdsp(1,fixed = T) + nodefactor('mult_gsa') + 
                       nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') + 
                       edgecov(kn_nb) + edgecov(kn_totalentities), control = control.ergm(parallel = 8, MCMC.samplesize = 1e5))
mod1_jc_net <- ergm(jac_net ~edges + twopath + gwdegree(1,fixed = T) + gwdsp(1,fixed = T) + nodefactor('mult_gsa') + 
                       nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                       edgecov(jac_nb) + edgecov(jac_totalentities), control = control.ergm(parallel = 8, MCMC.samplesize = 1e5))





dir.create( 'Network_Innovation_Paper/data_products/rds_placeholders/')
saveRDS(list(ref_net,ref_nb,ref_totalentities,ref_companyentities,ref_groupentities,ref_ngoentities),file = 'Network_Innovation_Paper/data_products/rds_placeholders/ref_object.rds')
saveRDS(list(jac_net,jac_nb,jac_totalentities,jac_companyentities,jac_groupentities,jac_ngoentities),file = 'Network_Innovation_Paper/data_products/rds_placeholders/jac_object.rds')
saveRDS(list(kn_net,kn_nb,kn_totalentities,kn_companyentities,kn_groupentities,kn_ngoentities),file = 'Network_Innovation_Paper/data_products/rds_placeholders/kn_object.rds')

library(texreg)
texreg::screenreg(list(mod2_ref_net, mod2_kn_net, mod2_jc_net), custom.model.names = c('references','knowledge.triples','5-grams'))


summary(mod2_jc_net)
gof(mod2_ref_net)



"Republican_Vote_Share_scaled" 


table(meta$gwsum,meta$priority_category)
table(meta$mult_gsas,meta$exante_collab)

   

ergm::search.ergmTerms(keywords = 'valued')



gsp <- <- readRDS('Network_Innovation_Paper/data_products/page_metadata.RDS')




summary(jac_dyads$score)

head(tt)

# Display network summary statistics
cat("Network summary statistics:\n")
cat("Reference network - nodes:", network.size(ref_net), "edges:", network.edgecount(ref_net), "\n")
cat("Knowledge network - nodes:", network.size(kn_net), "edges:", network.edgecount(kn_net), "\n")
cat("Jaccard network - nodes:", network.size(jac_net), "edges:", network.edgecount(jac_net), "\n")