library(statnet)
library(coop)
library(data.table)
library(tidyverse)

bad <- c('0053','0089')
source("Network_Innovation_Paper/Code/_paths.R")
source("Network_Innovation_Paper/Code/_corpus.R")   # latest_project_jaccard()
# Paper-owned political/economic covariates (NOT core; see inputs/README.md).
meta <- fread(nip_input('gsp_covariates.csv'), colClasses = c(gsp_id = 'character'))
meta <- meta[!duplicated(meta$gsp_id),]
meta$Republican_Vote_Share <- as.numeric(meta$Republican_Vote_Share)
meta$Agr_Share_Of_GDP <- as.numeric(meta$Agr_Share_Of_GDP)


gs_edge <- fread(nip_product('all_gsa_edges.csv'))
gs_edge$gsp_id <- formatC(gs_edge$gsp_id,width = 4,flag = '0')
gs_edge <- gs_edge[!gsp_id %in% bad,]
gs_melt <- melt(gs_edge,id.vars = c('gsp_id','gsa'))

source("Network_Innovation_Paper/Code/_entity_groups.R")
dict <- fread(nip_product('node_dictionary.csv'))

gs_melt <- gs_melt[variable != 'inter_agency',]

# Entity co-mention structure, keyed to the semantic entity types:
#   generic    = all institutional actors (spaCy ORG + GPE + NORP, junk-pruned)
#   consultant / research / ngo = the three focal subnetworks, each on its own
#   crn        = the 4th "grouped" run: the three focal types pooled
gsp_generic_mat    <- build_shared_entity_matrix(gs_melt, entity_names(dict, 'institutional'))
gsp_consultant_mat <- build_shared_entity_matrix(gs_melt, entity_names(dict, 'Consultant'))
gsp_research_mat   <- build_shared_entity_matrix(gs_melt, entity_names(dict, 'Research'))
gsp_ngo_mat        <- build_shared_entity_matrix(gs_melt, entity_names(dict, 'NGO'))
gsp_crn_mat        <- build_shared_entity_matrix(gs_melt, entity_names(dict, 'consultant_research_ngo'))

##### make reference similarity network #####
ref_dyads <- readRDS(nip_product('gsp_reference_pairs.rds'))
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
knowledge_df <- read.csv(nip_product('triple_similarity.csv'))
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

jac_dyads <- readRDS(latest_project_jaccard())
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
basin_ids <- fread(nip_input('gsp_basin_ids.csv'))
basin_ids$gsp_id <- formatC(basin_ids$gsp_id,width = 4,flag= '0')
# Disable S2 geometry
sf::sf_use_s2(FALSE)
# Read the GSP shapefile
gsp_bounds <- st_read(nip_input("GSP_Submitted"))
gsp_bounds <- sf::st_make_valid(gsp_bounds)
gsp_bounds$GSP.ID <- formatC(as.numeric(gsp_bounds$GSP.ID),width = 4,flag = '0')
#gsp_bounds$gsp_id <- formatC(as.numeric(gsp_bounds$GSP.ID), width = 4, flag = '0')
gsp_bounds <- gsp_bounds |> arrange(GSP.ID) |> filter(!GSP.ID %in% bad)
# Make sure GSP.ID is formatted correctly with 4 digits
neighbors_list <- poly2nb(gsp_bounds, queen = F,useC = T,row.names = gsp_bounds$GSP.ID)
neighbors_matrix <- nb2mat(neighbors_list,style = "B",zero.policy = T)
colnames(neighbors_matrix) <- rownames(neighbors_matrix)

ref_nb <- neighbors_matrix[network.vertex.names(ref_net),network.vertex.names(ref_net)]
kn_nb <- neighbors_matrix[network.vertex.names(kn_net),network.vertex.names(kn_net)]
jac_nb <- neighbors_matrix[network.vertex.names(jac_net),network.vertex.names(jac_net)]



ref_genericentities    <- align_gsp_matrix(gsp_generic_mat,    network.vertex.names(ref_net))
ref_consultantentities <- align_gsp_matrix(gsp_consultant_mat, network.vertex.names(ref_net))
ref_researchentities   <- align_gsp_matrix(gsp_research_mat,   network.vertex.names(ref_net))
ref_ngoentities        <- align_gsp_matrix(gsp_ngo_mat,        network.vertex.names(ref_net))
ref_crnentities        <- align_gsp_matrix(gsp_crn_mat,        network.vertex.names(ref_net))
ref_totalentities      <- ref_genericentities

kn_genericentities    <- align_gsp_matrix(gsp_generic_mat,    network.vertex.names(kn_net))
kn_consultantentities <- align_gsp_matrix(gsp_consultant_mat, network.vertex.names(kn_net))
kn_researchentities   <- align_gsp_matrix(gsp_research_mat,   network.vertex.names(kn_net))
kn_ngoentities        <- align_gsp_matrix(gsp_ngo_mat,        network.vertex.names(kn_net))
kn_crnentities        <- align_gsp_matrix(gsp_crn_mat,        network.vertex.names(kn_net))
kn_totalentities      <- kn_genericentities

jac_genericentities    <- align_gsp_matrix(gsp_generic_mat,    network.vertex.names(jac_net))
jac_consultantentities <- align_gsp_matrix(gsp_consultant_mat, network.vertex.names(jac_net))
jac_researchentities   <- align_gsp_matrix(gsp_research_mat,   network.vertex.names(jac_net))
jac_ngoentities        <- align_gsp_matrix(gsp_ngo_mat,        network.vertex.names(jac_net))
jac_crnentities        <- align_gsp_matrix(gsp_crn_mat,        network.vertex.names(jac_net))
jac_totalentities      <- jac_genericentities


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

library(bigergm)
library(Bergm)


vclass <- sapply(list.vertex.attributes(jac_net),function(x) class(jac_net %v% x))

sapply(list.vertex.attributes(jac_net),function(x) summary(jac_net %v% x))[vclass=='numeric']
sapply(list.vertex.attributes(jac_net),function(x) table(jac_net %v% x))[vclass!='numeric'][c('joint_agency','mult_gsa','priority')]


isSymmetric(jac_totalentities)
lapply(list(jac_totalentities,jac_consultantentities,jac_researchentities,jac_ngoentities,jac_crnentities),function(x) summary(c(x[upper.tri(x)])))


mlabs <- c("Text of management actions",
           "Knowledge graph of sustainability",
           "Scientific references")

length(c(jac_totalentities[upper.tri(jac_totalentities)]))

g1 <- ggplot() + geom_histogram(aes(x = c(jac_mat))) + theme_bw() + xlab(mlabs[1]) +
   geom_vline(xintercept = jac_threshold,lty = 2)

g2 <- ggplot() + geom_histogram(aes(x = c(kmat))) + theme_bw() + xlab(mlabs[2]) +
   geom_vline(xintercept = kn_threshold,lty = 2)

g3 <- ggplot() + geom_histogram(aes(x = c(ref_cosine))) + theme_bw() + xlab(mlabs[3]) +
   geom_vline(xintercept = ref_threshold,lty = 2)

library(gridExtra)

ggsave(plot = grid.arrange(g1,g2,g3,ncol = 2,
             top = 'Distribution of dyadic values and 0.9 quantile threshold'),
       filename = 'Network_Innovation_Paper/data_products/figure1_dv_distributions.png',units = 'in',dpi = 450,width = 6, height = 6)



mod0_ref_net <- bergm(ref_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(.5,fixed = T),
                      offset.coef = log(network.density(ref_net)), 
                      prior.mean = rep(1,4), 
                     prior.sigma = diag(10,4),
                      main.iters = 1e3,nchains = 10,burn.in = 1e3)
mod0_kn_net <- bergm(kn_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(.5,fixed = T),
                     offset.coef = log(network.density(kn_net)), 
                     prior.mean = rep(1,4), 
                     prior.sigma = diag(10,4),
                     main.iters = 1e3,nchains = 10,burn.in = 1e3)
mod0_jc_net <- bergm(jac_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(.5,fixed = T),
                     offset.coef = log(network.density(jac_net)), 
                     prior.mean = rep(1,4), 
                     prior.sigma = diag(10,4),
                     main.iters = 1e3,nchains = 10,burn.in = 1e3)


m1_ref_priors <- c(colMeans(mod0_ref_net$Theta),rep(0,7))
m1_ref_sigma <-diag(10,length(m1_ref_priors))
mod1_ref_net <- bergm(ref_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) + 
   nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                        edgecov(ref_nb[network.vertex.names(ref_net),network.vertex.names(ref_net)]) + 
      edgecov(ref_totalentities[network.vertex.names(ref_net),network.vertex.names(ref_net)]), 
                     offset.coef = log(network.density(ref_net)), 
                     prior.mean = m1_ref_priors,
                     prior.sigma = m1_ref_sigma,
                     main.iters = 1e3,nchains = 10,burn.in = 1e3)
                     
m1_kn_priors <- c(colMeans(mod0_kn_net$Theta),rep(0,7))
m1_kn_sigma <-diag(10,length(m1_kn_priors))   
mod1_kn_net <- bergm(kn_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) + 
                       nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                       edgecov(ref_nb[network.vertex.names(kn_net),network.vertex.names(kn_net)]) + edgecov(ref_totalentities[network.vertex.names(kn_net),network.vertex.names(kn_net)]), 
                    offset.coef = log(network.density(kn_net)), 
                    prior.mean = m1_kn_priors,
                    prior.sigma = m1_kn_sigma,
                    main.iters = 1e3,nchains = 10,burn.in = 1e3)


par(mfrow = c(1,1))
quantile(c(jac_totalentities),c(0.99))
rowSums(jac_totalentities)


m1_jc_priors <- c(colMeans(mod0_jc_net$Theta),rep(0,7))
m1_jc_sigma <-diag(10,length(m1_jc_priors))   
mod1_jc_net <- bergm(jac_net ~offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) + 
                        nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                        edgecov(jac_nb[network.vertex.names(jac_net),network.vertex.names(jac_net)]) + 
                        edgecov(jac_totalentities[network.vertex.names(jac_net),network.vertex.names(jac_net)]), 
                     offset.coef = log(network.density(jac_net)), 
                     prior.mean = m1_jc_priors,
                     prior.sigma = m1_jc_sigma,
                     main.iters = 1e3,nchains = 10,burn.in = 1e3)

m1_dt <- rbindlist(list(
     data.table(model = mlabs[1],mod1_jc_net$specs,t(apply(mod1_jc_net$Theta,2,quantile,c(0.025,0.975))),colMeans(mod1_jc_net$Theta)),
     data.table(model= mlabs[2],mod1_kn_net$specs,t(apply(mod1_kn_net$Theta,2,quantile,c(0.025,0.975))),colMeans(mod1_kn_net$Theta)),
     data.table(model = mlabs[3],mod1_ref_net$specs,t(apply(mod1_ref_net$Theta,2,quantile,c(0.025,0.975))),colMeans(mod1_ref_net$Theta))
     )
   )

setnames(m1_dt,c('V2','V4'),c('Coef','mean'))
m1_dt$Coef <- rep(c("edges (fixed)","twopath","gwdegree(decay = 1)","gwdsp(decay = 0.5)",
  "Mult-GSA GSP","Low/v. low priority","medium priority","Repub. vote share %",
  "Agr. % of local GDP",
  "Neighbor","Shared connection weight"),3)



#m1_dt$V2 <- str_remove(m1_dt$V2,'\\[.+\\]')
#m1_dt$V2 <- str_replace(m1_dt$V2,'edgecov\\.[a-z]{1,}_nb','neighbors')
#m1_dt$V2 <- str_replace(m1_dt$V2,'edgecov\\.[a-z]{1,}_totalentities','shared entities^2')
m1_dt$model <- rep(mlabs,each = nrow(m1_dt)/3)
library(forcats)

m1_dt$Coef <- fct_rev(fct_inorder(m1_dt$Coef))

m1_dt$INSIG <- (m1_dt$`2.5%`<0 & m1_dt$`97.5%`>0)
m1_dt$model <- fct_inorder(m1_dt$model)

(gg_mod1 <- ggplot(data = m1_dt) + 
   ggtitle('Plan similarity predicted by position in latent social space')+
   geom_vline(xintercept = 0,lty = 2,col = 'grey50') + 
   geom_errorbarh(aes(xmin = `2.5%`,xmax = `97.5%`,y = Coef),height = 0.25) + 
   geom_point(aes(x = mean,y = Coef,fill = INSIG),pch = 21) +
   facet_wrap(~model,ncol = 2) + theme_bw() + 
   scale_x_continuous('Posterior mean and 95% credible interval')+
   theme(axis.title.y = element_blank(),legend.background = element_rect(fill = alpha('white',0.5)),
         legend.position = 'inside',text = element_text(family = 'Times'),
         legend.position.inside = c(0.85,0.1)) + 
   labs(caption = '**values in connection^2 units') + 
   scale_fill_manual(name = '95% CI spans 0',values = c('black','white')))

ggsave(gg_mod1,filename = 'Network_Innovation_Paper/data_products/model1_plot.png',dpi = 450,units = 'in',height = 6,width = 6)

texreg::htmlreg(list(mod1_jc_net,mod1_kn_net,mod1_ref_net),
                custom.coef.names = c("edges (fixed)","twopath","gwdegree(decay = 1)","gwdsp(decay = 0.5)",
                                      "Mult-GSA GSP","Low/v. low priority","medium priority","Repub. vote share %",
                                      "Agr. % of local GDP",
                                      "Neighbor","Shared connection weight","Neighbor","Shared connection weight","Neighbor","Shared connection weight"),
                custom.model.names = mlabs,custom.note = '*0 not within 95% credible interval\n**^2 is due to cross-product calculation',
                file = 'Network_Innovation_Paper/data_products/mod1_html.html',
                single.row = T)



# Model 2 (disaggregated focal subnetworks): the three focal entity types entered
# separately -- consultant, research, ngo. (Each *_<type>entities matrix is
# already aligned to its network's vertex order, so no re-subsetting is needed.)
m2_ref_priors <- c(colMeans(mod0_ref_net$Theta),rep(0,9))
m2_ref_sigma <-diag(10,length(m2_ref_priors))
mod2_ref_net <- bergm(ref_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) +
                         nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                         edgecov(ref_nb) +
                         edgecov(ref_consultantentities) +
                         edgecov(ref_researchentities) +
                         edgecov(ref_ngoentities),
                      offset.coef = log(network.density(ref_net)),
                      prior.mean = m2_ref_priors,
                      prior.sigma = m2_ref_sigma,
                      main.iters = 1e3,nchains = 10,burn.in = 1e3)

m2_kn_priors <- c(colMeans(mod0_kn_net$Theta),rep(0,9))
m2_kn_sigma <-diag(10,length(m2_kn_priors))
mod2_kn_net <- bergm(kn_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) +
                        nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                        edgecov(kn_nb) +
                        edgecov(kn_consultantentities) +
                        edgecov(kn_researchentities) +
                        edgecov(kn_ngoentities),
                     offset.coef = log(network.density(kn_net)),
                     prior.mean = m2_kn_priors,
                     prior.sigma = m2_kn_sigma,
                     main.iters = 1e3,nchains = 10,burn.in = 1e3)

m2_jc_priors <- c(colMeans(mod0_jc_net$Theta),rep(0,9))
m2_jc_sigma <-diag(10,length(m2_jc_priors))
mod2_jc_net <- bergm(jac_net ~offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) +
                        nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                        edgecov(jac_nb) +
                        edgecov(jac_consultantentities) +
                        edgecov(jac_researchentities) +
                        edgecov(jac_ngoentities),
                     offset.coef = log(network.density(jac_net)),
                     prior.mean = m2_jc_priors,
                     prior.sigma = m2_jc_sigma,
                     main.iters = 1e3,nchains = 10,burn.in = 1e3)



m2_dt <- rbindlist(list(data.table(model = mlabs[1],mod2_jc_net$specs,t(apply(mod2_jc_net$Theta,2,quantile,c(0.025,0.975))),colMeans(mod2_jc_net$Theta)),
                        data.table(model = mlabs[2],mod2_kn_net$specs,t(apply(mod2_kn_net$Theta,2,quantile,c(0.025,0.975))),colMeans(mod2_kn_net$Theta)),
                        data.table(model = mlabs[3],mod2_ref_net$specs,t(apply(mod2_ref_net$Theta,2,quantile,c(0.025,0.975))),colMeans(mod2_ref_net$Theta))
))

setnames(m2_dt,c('V2','V4'),c('Coef','mean'))


m2_dt$Coef <-rep(c("edges (fixed)","twopath","gwdegree(decay = 1)","gwdsp(decay = 0.5)",
                    "Mult-GSA GSP","Low/v. low priority","medium priority","Repub. vote share %",
                    "Agr. % of local GDP",
                    "Neighbor","Shared consultant weight","Shared research weight","Shared NGO weight"),3)
               
#m2_dt$V2 <- str_remove(m2_dt$V2,'\\[.+\\]')
#m2_dt$V2 <- str_replace(m2_dt$V2,'edgecov\\.[a-z]{1,}_nb','neighbors')
#m2_dt$V2 <- str_replace(m2_dt$V2,'edgecov\\.[a-z]{1,}_totalentities','shared entities^2')

m2_dt$model <- rep(mlabs,each = nrow(m2_dt)/3)

m2_dt$Coef <- fct_rev(fct_inorder(m2_dt$Coef))

m2_dt$INSIG <- (m2_dt$`2.5%`<0 & m2_dt$`97.5%`>0)
m2_dt$model <- fct_inorder(m2_dt$model)
(gg_mod2 <- ggplot(data = m2_dt) + 
      ggtitle('Plan similarity predicted by common external entities')+
      geom_vline(xintercept = 0,lty = 2,col = 'grey50') + 
      geom_errorbarh(aes(xmin = `2.5%`,xmax = `97.5%`,y = Coef),height = 0.25) + 
      geom_point(aes(x = mean,y = Coef,fill = INSIG),pch = 21) +
      facet_wrap(~model,ncol = 2) + theme_bw() + 
      scale_x_continuous('Posterior mean and 95% credible interval')+
      theme(axis.title.y = element_blank(),legend.background = element_rect(fill = alpha('white',0.5)),
            legend.position = 'inside',text = element_text(family = 'Times'),
            legend.position.inside = c(0.85,0.1)) + 
      labs(caption = '**values in connection^2 units') + 
      scale_fill_manual(name = '95% CI spans 0',values = c('black','white')))

ggsave(gg_mod2,filename = 'Network_Innovation_Paper/data_products/model2_plot.png',dpi = 450,units = 'in',height = 6.25,width = 6)




texreg::htmlreg(list( mod2_jc_net, mod2_kn_net,mod2_ref_net),
                custom.coef.names = c("edges (fixed)","twopath","gwdegree(decay = 1)","gwdsp(decay = 0.5)",
                                      "Mult-GSA GSP","Low/v. low priority","medium priority","Repub. vote share %",
                                      "Agr. % of local GDP",
                                      "Neighbor","Shared consultant weight","Shared research weight","Shared NGO weight",
                                      "Neighbor","Shared consultant weight","Shared research weight","Shared NGO weight",
                                      "Neighbor","Shared consultant weight","Shared research weight","Shared NGO weight"),
                custom.model.names = mlabs,custom.note = '*0 not within 95% credible interval\n**^2 is due to cross-product calculation',
                file = 'Network_Innovation_Paper/data_products/mod2_html.html',single.row = T)

# =====================================================================
# Model 3 (grouped focal subnetworks): the 4th run -- consultant, research and
# NGO POOLED into a single "focal external entities" edge covariate (crn).
# =====================================================================
m3_ref_priors <- c(colMeans(mod0_ref_net$Theta),rep(0,7))
m3_ref_sigma  <- diag(10,length(m3_ref_priors))
mod3_ref_net <- bergm(ref_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) +
                        nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                        edgecov(ref_nb) + edgecov(ref_crnentities),
                      offset.coef = log(network.density(ref_net)),
                      prior.mean = m3_ref_priors, prior.sigma = m3_ref_sigma,
                      main.iters = 1e3,nchains = 10,burn.in = 1e3)

m3_kn_priors <- c(colMeans(mod0_kn_net$Theta),rep(0,7))
m3_kn_sigma  <- diag(10,length(m3_kn_priors))
mod3_kn_net <- bergm(kn_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) +
                       nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                       edgecov(kn_nb) + edgecov(kn_crnentities),
                     offset.coef = log(network.density(kn_net)),
                     prior.mean = m3_kn_priors, prior.sigma = m3_kn_sigma,
                     main.iters = 1e3,nchains = 10,burn.in = 1e3)

m3_jc_priors <- c(colMeans(mod0_jc_net$Theta),rep(0,7))
m3_jc_sigma  <- diag(10,length(m3_jc_priors))
mod3_jc_net <- bergm(jac_net ~ offset(edges) + twopath + gwdegree(1,fixed = T) + gwdsp(0.5,fixed = T) +
                       nodefactor('mult_gsa') + nodefactor('priority') + nodecov('Republican_Vote_Share') + nodecov('Agr_Share_Of_GDP') +
                       edgecov(jac_nb) + edgecov(jac_crnentities),
                     offset.coef = log(network.density(jac_net)),
                     prior.mean = m3_jc_priors, prior.sigma = m3_jc_sigma,
                     main.iters = 1e3,nchains = 10,burn.in = 1e3)

texreg::htmlreg(list(mod3_jc_net,mod3_kn_net,mod3_ref_net),
                custom.coef.names = c("edges (fixed)","twopath","gwdegree(decay = 1)","gwdsp(decay = 0.5)",
                                      "Mult-GSA GSP","Low/v. low priority","medium priority","Repub. vote share %",
                                      "Agr. % of local GDP",
                                      "Neighbor","Shared consultant/research/NGO weight",
                                      "Neighbor","Shared consultant/research/NGO weight",
                                      "Neighbor","Shared consultant/research/NGO weight"),
                custom.model.names = mlabs,custom.note = '*0 not within 95% credible interval\n**^2 is due to cross-product calculation',
                file = 'Network_Innovation_Paper/data_products/mod3_html.html',single.row = T)





mod1_ref_net <- str_remove(mod1_ref_net$specs ,'\\[[^0-9]+\\]')
mod1_jc_net <- str_remove(mod1_jc_net$specs ,'\\[[^0-9]+\\]')
mod1_kn_net <- str_remove(mod1_kn_net$specs ,'\\[[^0-9]+\\]')
mod2_ref_net <- str_remove(mod2_ref_net$specs ,'\\[[^0-9]+\\]')
mod2_jc_net <- str_remove(mod2_jc_net$specs ,'\\[[^0-9]+\\]')
mod2_kn_net <- str_remove(mod2_kn_net$specs ,'\\[[^0-9]+\\]')

library(texreg)
texreg::htmlreg(list(mod0_ref_net, mod0_kn_net, mod0_jc_net), custom.model.names = mlabs,
                file = 'Network_Innovation_Paper/data_products/mod0_html.html',single.row = T)


