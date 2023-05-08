
generate_phrases <- FALSE
test_data <- FALSE

library(reticulate)
library(spacyr)
library(tidyverse)
library(tidytext)
library(quanteda)
library(pbapply)
source('code/functions/generate_proper_names.R')

#prerequisites: step 1, install python
#step 2, install miniconda from https://conda.io/miniconda.html
#step 3 (unsure if this was required) I installed virtualenv, numpy, conda, and spacy
Sys.setenv(RETICULATE_PYTHON="/Users/elisemiller/miniconda3/envs/spacy_condaenv/bin/python")
py_config()
#spacy_install()
#spacy_download_langmodel(model = 'en_core_web_lg')
spacy_initialize(model = "en_core_web_lg")
pr_names_grouped <- generate_proper_names(underscore = T,to_lower=F)

if(generate_phrases){
   pr_names_sep <- generate_proper_names(underscore = F,to_lower=F)
   gsp_text_with_meta <- readRDS("data_output/gsp_docs_w_meta")
   gsp_text_with_meta$text <- pblapply(1:length(gsp_text_with_meta$text), function(i){
      stri_replace_all_regex(gsp_text_with_meta$text[i], pattern = pr_names_sep,
                             replacement = pr_names_grouped,
                             vectorize= F)
   })
   saveRDS(gsp_text_with_meta, "data_output/prepped_for_sna")
}

if(test_data==TRUE){
   test_collabs <- ("Invalid_sentence. Only one organization exists. National_Conservation_Service and US_Dept_of_Ag are to work together on providing the GSA the appropriate regulation language. Yolo County is to send the GSA all correspondence related to the basin setting. They shall report to the Board. The CDC will collaborate with NASA on the project. NRA's partners will include the FBI and several other agencies. The GSA is to submit their plan to the consultant. When the NSA meets with organizations such as the SWRCB, it is to take care to copy them on all correspondence. If they partner together, the GDE plan must be documented. The GSAs may not outsource their work to any other organizations. Sacramento Water Board may work with other partners as deemed necessary. Davis City Council may decide to incorporate the recommendations of BCDC.")
   
   test_parse <- spacy_parse(test_collabs,
                             pos = T,
                             tag = T,
                             lemma = T,
                             entity = T,
                             dependency = T,
                             nounphrase = T)
   valid_test_sentences <- test_parse  %>% group_by(doc_id, sentence_id) %>% 
      filter(any(dep_rel == "nsubj" | dep_rel == "nsubjpass") & 
                any(dep_rel == "pobj" | dep_rel == "iobj" | dep_rel == "dative" | dep_rel == "dobj"))
   
}


gsp_text_with_meta <- readRDS("data_output/prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)
gsp_planonly <- gsp_text_with_meta[is_comment==FALSE & is_reference==FALSE]



for (m in 1:length(gspids)){
   if(!file.exists(paste0("data_temp/parsed_",gspids[m]))){
      single_plan_text <- unlist(gsp_planonly[gsp_id==gspids[m]]$text)
      parsedtxt <- spacy_parse(single_plan_text,
                               pos = T,
                               tag = T,
                               lemma = T,
                               entity = T,
                               dependency = T,
                               nounphrase = T)
      saveRDS(parsedtxt, paste0("data_temp/parsed_",gspids[m]))
   }
   
   parsedtxt <- readRDS(paste0("data_temp/parsed_",gspids[m]))
   #part 1: POS tagging with spacy
   
   #part 2: identify entities
   entities <- entity_extract(parsedtxt,type="all")
   
   #removes invalid sentences with no root
   #only keeps sentences that have a subject and object
   #does not keep sentences with compound subject and no object
   valid_sentences <- parsedtxt %>% group_by(doc_id, sentence_id) %>% 
      filter(any(dep_rel == "ROOT") &
                any(dep_rel == "nsubj" | dep_rel == "nsubjpass") & 
                any(dep_rel == "pobj" | dep_rel == "iobj" | dep_rel == "dative" | dep_rel == "dobj"))
   
   
   #spacy_data %>% group_by(sentence_id, doc_id) %>% summarize(tally(pos==nsubj>0),tally(pos==nobj>0))
   
   #step 2 (opt) dependency parsing (disambiguating pronouns)
   
   #step 3 types of verbs -- analyzing types, groupings
   #Verb Group 13: Change of Possession
   #Verb Group 15: Hold and Keep
   #Verb Group 26: Verbs of Creation and Transformation
   #Verb Group 55: Aspectual Verbs
   #Verb Group 63: Enforce Verbs
   #Verb Group 68: Pay Verbs
   #Verb Group 70: Rely Verbs
   #Verb Group 71: Conspire Verbs
   #Verb Group 72: Help Verbs
   #Verb Group 73: Cooperate Verbs
   #Verb Group 86: Correlating and Relating Verbs
   #Verb Group 93: Adopt Verbs
   #Verb Group 102: Promote Verbs
   #Verb Group 104: Spending Time Verbs
   #Verb Group 105: Use Verbs
   #Verb Group 107: Involve Verbs
   
   #Support? Teach? Educate?
   
   
   #step 4 = Uses Verbnet database from Univ of Colorado, nature of verbs, categorization. match with verb cats from verbnet
   #verbnet <- xmlToList("data_raw/verbnet3.3/cooperate-73.1.xml")
   #verbnet2 <- read_xml("data_raw/verbnet3.3/cooperate-73.1.xml")
   #xml_find_all(verbnet2, "/pathway//entry")
   
   #step 5 (opt) lemmatization - how do we chop the verbs? do we use lemma or not
   #step 6 (opt) hedging and polarity (should be able to rely on POS tagging)
   
   #step 7 cross-reference database of agency names
   
   
   
   #create adjacency matrix of entities and orgs
   entities_subset <- unique(entities[entities$entity_type=="ORG" | 
                                         entities$entity_type=="GPE" | 
                                         entities$entity_type=="PERSON",][,'entity'])
   
   
   
   
   #later add:     conduct hedging detection on verbs
   #later add:     conduct polarity detection on verbs
   #part 5: use event extraction (define event) based on 
   #later add        non-hedging, positive verb relationships between entities (Bethard and Martin, 2006)
   #verb net lexicon -- dictionary of verbs
   #focus on one or two plans
   #1 million char max
   #could call pdftotext, then run cleaning, then collapse into giant string, then tokenizer
   #deal with generic nouns when we scale up eg county that refer to different counties
   
   adj_mat <- matrix(0, nrow = length(entities_subset) + length(pr_names_grouped), 
                     ncol = length(entities_subset) + length(pr_names_grouped),
                     dimnames = list(c(entities_subset, pr_names_grouped),c(entities_subset, pr_names_grouped))
   )
   #if sentence contains 2+
   #"ORG" "GPE" "PERSON" or matches agency database
   #connected_entities <- valid_sentences  %>% filter(sum(
   #              entity=="ORG_I" | 
   #             entity=="ORG_B" | 
   #            entity=="GPE_I" | 
   #           entity=="GPE_B" |
   #          entity=="PERSON_I" |
   #         entity=="PERSON_B" |
   #        token %in% pr_names_grouped)>1)
   entities_mini_df <- entities[entities$entity_type=="ORG" | 
                                   entities$entity_type=="GPE" | 
                                   entities$entity_type=="PERSON"|
                                   entities$entity %in% pr_names_grouped,]
   
   entities_tiny_df <- entities_mini_df %>% 
      filter(doc_id %in% valid_sentences$doc_id & sentence_id %in% valid_sentences$sentence_id)
   
   #only_entities <- connected_entities %>% filter(entity=="ORG_I" | 
   #                                                 entity=="ORG_B" | 
   #                                                entity=="GPE_I" | 
   #                                               entity=="GPE_B" |
   #                                              entity=="PERSON_I" |
   #                                             entity=="PERSON_B" |
   #                                            token %in% pr_names_grouped) 
   
   entities_grouped <- entities_tiny_df %>% group_by(doc_id, sentence_id)
   #TODO unique of entities_grouped so as to not have loops
   #then increment edge between entities in adjacency matrix by 1
   
   combos <- group_map(entities_grouped, ~ if(length(.x$entity)>1)combn(.x$entity,2))
   
   for(s in 1:length(combos)){
      if(!is.null(combos[[s]])){
         for(i in 1:ncol(combos[[s]])){
            if(combos[[s]][1,i]!=combos[[s]][2,i]){
               adj_mat[combos[[s]][1,i],combos[[s]][2,i]] = adj_mat[combos[[s]][1,i],combos[[s]][2,i]] + 1
               adj_mat[combos[[s]][2,i],combos[[s]][1,i]] = adj_mat[combos[[s]][2,i],combos[[s]][1,i]] + 1
               
            }
         }
      }
   }

   #preserving original adj_mat
   adj_mat_orig <- adj_mat
   if(!file.exists(paste0("data_temp/adjmat_orig_",gspids[m]))){
      saveRDS(adj_mat_orig, paste0("data_temp/adjmat_orig_",gspids[m]))
   }
  adj_mat_orig <- readRDS(paste0("data_temp/adjmat_orig_",gspids[m]))
  adj_mat <- readRDS(paste0("data_temp/adjmat_orig_",gspids[m]))
  
   library(network)
   
   if(!file.exists(paste0("data_output/network_",gspids[m]))){
      #removing node clusters who are only connected to other nodes in their same sentence
      #since this likely either a parsing mistake or a restating of the entity name in a different way
      #TODO fix this hyperedge removal algorithm 
      #determine how many sentences agency_q is found in and continue if only found in one sentence
         sentnc <- which(entities$entity==rownames(adj_mat)[q])
      if(length(sentnc)==1){
            doc <- entities$doc_id[sentnc]
            sen <- entities$sentence_id[sentnc]
      }
      
      #then agency_q is tagged "in_single_sentence"
      #if all agencies in a sentence are "in_single_sentence", set their whole column and row = 0
      for(i in 1:nrow(adj_mat)){
         if(sum(adj_mat[i,])==1 & (ncol(adj_mat)>i && adj_mat[i,i+1]==1)){
            adj_mat[i,i+1] <- 0
            adj_mat[i+1,i] <- 0
         }
         if(sum(adj_mat[i,])==1 & (i > 1 && adj_mat[i,i-1]==1)){
            adj_mat[i,i-1] <- 0
            adj_mat[i-1,i] <- 0
         }
      }
      
      #testing to see if it worked. Should only print organizations who were in the middle of a cluster of three
      #for(i in 1:nrow(adj_mat)){
      #   if(sum(adj_mat[i,])==1 & ((ncol(adj_mat)>i && adj_mat[i,i+1]==1) | (i > 1 && adj_mat[i,i-1]==1))){
      #      print(i)
      #   }
      #}
      
      adj_mat_subset <- adj_mat[rowSums(adj_mat)>0,]
      adj_mat_subset <- adj_mat_subset[,colSums(adj_mat_subset)>0]
      
      agency_net <- network::network(adj_mat_subset, matrix.type = "adjacency", 
                                     directed = FALSE, ignore.eval = F, names.eval = "weights")
      
      saveRDS(agency_net, paste0("data_output/network_",gspids[m]))
   } else{
     agency_net <- readRDS(paste0("data_output/network_",gspids[m]))
   }
   
   library(sna)
   name <- network::get.vertex.attribute(agency_net, "vertex.names")
   
   closens <- sna::closeness(agency_net, gmode = "graph", cmode="undirected")
   
   between <- sna::betweenness(agency_net,gmode = "graph",cmode="undirected")
   
   deg <- sna::degree(agency_net, gmode = "graph", cmode = "undirected")
   
   eigenvector <- sna::evcent(agency_net, gmode = "graph")
   #why does GSA boards have degree of 0?
   
   centr_df <- tibble(name, closens, between, deg, eigenvector)
   
   
   if(!file.exists(paste0("data_temp/centrality_",gspids[m]))){
      saveRDS(centr_df, paste0("data_temp/centrality_",gspids[m]))
   }

   network_properties <- c(sna::connectedness(agency_net),sna::centralization(agency_net,sna::degree))
   names(network_properties) <- c("connectedness", "centralization")
   if(!file.exists(paste0("data_temp/network_properties_",gspids[m]))){
      saveRDS(network_properties, paste0("data_temp/network_properties_",gspids[m]))
   }
   
   #can also use gplot instead of ggnet2, but ggnet2 is pretty cool
   library(ggplot2)
   library(GGally)
   #detach(package:GGally, unload = TRUE)
   #ggraph is for igraph in ggplot
   gov_net <- ggnet2(agency_net, node.size=3, node.color = "black",
          edge.size = 0.2,
          singletons=F,
          edge.color="grey")
   ggsave(paste0("network_",gspids[m],".png"), plot = gov_net, device = "png",
          path = "figures", width = 4020, height = 1890, dpi = 300,
          units = "px", bg = "white")
   
   library(igraph)
   library(intergraph)
   #igr <- graph_from_adjacency_matrix(adj_mat_subset, mode = "undirected", 
   #                                   weighted = TRUE, diag = FALSE)
   
   igr <- asIgraph(agency_net)
   if(!file.exists(paste0("data_output/igraph_",gspids[m]))){
      saveRDS(igr, paste0("data_output/igraph_",gspids[m]))
   }
   
   #V(igr)$realname <- V(igr)$name
   V(igr)$realname <- V(igr)$vertex.names
   
   #V(igr)$name <- 1:length(V(igr)$name)
   V(igr)$vertex.names <- 1:length(V(igr)$vertex.names)
      
   lc <- cluster_louvain(igr)
   num_communities <- length(unique(lc$membership))
   if(!file.exists(paste0("data_temp/num_comm_",gspids[m]))){
      saveRDS(num_communities, paste0("data_temp/num_comm_",gspids[m]))
   }
      
   V(igr)$bigname <- ifelse(igraph::degree(igr)>50,V(igr)$realname,"")
   
   png(paste0("figures/igraph_",gspids[m],".png"), 4020, 1890)
   plot(igr, vertex.size = 3, vertex.label = V(igr)$bigname,
        vertex.color=rainbow(num_communities, alpha=0.3)[lc$membership])
   dev.off()
   print(m)
   
   #take a couple steps to remove noise in orgs
   #dig into the probability under each org (keep orgs with > 0.75 prob)
   
   
   #goal of four-ish layers with different types of connections, then count within each category (could use verbnet here)
   #don't worry about tense
   #look at how the cues we know (like "might") is classified in spacyr parse
   
   
   
   
   #part 6: build network based on identified events
   #part 7: Use SNA to identify:
   #                 central actors
   
   
   
   
   #part 8: make plot of how these factors differ for GSPs
   #part 9: make a regression about how each of those four 
   #        factors is related to topic prevalence
   #part 10: make a regression about how each of those four
   #         factors is related to topic correlation network connectivity/centralization
}

spacy_finalize()

network_properties_list <- list.files(path = "data_temp", pattern = "network_properties", full.names = T)
centrality_list <- list.files(path = "data_temp", pattern = "centrality", full.names = T)

network_properties_df <-  data.frame(matrix(NA, nrow = length(gspids), ncol=4))
colnames(network_properties_df) <- c("gsp_id","connectedness", "centralization",
                                     "num_communities")
networks <-  vector(mode = "list", length = length(gspids))


for(i in 1:length(gspids)){
   network_properties_i <- readRDS(network_properties_list[i])
   num_comm_i <- readRDS(paste0("data_temp/num_comm_",gspids[i]))
   network_properties_df[i,"gsp_id"] <- gspids[i]
   network_properties_df[i,"connectedness"] <- unname(
      network_properties_i["connectedness"])
   network_properties_df[i,"centralization"] <- unname(
      network_properties_i["centralization"])
   network_properties_df[i,"num_communities"] <- num_comm_i
   networks[[i]] <- list("gspid" = gspids[i],
                        "network_properties" = network_properties_i, 
                         "centrality" = readRDS(centrality_list[i])
                         )
}
saveRDS(network_properties_df, paste0("data_temp/all_network_properties"))
#extremes of connection and centralization
min_connect <- network_properties_df %>% 
   filter(connectedness == min(connectedness))
max_connect <- network_properties_df %>% 
   filter(connectedness == max(connectedness))
min_centr <- network_properties_df %>% 
   filter(centralization == min(centralization))
max_centr <- network_properties_df %>% 
   filter(centralization == max(centralization))
min_comm <- network_properties_df %>% 
   filter(num_communities== min(num_communities))
max_comm <- network_properties_df %>% 
   filter(num_communities== max(num_communities))



