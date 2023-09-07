library(data.table)
library(stringr)
library(dplyr)
#important! If you want to do any additional filtering of the dataset, please 
#DO NOT edit the cleaned_extracts file. 
#Instead, save the filtered dataset in a different folder
file_list <- list.files(path = "data/cleaned_extracts", full.names = T)
gspids <- unname(sapply(file_list, function(k) gsub("\\D", "", k)))

verblist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      verblist[[m]] <- readRDS(paste0("data/cleaned_extracts/",gspids[m],".RDS"))$verblist
   }
}
vlist <- rbindlist(verblist)

vlist <- vlist[!duplicated(vlist$head_verb_lemma)]
vlist <- vlist[!str_detect(vlist$head_verb_lemma,"[0-9]")]

edgelist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      edgelist[[m]] <- readRDS(paste0("data/cleaned_extracts/",gspids[m],".RDS"))$edgelist
   }
}

nodelist <- vector(mode="list",length=length(file_list))
for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      nodelist[[m]] <- readRDS(paste0("data/cleaned_extracts/",gspids[m],".RDS"))$nodelist
   }
}

for(m in 1:length(file_list)){
   if(!gspids[m] %in% c("0053","0089")){
      edgelist[[m]] <- left_join(edgelist[[m]],nodelist[[m]][,c("entity_cat","entity_type")], by= c("source"="entity_cat"))
      colnames(edgelist[[m]])[which(colnames(edgelist[[m]])=="entity_type")] <- "source_entity_type"
      edgelist[[m]] <- left_join(edgelist[[m]],nodelist[[m]][,c("entity_cat","entity_type")], by= c("target"="entity_cat"))
      colnames(edgelist[[m]])[which(colnames(edgelist[[m]])=="entity_type")] <- "target_entity_type"
   }
}


elist <- rbindlist(edgelist)
evlist <- left_join(elist, vlist)

#dummy out verbnet types
all_type_ids <- as.character(sort(unique(as.numeric(unlist(evlist$type_id)))))
type_dummies <- setNames(data.table(matrix(nrow = nrow(evlist), ncol = length(all_type_ids))), paste0("type_",all_type_ids))

for(i in 1:nrow(evlist)){
   type_dummies[i,names(type_dummies):= lapply(all_type_ids, 
                              function(j) j %in% evlist$type_id[[i]])]
}

saveRDS(type_dummies, "data/temp_large_files/verb_type_dummies_for_edgelist")
edgelist_with_vclass <- cbind(evlist, type_dummies)

edgelist_with_vclass$gsp_id <- unlist(lapply(edgelist_with_vclass$doc_sent_verb, 
                                      function(j) gsub(".*?([0-9]+).*", "\\1", j)))
saveRDS(edgelist_with_vclass, "data/output_large_files/edgelist_with_verb_class")

#join on gsp_mini for approval, etc

#vars: source_entity_type, target_entity_type
#approval
#drinking water and ag variables
gsp_meta <- readRDS("data/output_large_files/gsp_docs_lean")
gsp_mini <- unique(gsp_meta[,c("gsp_id","approval","mult_gsas",
                               "fract_of_area_in_habitat_log_scaled",
                               "urbangw_af_log_scaled",
                               "gwsum",
                               "percent_dac_by_pop_scaled",
                               "Republican_Vote_Share_scaled",
                               "Agr_Share_Of_GDP_scaled",
                               "Perc_Bach_Degree_Over25_scaled",
                               "local_govs_per_10k_people_log_scaled",
                               "maxdryspell_scaled")])
gsp_mini <- gsp_mini[!gsp_mini$gsp_id %in% c("0089","0053"),]
#for meta
#network_properties <- as_tibble(evlist)
edgelist_w_meta <- merge(gsp_mini, edgelist_with_vclass)
saveRDS(edgelist_w_meta, "data/output_large_files/edgelist_with_verb_meta")

#Section 1: Verb Classes and Basin Attributes ####
#VERB CLASSES AND AG
#summary(glm(type_64 ~ Agr_Share_Of_GDP_scaled + Republican_Vote_Share_scaled, data = edgelist_w_meta, family = "binomial"))
#class 64 ~ "allow_verbs"
#should remove word "include" and "forbid" type words

#VERB CLASSES AND DW
summary(glm(type_76 ~ Agr_Share_Of_GDP_scaled + urbangw_af_log_scaled + fract_of_area_in_habitat_log_scaled +
               maxdryspell_scaled + gwsum, data = edgelist_w_meta, family = "binomial"))
#class 76 limit_verbs #water restraint to protect drinking water
#hypoth confirmed for ag and gwsum, not for urban, not for habitat, not for maxdryspell
summary(glm(type_90 ~ Agr_Share_Of_GDP_scaled + urbangw_af_log_scaled + fract_of_area_in_habitat_log_scaled +
               maxdryspell_scaled + gwsum, data = edgelist_w_meta, family = "binomial"))
#class 90 exceed_verbs
#hypoth confirmed for maxdryspell, not for gwsum, habitat, urban, or ag

#VERB CLASSES AND GDEs
#class 26 verbs_of_creation_and_transformation (mostly "develop")
#class 76 limit_verbs #water restraint to protect drinking water
#(see above)
#class 90 exceed_verbs
#(see above)

#VERB CLASSES AND mult_gsas
#class 13 verbs_of_change_of_possession
summary(glm(type_13 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed

#class 14 learn_verbs
summary(glm(type_14 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#not confirmed

#class 16 verbs_of_concealment
summary(glm(type_16 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed

#class 22 verbs_of_combining_and_attaching
summary(glm(type_22 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#not confirmed

#class 23 verbs_of_separating_and_disassembling
summary(glm(type_23 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed, although most common word is "vary" and "divide"

#class 36 verbs_of_social_interaction, eg "communicate", "collaborate", "correspond", "cooperate"
summary(glm(type_36 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 37 verbs_of_communication
summary(glm(type_37 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 58 verbs_of_urging_and_begging
summary(glm(type_58 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed! Main verb is "require"

#class 70 rely_verbs
summary(glm(type_70 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 71 conspire_verbs (main verb is collaborate. also includes "partner" but collaborate is covered elsewhere)

#class 72 help_verbs
summary(glm(type_72 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 73 cooperate_verbs
summary(glm(type_73 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))

#class 107 involve_verbs #main verb is include
summary(glm(type_107 ~ mult_gsas, data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#VERB CLASSES AND approval
#has_hedge and is_future #confidence, planning
#class 61 ~ "try_verbs"
summary(glm(type_61 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#unapproved plans "try"/"attempt" more!

#class 14 learn_verbs #learning
summary(glm(type_14 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#no clear trend

#class 26 verbs_of_creation_and_transformation #landscape transformation
summary(glm(type_26 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#no clear trend

#class 30 verbs_of_perception #psych elements (main verb is meet. would need to remove "meet")

#class 31 psych-verbs-verbs_of_psychological_state #psych elements (main verbs are support, approve, affect, recharge)
summary(glm(type_31 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#approved plans have this most. maybe because of discussion of "recharge"?

#class 33 judgment_verbs #evaluation (main verbs = report, approve, recommend)
summary(glm(type_33 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#no clear trend

#class 34 verbs_of_assessment #thoroughness (main verbs = monitor, estimate, follow)
summary(glm(type_34 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 35 verbs_of_searching #thoroughness (main verb = monitor)
summary(glm(type_35 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis not confirmed! Weird -- do rejected plans discuss monitoring more?

#class 45 verbs_of_change_of_state #transformation (main verbs = set, collect, operate, reduce)
summary(glm(type_45 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 54 measure_verbs #thoroughness (main verbs = estimate, contain, hold, serve, take, assess)
summary(glm(type_54 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#class 87 verbs_of_focusing_and_comprehending #thoroughness; main verb is "follow"
summary(glm(type_87 ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#hypothesis confirmed!

#Section 2: Verb Class and Node Attributes ####
#(such as degree, centrality, reciprocity, transitivity, and num_GSPs_in)
#eg do social verbs have more reciprocity and transitivity than other verbs?

#Section 3: Verb Class and Verb Tense/Hedges ####
#which actions have already been done and which are yet to be done?
#eg do we see evidence for social verbs/collaboration happening in the past, present, and future to equal degrees?
#eg do we see transformation verbs (eg signalling landscape transformation and gov structural modifications) happening in the past, present, or future most often?
#eg do we see hedges disproportionately attached to transformation verbs, signalling the uncertainty of the actual project actions?
#which types of verbs are the most likely to be hedged, proportionately to their total percentage of use?

#Section 4: Verb Tense and Node Attributes ####
#do the most central nodes have more past tense, present tense, or future tense edges attached to them than is typical in the network?
#this may require an ergm.

