# is_future #confidence, planning

library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$source_entity_type!="GPE" | 
                                      edgelist_w_meta$target_entity_type!="GPE",]


edgelist_w_meta$is_transf <- 
   edgelist_w_meta$type_26 == T |
   edgelist_w_meta$type_33 == T |
   edgelist_w_meta$type_34 == T |
   edgelist_w_meta$type_35 == T |
   edgelist_w_meta$type_45 == T |
   edgelist_w_meta$type_54 == T |
   edgelist_w_meta$type_87 == T 

#only keep the six main verb tenses
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$head_verb_tense %in% c(
   "VB","VBD","VBG","VBN","VBP","VBZ"),]

edgelist_w_meta$head_verb_tense <- as.factor(edgelist_w_meta$head_verb_tense)
#only keep verbs tagged in verbnet
edgelist_w_meta <- edgelist_w_meta[unlist(lapply(seq_along(edgelist_w_meta$type_id), function(k) !is.null(edgelist_w_meta$type_id[[k]]))),]

#Verb Tense ####
#which actions have already been done and which are yet to be done?
#eg do we see transformation verbs (eg signalling landscape transformation and gov structural modifications) happening in the past, present, or future most often?
summary(glm(is_transf ~ head_verb_tense + has_hedge + is_future, data = edgelist_w_meta, family = "binomial"))
#compared to the base verb form, past tense and past participle are
#more likely to be transformational verbs, which is interesting considering
#some of these categories presumably apply to monitoring efforts and 
#ongoing actions needed to bring water goals into being by 2040.
#singular present verbs are less likely to be transformation verbs,
#especially the non-third person one. (Does this mean the actions are being
#outsourced to third parties more, compared to the collaboration results
#where the third-person present was less common, signalling less collaboration where a
#third party is the main actor?)
#hedging increases the probability of one of these verbs, indicating
#that the actual plan actions are more likely to be uncertain.
#future tense indicators also increase the probability of these actions.
#in contrast to the collaboration set, the base verb is one of the
#least common verb tenses for the transformation collection.

#Is hedging of transformation related to approval?
summary(glm(has_hedge ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
#if a sentence is from a plan under review,
#it is less likely to be hedged, compared to an approved plan

#in other words, under-review plans have more decisive language. 

summary(glm(is_transf ~ as.factor(approval), data = edgelist_w_meta, family = "binomial"))
