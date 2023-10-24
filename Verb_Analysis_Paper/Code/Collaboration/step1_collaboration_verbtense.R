library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)


edgelist_w_meta$is_collab <- edgelist_w_meta$type_13 == T |
                              edgelist_w_meta$type_14 == T |
                              edgelist_w_meta$type_22 == T |
                              edgelist_w_meta$type_36 == T |
                              edgelist_w_meta$type_37 == T |
                              edgelist_w_meta$type_58 == T |
                              edgelist_w_meta$type_70 == T |
                              edgelist_w_meta$type_71 == T |
                              edgelist_w_meta$type_72 == T |
                              edgelist_w_meta$type_73 == T |
                              edgelist_w_meta$type_107 == T 

#only keep the six main verb tenses
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$head_verb_tense %in% c(
   "VB","VBD","VBG","VBN","VBP","VBZ"),]

edgelist_w_meta$head_verb_tense <- as.factor(edgelist_w_meta$head_verb_tense)
#only keep verbs tagged in verbnet
edgelist_w_meta <- edgelist_w_meta[unlist(lapply(seq_along(edgelist_w_meta$type_id), function(k) !is.null(edgelist_w_meta$type_id[[k]]))),]

#Verb Tense ####
#which actions have already been done and which are yet to be done?
#eg do we see evidence for social verbs/collaboration happening in the past, present, and future to equal degrees?
summary(glm(is_collab ~ head_verb_tense + has_hedge + is_future, data = edgelist_w_meta, family = "binomial"))

#compared to the base form, past tense and past participle verbs are
#less likely to be collaboration verbs, 
#gerund or present participle verbs are somewhat less likely to be
#collaboration verbs, and singular present verbs are
#less likely to be less collaboration verbs
#hedged verbs are less likely to be collaboration verbs
#future verbs are somewhat more likely to be collaboration verbs

#In other words, future tense makes it more likely to be a collaboration verb
#and past tense or singular present makes it less likely to be collaboration.
#collaboration actions are skewed toward the future. Hedging makes it
#less likely to be a collaboration verb so collaboration verbs are
#relatively certain compared to other verbs

#TODO
#Verb Tense and Node Attributes ####
#do the most central nodes have more past tense, present tense, or future tense edges attached to them than is typical in the network?
#this may require an ergm.

