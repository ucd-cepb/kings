library(data.table)
library(stringr)
library(dplyr)
filekey <- read.csv("filekey.csv")

edgelist_w_meta <- readRDS(filekey[filekey$var_name=="edgelist_w_meta",]$filepath)
#need at least one of the nodes in each edge to be not GPE
edgelist_w_meta <- edgelist_w_meta[edgelist_w_meta$source_entity_type!="GPE" | 
                                      edgelist_w_meta$target_entity_type!="GPE",]

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


edgelist_w_meta$is_transf <- 
   edgelist_w_meta$type_26 == T |
   edgelist_w_meta$type_33 == T |
   edgelist_w_meta$type_34 == T |
   edgelist_w_meta$type_35 == T |
   edgelist_w_meta$type_45 == T |
   edgelist_w_meta$type_54 == T |
   edgelist_w_meta$type_87 == T 
portiontransf <- sum(edgelist_w_meta$is_transf)/length(edgelist_w_meta$is_collab)
portionnotcollab <- sum(!edgelist_w_meta$is_collab)/length(edgelist_w_meta$is_collab)

portiontransf / portionnotcollab
byplan <- edgelist_w_meta %>% group_by(gsp_id) %>% summarize(pct_collab = mean(is_collab), is_transf = mean(is_transf))
cor(byplan$pct_collab, byplan$is_transf, method = "pearson")

#H1 confirmed?
alpha <- cor(byplan$pct_collab, byplan$is_transf, method = "pearson") #> -1*portiontransf / portionnotcollab

N <- 1000
bootstrap <- vector(mode = "list", length = N)
corrs <- vector(mode = "list", length = N)
for(i in 1:N){
   bootstrap[[i]] <- sample(byplan$is_transf, length(byplan$is_transf), replace = F)
   corrs[[i]] <- cor(byplan$pct_collab, bootstrap[[i]], method = "pearson")
}


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
#future verbs are neither more nor less likely to be collaboration verbs

#In other words, past tense or singular present makes it less likely to be collaboration.
#Hedging makes it
#less likely to be a collaboration verb so collaboration verbs are
#relatively certain compared to other verbs

#TODO
#Verb Tense and Node Attributes ####
#do the most central nodes have more past tense, present tense, or future tense edges attached to them than is typical in the network?
#this may require an ergm.

library(ggplot2)

edgelist_w_meta <- edgelist_w_meta %>% mutate(verbtype = ifelse(is_collab==T, ifelse(is_transf==F,"Teamwork","both"), 
                                                                ifelse(is_transf==T, "Tasks","neither")))
edgelist_w_meta$head_verb_tense <- as.character(edgelist_w_meta$head_verb_tense)
edgelist_w_meta <- edgelist_w_meta %>% mutate(verb_tense = ifelse(is_future==T,"Future",head_verb_tense))
edgelist_w_meta$verb_tense <- factor(edgelist_w_meta$verb_tense,
                                          levels = c("VBN","VBD","VBG","VBZ","VBP","VB","Future"))
edgelist_w_meta$verbtype <- factor(edgelist_w_meta$verbtype,
                                      levels = c("Tasks","both","neither","Teamwork"))
teamsandtasks <- edgelist_w_meta[!edgelist_w_meta$verbtype %in% c("both"),]
ggplot(teamsandtasks, aes(x = verbtype, fill = verb_tense)) +
   geom_bar(position="fill")+
   ylab("proportion")+
   scale_fill_viridis_d(option = "C")

ggplot(teamsandtasks, aes(x = verbtype, fill = has_hedge)) +
   geom_bar(position="fill")+
   ylab("proportion")+
   scale_fill_viridis_d(option = "D")

ggplot(teamsandtasks, aes(x = approval, fill = verbtype)) +
   geom_bar(position="fill")+
   ylab("proportion")+
   scale_fill_viridis_d(option = "E")+
   scale_x_discrete(labels=c('Approved', 'Rejected', 'Review in Progress'))
